source('inst/shiny/check_season/global.R')
source("test/stable/load_pkgs.R")
# source("test/phenology_async/R/step1. prepare input data/main_phenofit.R")
load("data/phenoflux_115_gs.rda")


# df <- lst_sm$MOD13A1

# df <- df[scale == "0m", .(site, t, date, y = EVI, w, SummaryQA)]
# df$SummaryQA %<>% factor(qc_values, qc_levels)

st[, `:=`(IGBPname = IGBP, lon = long)]


data <- df[, .(site, date, Rn, Rs = SW_IN, VPD, Prcp = P, T = TA, GPP_DT, GPP_NT)] %>%
    add_dn(days = 16)
data$GPP <- rowMeans(as.matrix(data[, .(GPP_DT, GPP_NT)]), na.rm = T)

vars <- c("GPP_NT", "GPP")
data[, (vars) := lapply(.SD, fix_neg), .SDcols = vars]

## 1. merge EVI and GPP
vars_com <- c("site", "date", "year", "doy", "d16")
vars <- setdiff(colnames(data), vars_com)
data_d16 <- data[, lapply(.SD, mean, na.rm = T), .(site, year, d16), .SDcols = vars]
data_d16[, date := as.Date(sprintf("%d%03d", year, (d16-1)*16+1), "%Y%j")]
data_d16 <- merge(st[, .(site, IGBP)], data_d16, by = "site")

d_mod13a1 <- lst_sm$MOD13A1[scale == "0m", .(site, date, NDVI, EVI, SummaryQA)]

d <- merge(data_d16[GPP >= 0], d_mod13a1, by = c("site", "date")) # rm NA and negative
d[, yd16 := (year - 2000)*23 + d16]
d <- ddply(d, .(site), addPredictor_tn) %>% data.table() %>%
    reorder_name(c("site", "IGBP", "date", "year", "d16", "yd16"))


## 2. Perform regression
# varnames <- c("Rn", "VPD", "Prcp", "T", "EVI", "GPP_t1")

## 3.1 regression coefs
# FUN =  # pls_coef, lm_coef

varnames <- c("EVI", "NDVI", "T", "Prcp", "Rs", "VPD", paste0("GPP_t", 1:3))[c(1, 3:7)]
formula  <- varnames %>% paste(collapse = "+") %>% {as.formula(paste("GPP~", .))}

d_coef0 <- ddply(d, .(d16, IGBP), pls_coef) %>% data.table() %>% .[n >= 10, ]

select_var <- function(d_coef0, type){
    res <- dplyr::select(d_coef0, starts_with(type)) %>% set_names(varnames) %>%
        cbind(d_coef0[, .(d16, IGBP, n)]) %>%
        melt(c("d16", "IGBP", "n"))
    ncol <- ncol(res)
    colnames(res)[ncol] <- type
    res
}
d_coef <- select_var(d_coef0, "coef")
d_vip  <- select_var(d_coef0, "VIP")

d_pls  <- merge(d_coef, d_vip)

## 3.2 scaled GPP into same range of coef, by IGBP
range_coef <- getRange(d_pls, "coef", .(IGBP))
d_gpp <- merge(d[, .(d16, IGBP, GPP, EVI)], range_coef) %>%
    ddply(.(IGBP), scaleGPP_ByCoef) %>% data.table()
# Get IGBP mean and sdn
d_gpp_IGBP <- d_gpp[, .(mean = mean(GPP_z), sd = sd(GPP_z), n = .N), .(d16, IGBP)]
d_gpp_IGBP[, `:=`(max = mean + sd, min = mean - sd)]
d_gpp_IGBP[, peak := d16[which.max(mean)], .(IGBP)]

data <- merge(d_pls, d_gpp_IGBP, by = c("IGBP", "d16")) # value was coef, others are GPP variables
data <- data[abs(coef) < 5 & variable != "GPP_t1", ]
data[, `:=`(strip = sprintf('%s-%s', IGBP, variable))]
data$variable %<>% factor(varnames)

d_peak <- data[, .(IGBP, variable, strip, peak)] %>% unique()
p1 <- ggplot(data, aes(d16, coef)) +
    geom_line(aes(y = mean), color = "green", size = 0.6) +
    geom_errorbar(aes(ymin = min, ymax = max), color = "green", alpha = 0.7, size = 0.6) +
    geom_point(aes(color = VIP >= 0.8)) +
    geom_line() +
    facet_grid(IGBP~variable, scales = "free_y") + # ncol = length(varnames)-1
    geom_hline(aes(yintercept = 0), color = "red") +
    geom_vline(data = d_peak, aes(xintercept = peak), color = "yellow")
    # scale_color_manual(values = c("grey", "black"))

file <- "Figure2_different mete variables PLS coefs acrossd 16_v4.pdf"
CairoPDF(file, 12, 8)
print(p1)
dev.off()
file.show(file)
# GPPobs figure


## Overall
a <- d[, .(GPP = mean(GPP), EVI = mean(EVI, na.rm = T)),.(d16)]
# par(mfrow = c(2, 1))
# plot(GPP~d16, a)
# plot(EVI~d16, a); GRID()

ggplot(a, aes(d16, GPP)) + geom_line(color = "green") +
    geom_line(aes(y = EVI*30 - 5), color = "blue")
# p0 <- ggplot(d_gpp, aes(d16, mean)) +
#     geom_point() + geom_line() +
#     geom_errorbar(aes(ymin = min, ymax = max)) +
#     facet_wrap(~IGBP,
#                ncol = 1, scales = "free_y", strip.position = "right")
