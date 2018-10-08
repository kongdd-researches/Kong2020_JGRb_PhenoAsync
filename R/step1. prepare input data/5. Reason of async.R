source('test/phenology_async/R/step1. prepare input data/5.0 Reason of async (INPUT).R')

## 1.2 rm bad points

d <- d[SummaryQA <= 0 & EVI > 0]
info <- d[, .N, .(d16, IGBP)][order(N)]
d    <- merge(d, info[N >= 10])

## 2. Perform regression
# varnames <- c("Rn", "VPD", "Prcp", "T", "EVI", "GPP_t1")

## 3.1 regression coefs
# FUN =  # pls_coef, lm_coef

varnames <- c("EVI", "NDVI", "Rs", "APAR", "T", "Prcp", "VPD", paste0("GPP_t", 1:3))[c(1, 3:8)]
formula  <- varnames %>% paste(collapse = "+") %>% {as.formula(paste("GPP~", .))}

d_coef_EVI <- ddply(d[!is.na(EVI), ], .(d16, IGBP), pls_coef,
                    predictors_var = varnames[2:5],
                    response_var = varnames[1]) %>%
    data.table() %>% .[n >= 10, ]
d_coef_GPP <- ddply(d, .(d16, IGBP), pls_coef) %>%
    data.table() %>% .[n >= 10, ]

select_var <- function(d_coef0, type){
    res <- dplyr::select(d_coef0, starts_with(type)) %>%
        set_names(str_extract(names(.), "(?<=\\.).*")) %>% # rm 'coef.' or 'VIP.'
        cbind(d_coef0[, .(d16, IGBP, n)]) %>%
        melt(c("d16", "IGBP", "n"))
    ncol <- ncol(res)
    colnames(res)[ncol] <- type
    res
}

d_coef0 <- d_coef_GPP
d_coef <- select_var(d_coef0, "coef")
d_vip  <- select_var(d_coef0, "VIP")

d_pls  <- merge(d_coef, d_vip)

## 3.2 scaled GPP into same range of coef, by IGBP
range_coef <- getRange(d_pls, "coef", .(IGBP))
d_gpp <- merge(d[, .(d16, IGBP, GPP, EVI)], range_coef, by = "IGBP") %>%
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

file <- "Figure2.1_PLScoefs of GPP.pdf"
# file <- "Figure2.2_PLScoefs of EVI.pdf"
CairoPDF(file, 12, 8)
print(p1)
dev.off()
# file.show(file)
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
