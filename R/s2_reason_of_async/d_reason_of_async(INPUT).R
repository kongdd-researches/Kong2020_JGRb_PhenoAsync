source('inst/shiny/check_season/global.R')
source("test/stable/load_pkgs.R")
source('test/phenology_async/R/main_async.R')

load("data/phenoflux_115_gs.rda") # st
# kongdd/plyr (v1.8.4.9001)

aggregate_dn <- function(data, nday = 16){
    nptperyear <- ceiling(365/nday)
    byname     <- c("site", "year", paste0("d", nday))

    res <- data[, lapply(.SD, mean, na.rm = T), byname, .SDcols = vars]
    colnames(res)[3] <- "dn"

    res <- merge(st[, .(site, IGBP, lat)], res, by = "site") %>%
        .[, `:=`(date = as.Date(sprintf("%d%03d", year, (dn-1)*nday+1), "%Y%j"),
                 ydn  = (year - 2000)*nptperyear + dn)] %>% #dn ID order
        ddply(.(site), addPredictor_tn)  %>%
        reorder_name(c("site", "IGBP", "date", "year", "year2", "d16", "d8", "ydn"))
    res[, year2 := as.integer(year + ((month(date)>=7)-1)*(lat<0))]
    res
}

#' cal_LSWImax
#' works for site
cal_LSWImax <- function(x){
    site = x$site[1]

    tryCatch({
        x <- x[date >= sos_date & date <= eos_date]
        res <- x[QC_flag == "good", .(LSWI_max = max(LSWI)), .(site, year2)]

        res[, LSWI_max := zoo::rollapply(LSWI_max, 5, nth_max, partial = T),
             .(site)] # 5 year second maximum moving
        return(res)
    }, error = function(e){
        message(sprintf("[%s] %s", site, e$message))
    })
}

############################### AGGREGATE GPP_EC ###############################
st[, `:=`(IGBPname = IGBP, lon = long)]

data <- df[, .(site, date, Rn, Rs = SW_IN, VPD, Prcp = P, T = TA, GPP_DT, GPP_NT)] %>%
    add_dn(days = c(8, 16))
data$GPP <- rowMeans(as.matrix(data[, .(GPP_DT, GPP_NT)]), na.rm = T)

vars <- c("GPP_NT", "GPP")
data[, (vars) := lapply(.SD, clamp_min), .SDcols = vars] # clamp_min to 0

## 1. merge EVI and GPP
vars_com <- c("site", "date", "year", "doy", "d16", "d8")
vars <- setdiff(colnames(data), vars_com)

data_d8  <- aggregate_dn(data, nday = 8)
data_d16 <- aggregate_dn(data, nday = 16)

############################ LOAD SATELLITE DATA ###############################
## 2.1 MOD13A1
d_mod13a1 <- lst_sm$MOD13A1[scale == "0m", .(site, date, NDVI, EVI, SummaryQA)]
d_mod13a1 <- merge(data_d16, d_mod13a1, by = c("site", "date")) # no negative after fix_neg

d_mod13a1[, APAR := Rs*0.45*1.25*(EVI - 0.1)] # Zhang Yao, 2017, sci data

## 2.2 MOD09A1
## 2.2.1 tidy MOD09A1
d_mod09a1 <- readRDS("data_test/flux212_MOD09A1_VI.RDS")
d_mod09a1 <- d_mod09a1[scale == "0m", .(site, t, date, year, EVI, EVI2, NDVI, LSWI, w, StateQA, QC_flag)]

d_mod09a1[QC_flag %in% c("cloud", "snow"), EVI := EVI2] # fix bright EVI
# (a) make sure values in a reasonable range
d_mod09a1[ EVI > 1 | EVI < -0.1, EVI := NA]
# (b) remove outliers: abs(y - mean) > 3sd
d_mod09a1[!is.na(EVI), `:=`(mean = mean(EVI), sd = sd(EVI)), .(site)]
d_mod09a1[abs(EVI - mean) >= 3*sd & QC_flag != "good", EVI := NA_real_, .(site)]
d_mod09a1[, c("mean", "sd") := NULL]

d_mod09a1 <- merge(d_mod09a1, st[, .(site, lat)])
d_mod09a1[, year2 := year + ((month(date)>=7)-1)*(lat<0)]

# with(, plot(EVI2, y)); grid(); abline(a = 0, b=1, col="red")

## Wscaler
pheno_T <- readRDS('data_test/flux212_MYD11A2_T_phenology_5d_10d.RDS')

df1 <- merge(d_mod09a1[, .(site, date, year2, LSWI, QC_flag)],
    pheno_T[, .(site, year2, sos_date, eos_date)], by = c("site", "year2"))
d <- df1[site == sitename]

d_LSWImax <- ddply(df1, .(site), cal_LSWImax)
d_LSWImax[LSWI_max < 0.1, LSWI_max := 0.1]

d_mod09a1 <- merge(d_mod09a1, d_LSWImax, by = c("site", "year2"))
d_mod09a1[, Wscalar := (1 + LSWI) / (1 + LSWI_max)]

## Tscalar
d_mod09a1 <- merge(data_d8, d_mod09a1, by = c("site", "date", "year", "year2","lat"))
d_mod09a1[, Tscalar := cal_Tscalar(T, IGBP), .(IGBP)]

# make sure Tscalar and Wscaler in the range of 0-1
vars_scalar <- c('Wscalar', 'Tscalar')
d_mod09a1[, (vars_scalar) := lapply(.SD, clamp), .SDcols = vars_scalar]

# add APAR
d_mod09a1[, APAR := Rs*0.45*1.25*(EVI - 0.1)] # Zhang Yao, 2017, sci data

save(d_mod13a1, d_mod09a1, st, file = "data_test/flux115_async_input.rda")
