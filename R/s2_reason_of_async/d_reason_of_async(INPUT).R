source('inst/shiny/check_season/global.R')
source("test/stable/load_pkgs.R")
source('test/phenology_async/R/main_async.R')

load("data/phenoflux_115_gs.rda")


# df <- lst_sm$MOD13A1

# df <- df[scale == "0m", .(site, t, date, y = EVI, w, SummaryQA)]
# df$SummaryQA %<>% factor(qc_values, qc_levels)

st[, `:=`(IGBPname = IGBP, lon = long)]

data <- df[, .(site, date, Rn, Rs = SW_IN, VPD, Prcp = P, T = TA, GPP_DT, GPP_NT)] %>%
    add_dn(days = c(8, 16))
data$GPP <- rowMeans(as.matrix(data[, .(GPP_DT, GPP_NT)]), na.rm = T)

vars <- c("GPP_NT", "GPP")
data[, (vars) := lapply(.SD, fix_neg), .SDcols = vars]

## 1. merge EVI and GPP
vars_com <- c("site", "date", "year", "doy", "d16", "d8")
vars <- setdiff(colnames(data), vars_com)
data_d16 <- data[, lapply(.SD, mean, na.rm = T), .(site, year, d16), .SDcols = vars]
data_d16[, date := as.Date(sprintf("%d%03d", year, (d16-1)*16+1), "%Y%j")]
data_d16 <- merge(st[, .(site, IGBP)], data_d16, by = "site")

data_d16 <- data[, lapply(.SD, mean, na.rm = T),
                .(site, year, d16), .SDcols = vars] %>%
    merge(st[, .(site, IGBP)], ., by = "site") %>%
    .[, `:=`(date = as.Date(sprintf("%d%03d", year, (d16-1)*16+1), "%Y%j"),
             ydn  = (year - 2000)*23 + d16)] #d8, d16 ID order


data_d8 <- data[, lapply(.SD, mean, na.rm = T),
                .(site, year, d8), .SDcols = vars] %>%
    merge(st[, .(site, IGBP)], ., by = "site") %>%
    .[, `:=`(date = as.Date(sprintf("%d%03d", year, (d8-1)*8+1), "%Y%j"),
             ydn  = (year - 2000)*46 + d8)]



d_mod13a1 <- lst_sm$MOD13A1[scale == "0m", .(site, date, NDVI, EVI, SummaryQA)]
d_mod13a1 <- merge(data_d16, d_mod13a1, by = c("site", "date")) # no negative after fix_neg

d <- ddply(d, .(site), addPredictor_tn) %>% data.table() %>%
    reorder_name(c("site", "IGBP", "date", "year", "d16", "yd16"))
d[, APAR := Rs*0.45*(EVI - 0.1)] # Zhang Yao, 2017, sci data
