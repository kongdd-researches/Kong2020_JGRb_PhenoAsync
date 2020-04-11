source("test/main_pkgs.R")

varname = "GPP_NT"
version = glue("({varname}) v0.2.6.9000") # test version

file_pheno_full = glue("INPUT/pheno_flux166_full {version}.rda")

# -------------------------------------------------------------------------
df <- fread("E:/Research/phenology/rfluxnet/OUTPUT/fluxsites166_FULLSET_daily_v20200411 (80%).csv") %>%
    plyr::mutate(LE = w2mm(LE, TA),
                 LE_CORR = w2mm(LE_CORR, TA),
                 # H = w2mm(H, TA),
                 # H_CORR = w2mm(H_CORR, TA),
                 H_CORR %<>% W2_MJ(),
                 Rn = w2mm(H_CORR, TA)
    )

vars <- colnames(df) %>% .[grep("QC_", .)] %>% gsub("QC_", "", .)
d_mean <- df[, map(.SD, ~mean(., na.rm = TRUE)*365), .(site, year), .SDcols = vars] %>% cbind(I = 1:nrow(.), .)
d_days <- df[, lapply(.SD, . %>% {sum(!is.na(.))}), .(site, year), .SDcols = vars]

## 加载物候数据 ----------------------------------------------------------------
load(file_pheno_full)
metrics_all <- colnames(df_gpp)[-(1:4)]
# 挑选只有一个生长季的站点
{
    d <- df_gpp[substr(flag, 6, 6) == 2, .N, .(site, origin)]
    d_multi <- df_gpp[, 1:4] %>% cbind(I = 1:nrow(.), .) %>% merge(d)

    d_gpp <- df_gpp[-d_multi$I, ] %>% .[meth %in% c("Beck", "Elmore")] %>%
        .[, map(.SD, mean, na.rm = TRUE), .(site, origin), .SDcols = metrics_all]
    # 至少5年以上观测的站点
    sites <- d_gpp[, .N, .(site)][N >= 5, site]
    d_gpp <- d_gpp[site %in% sites] %>%
        plyr::mutate(year = year(origin), TRS1.los = TRS1.eos - TRS1.sos)
    # d_gpp.anorm <- d_gpp[, map(.SD, ~scale(., scale = FALSE)[,1]), .(site), .SDcols = metrics_all] %>%
    #     cbind(d_gpp[, .(year)]) %>% reorder_name(c("site", "year"))
    mete_vars <- c("H", "LE", "H_CORR", "LE_CORR", "G", "NEE", "GPP_DT", "RECO_DT", "GPP_NT", "RECO_NT", "Rn")
    d_mean2 <- d_mean[site %in% sites, .SD, .SDcols = c("site", "year", "I", mete_vars)]

    st <- st_212[site %in% sites, .(site, LC)]
    temp <- merge(d_gpp, d_mean2)

    data2 <- df_Terra[site %in% sites & meth %in% c("Elmore", "Beck")] %>%
        .[, map(.SD, mean, na.rm = TRUE), .(type_VI, site, origin), .SDcols = metrics_all] %>%
        plyr::mutate(TRS1.los = TRS1.eos - TRS1.sos) %>%
        merge(temp[, c(1, 3, 29:39)], by = c("site", "origin")) %>%
        .[, map(.SD, ~ scale(., scale = FALSE)[, 1]), .(type_VI, site), .SDcols = c(metrics_all, "TRS1.los", mete_vars)] %>%
        merge(st, ., by = "site")
    data <- temp[, map(.SD, ~ scale(., scale = FALSE)[, 1]), .(site), .SDcols = c(metrics_all, "TRS1.los", mete_vars)] %>%
        merge(st, ., by = "site")
    # merge(d_days[, .SD, .SDcols = c("site", "year", mete_vars)], d_mean2[, .(site, year)])
    # d_mean[, ]1
}

# H, LE, Rn
lc_names <- c("Cropland", "Grassland", "Shrubland", "Forest", "ENF")
data2$LC %<>% factor(lc_names)
lc_N <- data2[, .(N = .N / 2), .(LC)][order(LC)]
lc_newname <- sprintf("%s (N=%d)", c("耕地", "草地", "灌木", "森林", "针叶林"), lc_N$N)

devices = c("pdf", "jpg")[2]
{
    ## 1. NDVI -----------------------------------------------------------------
    d1 <- data2[type_VI == "NDVI", .(site, LC, TRS1.sos, TRS1.eos, TRS1.los, NEE, GPP_NT, LE_CORR, H_CORR)] %>% #GPP_NT, GPP_DT,
        melt(c("site", "LC", "TRS1.sos", "TRS1.eos", "TRS1.los")) %>%
        melt(c("site", "LC", "variable", "value"), variable.name = "metric", value.name = "pheno")
    d1$LC %<>% mapvalues(lc_names, lc_newname)
    outfile <- glue("Figure7-10 phenology impact on ET and GPP NDVI greenness pheno.pdf")
    r2 <- pheno_impact(d1, outfile, c("pdf", "jpg"))

    ## 2. EVI ------------------------------------------------------------------
    d1 <- data2[type_VI == "EVI", .(site, LC, TRS1.sos, TRS1.eos, TRS1.los, NEE, GPP_NT, LE_CORR, H_CORR)] %>% #GPP_NT, GPP_DT,
        melt(c("site", "LC", "TRS1.sos", "TRS1.eos", "TRS1.los")) %>%
        melt(c("site", "LC", "variable", "value"), variable.name = "metric", value.name = "pheno")
    d1$LC %<>% mapvalues(lc_names, lc_newname)
    outfile <- glue("Figure7-10 phenology impact on ET and GPP EVI greenness pheno.pdf")
    r2 <- pheno_impact(d1, outfile, c("pdf", "jpg"))
}

# 核对GPP, ET的单位
# st_212[site %in% sites, LC] %>% table()
# sites <- d_gpp$site %>% unique()
# {
#     load(file_pheno_full)
#     st = st_166[site %in% sites, .(site, lat, south = lat < 0)]
#     df_combined = df_combined[site %in% sites & origin >= "2003-01-01", ]
#     df_Aqua     = df_Aqua[site %in% sites & origin >= "2003-01-01", ]
#     df_Terra    = df_Terra[site %in% sites & origin >= "2000-01-01", ]
#
#     df_VI = list(combined = df_combined, df_Aqua = df_Aqua, df_Terra = df_Terra) %>%
#         melt_list("sate")
#     df_VI_prim  <- filter_primary(df_VI)
#     df_gpp_prim <- filter_primary(df_gpp)
#
#     df_EVI_pc_prim <- filter_primary(df_EVI_pc)
#     df_EVI_pc_prim %<>% cbind(sate = "Terra", type_VI = "EVI_pc", .)
#     df_VI_prim %<>% rbind(df_EVI_pc_prim)
#     # save(df_VI_prim, df_gpp_prim, sites, st, file = file_pheno_prim)
# }
