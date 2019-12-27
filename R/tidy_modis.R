# FOR MOD09A1
# // 620-670nm, RED, sur_refl_b01
# // 841-876nm, NIR, sur_refl_b02
# // 459-479nm, BLUE, sur_refl_b03
# // 1628-1652nm, SWIR, sur_refl_b06
# 
# examples
# df = tidy_modis_09A1(satellite = "MYD09A1")
tidy_modis_09A1 <- function(satellite = "MYD09A1") {
    infile_VI <- glue("INPUT/st212_{satellite}_9grids.RDS")

    if (!file.exists(infile_VI)) {
        # https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MOD09A1
        file = glue("INPUT/fluxnet/st212_{satellite}_0m_buffer.csv")
        df = fread(file, drop = 1) %>%
            .[, .(group, site, date = as.Date(date), DayOfYear,
                red = sur_refl_b01/1e4, nir = sur_refl_b02/1e4, blue = sur_refl_b03/1e4, swir = sur_refl_b06/1e4,
                QC = StateQA, RelativeAzimuth, SolarZenith, ViewZenith)] %>%
            plyr::mutate(
                t    = getRealDate(date, DayOfYear),
                EVI  = RS_EVI(nir, red, blue),
                EVI2 = RS_EVI2(nir, red),
                NDVI = RS_NDVI(nir, red),
                LSWI = RS_LSWI(nir, swir)
            )
        # .[date <= "2015-12-31", .(site, group, t = date, y = Lai/10, QC = FparExtra_QC, FparLai_QC)] # , FparExtra_QC
        df[, c("QC_flag", "w") := qc_StateQA(QC)]
        df %<>% merge(st_212[, .(site, lat)]) %>% plyr::mutate(
            year  = year(date),
            year2 = year + ((month(date) >= 7) - 1)*(lat < 0)) %>%
            reorder_name(c("site", "group", "date", "t", "DayOfYear", "year", "year2"))
        df2 <- check_EVI(df)
        # d = df[site == "AR-SLu" & group == 5,]
        saveRDS(df2, infile_VI)
    } else {
        df = readRDS(infile_VI)
    }
    df
}
