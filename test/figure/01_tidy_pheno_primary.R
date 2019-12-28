source("test/main_pkgs.R")

file_pheno_full = "INPUT/pheno_flux166_full.rda"
if (!file.exists(file_pheno_full)) {
    # 1. combined
    load("./INPUT/pheno_MCD09A1_EVI_st166.rda")
    load("./INPUT/pheno_MCD09A1_NDVI_st166.rda")
    load("./INPUT/pheno_MCD15A3H_LAI_st166.rda")

    lst_VI = list(EVI = lst_EVI, NDVI = lst_NDVI, LAI = lst_LAI)
    df_combined = map(lst_VI, melt_pheno) %>% Ipaper::melt_list("type_VI")
    # Aqua >= 2003

    # 2. Terra
    load("./INPUT/pheno_MOD09A1_EVI_st166.rda")
    load("./INPUT/pheno_MOD09A1_NDVI_st166.rda")

    lst_VI = list(EVI = lst_EVI, NDVI = lst_NDVI)
    df_Terra = map(lst_VI, melt_pheno) %>% Ipaper::melt_list("type_VI")

    # 3. Aqua
    load("./INPUT/pheno_MYD09A1_EVI_st166.rda")
    load("./INPUT/pheno_MYD09A1_NDVI_st166.rda")

    lst_VI = list(EVI = lst_EVI, NDVI = lst_NDVI)
    df_Aqua = map(lst_VI, melt_pheno) %>% Ipaper::melt_list("type_VI")

    # 0. GPPobs from flux site
    load("INPUT/pheno_gpp_st109.rda")
    sites = names(lst_pheno)[1:95]
    df_gpp = map(lst_pheno[sites], "doy") %>% melt_tree(c("site", "meth"))

    save(df_combined, df_Aqua, df_Terra, df_gpp, sites, file = file_pheno_full)
} else {
    load(file_pheno_full)
}

# filter for Aqua
{
    load(file_pheno_full)
    st = st_166[site %in% sites, .(site, lat, south = lat < 0)]
    df_combined = df_combined[site %in% sites & origin >= "2003-01-01", ]
    df_Aqua     = df_Aqua[site %in% sites & origin >= "2003-01-01", ]
    df_Terra    = df_Terra[site %in% sites & origin >= "2000-01-01", ]
}

{
    df_VI = list(combined = df_combined, df_Aqua = df_Aqua, df_Terra = df_Terra) %>%
        melt_list("sate")
    df_VI_prim  <- filter_primary(df_VI)
    df_gpp_prim <- filter_primary(df_gpp)

    save(df_VI_prim, df_gpp_prim, sites, st, file = file_pheno_prim)
}
