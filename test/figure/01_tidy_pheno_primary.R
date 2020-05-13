source("test/main_pkgs.R")

## update 20200322 -------------------------------------------------------------
varname = "GPP_NT"
# varname = "GPP_DT"
version = glue("({varname}) v0.2.6.9000") # test version

file_pheno_full = glue("INPUT/pheno_flux166_full {version}.rda")
file_pheno_prim = glue("INPUT/pheno_flux95_prim ({version}).rda")
file_brks = glue("INPUT/pheno_gpp_st109 {version}.rda")

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
    load(file_brks) # "INPUT/pheno_gpp_st109.rda"
    sites = names(lst_pheno)[1:95]
    df_gpp = map(lst_pheno[sites], "doy") %>% melt_tree(c("site", "meth"))

    save(df_combined, df_Aqua, df_Terra, df_gpp, sites, file = file_pheno_full)
} else {
    load(file_pheno_full)
}


if (FALSE) {
    var = "NDVI"
    load(glue("./INPUT/pheno_MOD09A1_{var}_PC_st166.rda"))
    file_pc = glue("INPUT/pheno_MOD09A1_{var}_PC_tidy.RDS")

    names(lst_NDVI_pc) <- st_166$site
    df_pc = melt_pheno(lst_NDVI_pc)
    saveRDS(df_pc, file = file_pc)
}

var = "EVI"
df_pc  <- readRDS(glue("INPUT/pheno_MOD09A1_{var}_PC_tidy.RDS"))
df_EVI_pc_prim <- filter_primary(df_pc)
df_EVI_pc_prim %<>% cbind(sate = "Terra", type_VI = glue("{var}_pc"), .)
var = "NDVI"
df_pc  <- readRDS(glue("INPUT/pheno_MOD09A1_{var}_PC_tidy.RDS"))
df_NDVI_pc_prim <- filter_primary(df_pc)
df_NDVI_pc_prim %<>% cbind(sate = "Terra", type_VI = glue("{var}_pc"), .)

# filter for Aqua
{
    load(file_pheno_full)
    st = st_166[site %in% sites, .(site, lat, south = lat < 0)]
    df_combined = df_combined[site %in% sites & origin >= "2003-01-01", ]
    df_Aqua     = df_Aqua[site %in% sites & origin >= "2003-01-01", ]
    df_Terra    = df_Terra[site %in% sites & origin >= "2000-01-01", ]

    df_VI = list(combined = df_combined, df_Aqua = df_Aqua, df_Terra = df_Terra) %>%
        melt_list("sate")
    df_VI_prim  <- filter_primary(df_VI)
    df_gpp_prim <- filter_primary(df_gpp)

    df_VI_prim %<>% rbind(df_EVI_pc_prim, df_NDVI_pc_prim)

    save(df_VI_prim, df_gpp_prim, sites, st, file = file_pheno_prim)
}
