# PPFD_IN: Photosynthetic photon flux density, incoming, n2q    qq  q1gb
# PAR    : photosynthetically active radiation

# source('inst/shiny/check_season/global.R')
source("test/main_pkgs.R")

# load("data/phenoflux_115_gs.rda") # st
file_MOD09A1 = "INPUT/fluxnet115/flux115_async_input.rda"
load(file_MOD09A1)

# test/async/s2_MOD09A1_EVI.R
df_mod09a1 <- readRDS("INPUT/st212_MOD09A1_9grids.RDS")

load("INPUT/pheno_MOD09A1_EVI_st166.rda")
lst_evi <- map_depth(lst_EVI, 2, ~get_fitting2(.x)) #%>% melt_list("site")
df_evi = melt_tree(lst_evi, c("site", "group"))
df_evi$group %<>% as.numeric()
names(df_evi)[(3:7) + 2] %<>% paste0("EVI.", .)

df = merge(df_mod09a1[site %in% st_166$site], df_evi, by = c("site", "group", "t"), all.x = TRUE)
pheno_T <- readRDS(file_MYD11A2_pheno)
df2 <- cal_Wscalar(df, pheno_T)

# process flux site GPP data
df_gpp  <- fread(file_official_dd)
df2_gpp <- process_gpp(df_gpp, st_212)$d8

d_vpm <- fread("INPUT/fluxnet212/flux212_GPP_vpm(zhang2017).csv")
d_vpm$date %<>% ymd()

df_final <- merge(df2_gpp,
    df2[, .(site, group, t, date, EVI, EVI2, NDVI, LSWI, LSWI_max, Wscalar,
            w, QC_flag, EVI.AG, EVI.Beck, EVI.Elmore, EVI.Zhang, EVI.whit)],
    by = c("site", "date")) %>%
    merge(d_vpm[, .(site, date, GPP_vpm)], by = c("site", "date"), all.x = TRUE)

# make sure Tscalar and Wscaler in the range of 0-1
vars_scalar <- c('Wscalar', 'Tscalar')
df_final[, (vars_scalar) := lapply(.SD, clamp, lims = c(0, 1)), .SDcols = vars_scalar]

saveRDS(df_final, "pheno_async_INPUT_st114.RDS")

df_fullset = fread(file_FULLSET_dd)
colnames(df_fullset)[3] <- "dn"

{
    # df = df_final
    # df = merge(df_final, df_fullset[, .(site, year, dn, Tair_day)],
    #            by = c("site", "year", "dn"))
    df = df_obs
    df[, Tscalar2 := cal_Tscalar(Tair_day, IGBP)]
    df[, `:=`(GPP_sim  = pmax(0, 0.078*1.25*(EVI.whit-0.1)*Rs*Tscalar*Wscalar))]
    df[, as.list(GOF(GPP, GPP_sim, include.r = TRUE))] %>% print()
    df[, as.list(GOF(GPP, GPP_vpm, include.r = TRUE))] %>% print()
    # d_mod09a1[, `:=`(GPPsim3  = pmax(0, 0.078*1.25*(EVI-0.1)*PPFD_IN*Tscalar*Wscalar))]
    # d_mod09a1[, as.list(GOF(GPP, GPPsim3))]
}

# NCEP2 forcing:
# 1. Tair.2m https://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBListFiles.pl?did=59&tid=72238&vid=4237
# 2. Rs https://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBListFiles.pl?did=59&tid=72238&vid=4241

# df_mod09a1[is.na(EVI.whit), EVI.whit := EVI]
# NSE = 0.62,
############################### AGGREGATE GPP_EC ###############################
############################ LOAD SATELLITE DATA ###############################
## 2.1 MOD13A1
# d_mod13a1 <- lst_sm$MOD13A1[scale == "0m", .(site, date, NDVI, EVI, SummaryQA)]
# d_mod13a1 <- merge(data_d16, d_mod13a1, by = c("site", "date")) # no negative after fix_neg
# d_mod13a1[, APAR := Rs*0.45*1.25*(EVI - 0.1)] # Zhang Yao, 2017, sci data

## 2.2 MOD09A1
## 2.2.1 tidy MOD09A1
# d_mod09a1 <- d_mod09a1[scale == "0m", .(site, t, date, year, EVI, EVI2, NDVI, LSWI, w, StateQA, QC_flag)]
# with(, plot(EVI2, y)); grid(); abline(a = 0, b=1, col="red")

## Wscaler
## Tscalar
# d_mod09a1 <- merge(data_d8, d_mod09a1, by = c("site", "date", "year", "year2","lat"))

# # add APAR
# d_mod09a1[, APAR := Rs*0.45*1.25*(EVI - 0.1)] # Zhang Yao, 2017, sci data
# save(d_mod13a1, d_mod09a1, st, file = file_MOD09A1)

# d_mod09a1[, `:=`(GPPsim3 = pmax(0, 0.078*1.25*(EVI-0.1)*Rs*Tscalar*Wscalar))]
# d_mod09a1[, `:=`(GPPsim2 = pmax(0, 0.078*1.25*(EVI-0.1)*PPFD_IN*Tscalar*Wscalar))]
# d_mod09a1[, as.list(GOF(GPP, GPPsim)), .(IGBP)]
# d_mod09a1[, as.list(GOF(GPP, GPPsim2)), .(IGBP)]
# d_mod09a1[, as.list(GOF(GPP, GPP_vpm)), .(IGBP)]

# 0.42
# g C m-2 day-1 /mol m-2 day -1
# 0.078
# g C m-2 day-1 /W m-2
