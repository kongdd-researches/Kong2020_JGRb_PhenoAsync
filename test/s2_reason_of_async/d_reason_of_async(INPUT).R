# PPFD_IN: Photosynthetic photon flux density, incoming, n2q    qq  q1gb
# PAR    : photosynthetically active radiation

# source('inst/shiny/check_season/global.R')
source("test/main_pkgs.R")


# load("data/phenoflux_115_gs.rda") # st
file_MOD09A1 = "INPUT/fluxnet115/flux115_async_input.rda"

df_mod09a1 <- readRDS("INPUT/st212_MOD09A1_9grids.RDS")

load(file_MOD09A1)
load("INPUT/pheno_MOD09A1_EVI_st166.rda")

lst_evi <- map_depth(lst_EVI, 2, ~get_fitting2(.x)) #%>% melt_list("site")
df_evi = melt_tree(lst_evi, c("site", "group"))
df_evi$group %<>% as.numeric()
names(df_evi)[(3:7)+2] %<>% paste0("EVI.", .)

df = merge(df_mod09a1, df_evi, by = c("site", "group", "t"), all.x = TRUE)

# process flux site GPP data
df_gpp  <- readRDS(file_GPP_north)
lst_gpp <- process_gpp(df_gpp, st_212)

# df_mod09a1[is.na(EVI.whit), EVI.whit := EVI]
{
    df = df_mod09a1
    df[, `:=`(GPPsim3  = pmax(0, 0.078*1.25*(EVI.whit-0.1)*Rs*Tscalar*Wscalar))]
    df[, as.list(GOF(GPP, GPPsim3, include.r = TRUE))] %>% print()
    # d_mod09a1[, `:=`(GPPsim3  = pmax(0, 0.078*1.25*(EVI-0.1)*PPFD_IN*Tscalar*Wscalar))]
    # d_mod09a1[, as.list(GOF(GPP, GPPsim3))]
}

# NSE = 0.62,
############################### AGGREGATE GPP_EC ###############################
############################ LOAD SATELLITE DATA ###############################
## 2.1 MOD13A1
d_mod13a1 <- lst_sm$MOD13A1[scale == "0m", .(site, date, NDVI, EVI, SummaryQA)]
d_mod13a1 <- merge(data_d16, d_mod13a1, by = c("site", "date")) # no negative after fix_neg

d_mod13a1[, APAR := Rs*0.45*1.25*(EVI - 0.1)] # Zhang Yao, 2017, sci data

## 2.2 MOD09A1
## 2.2.1 tidy MOD09A1
# d_mod09a1 <- readRDS("data_test/flux212_MOD09A1_VI.RDS")
# d_mod09a1 <- d_mod09a1[scale == "0m", .(site, t, date, year, EVI, EVI2, NDVI, LSWI, w, StateQA, QC_flag)]
#
# d_mod09a1 <- merge(d_mod09a1, st[, .(site, lat)])
# d_mod09a1[, year2 := year + ((month(date)>=7)-1)*(lat<0)]
# with(, plot(EVI2, y)); grid(); abline(a = 0, b=1, col="red")

## Wscaler
pheno_T <- readRDS('INPUT/fluxnet115/flux212_MYD11A2_T_phenology_5d_10d.RDS')

## Tscalar
d_mod09a1 <- merge(data_d8, d_mod09a1, by = c("site", "date", "year", "year2","lat"))
d_mod09a1[, Tscalar := cal_Tscalar(T, IGBP), .(IGBP)]

# make sure Tscalar and Wscaler in the range of 0-1
vars_scalar <- c('Wscalar', 'Tscalar')
d_mod09a1[, (vars_scalar) := lapply(.SD, clamp), .SDcols = vars_scalar]

# add APAR
d_mod09a1[, APAR := Rs*0.45*1.25*(EVI - 0.1)] # Zhang Yao, 2017, sci data
save(d_mod13a1, d_mod09a1, st, file = file_MOD09A1)


d = d_mod09a1[, .(GPP2 = pmin(epsilon_C3 * Tscalar * Wscalar * APAR, 0), GPPsim, GPPsim2, GPP), ]

d_mod09a1[, `:=`(GPPsim3  = pmax(0, 0.078*1.25*(EVI-0.1)*Rs*Tscalar*Wscalar))]
d_mod09a1[, `:=`(GPPsim2 = pmax(0, 0.078*1.25*(EVI-0.1)*PPFD_IN*Tscalar*Wscalar))]

d_mod09a1[, as.list(GOF(GPP, GPPsim)), .(IGBP)]
d_mod09a1[, as.list(GOF(GPP, GPPsim2)), .(IGBP)]

d_vpm <- fread("file:///F:/Github/data/flux/fluxnet212/flux212_GPP_vpm(zhang2017).csv")
d_vpm$date %<>% ymd()
d_mod09a1 <- merge(d_mod09a1, d_vpm[, .(site, date, GPP_vpm)], by = c("site", "date"))

d_mod09a1[, as.list(GOF(GPP, GPP_vpm)), .(IGBP)]

d_mod09a1[, as.list(GOF(GPP, GPP_vpm))]
d_mod09a1[, as.list(GOF(GPP, GPPsim))]
d_mod09a1[, as.list(GOF(GPP, GPPsim2))]
d_mod09a1[, as.list(GOF(GPP, GPPsim3))]

# 0.42
# g C m-2 day-1 /mol m-2 day -1
# 0.078
# g C m-2 day-1 /W m-2
