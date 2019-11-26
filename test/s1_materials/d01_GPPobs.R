rm(list = ls())
source("test/stable/load_pkgs.R")

tidy_mod13 <- function(df){
    res <- df[, .(site, date,
                   DayOfYear = as.integer(round(DayOfYear)),
                   NDVI = NDVI/1e4, EVI = EVI/1e4, SummaryQA, scale)] %>%
        getRealDate()
    res$w <- qc_summary(res$SummaryQA)
    res
}

initialw_5l <- function(df){
    df$w <- qc_5l(df$QC)
    df
}

stations <- fread(paste0(dir_flux, "station/flux_166.csv"))
stations$ID <- 1:nrow(stations)
# tasks:
# check multiple growing season regions
# whether wWHd or wHANTS

# prepare gpp input data, 04 March, 2018
# Update 19 Sep, 2018
# 1. fluxsite observations -----------------------------------------------------
df_raw <- fread(paste0(dir_flux, "fluxsites166_official_dd.csv"))
df_raw <- merge(df_raw, stations[, .(site, lat, IGBP)])
# South Hemisphere
df_raw$date %<>% ymd()

# GPP_NT has 42147 negative values, while GPP_DT has no negative value.
info_raw <- df_raw[, .(npos  = sum(GPP_DT >= 0, na.rm = T),
                       npos2 = sum(GPP_DT >= 1, na.rm = T)), .(site, year)]

nday  <- 300 # 365*0.6 # 365*0.6 # 300
info1 <- info_raw[year >= 2000 & npos >= nday & npos2 >= 100, ]
# info2 <- info_raw[N <  nday, ] #too less, all year data set to NA

# merge will drop site-year not in info1
d_obs1 <- merge(info1, df_raw, by = c("site", "year"))

# d_obs2 <- merge(info2, df_raw, by = c("site", "year"))
# d_obs2$GPP_NT <- NA
# d_obs <- rbind(d_obs1, d_obs2)

df <- d_obs1
df %<>% reorder_name(c("site", "IGBP", "lat","date", "year", "month",
                       "growing", "N", "GPP_DT", "GPP_NT", "GPP_vpm"))
setkeyv(df, c("site", "date"))

sites_rm <- c("GF-Guy", "AU-Cum")
sites <- unique(df$site) %>% setdiff(sites_rm)
st <- stations[site %in% sites]
st$IGBP <- factor(st$IGBP, IGBPnames_006)
st    <- st[order(IGBP, site)] %>% {.[, ID := 1:.N]}
sites <- st$site

fprintf("%s sites left ...\n", length(sites))

save(df, st, sites, file = "data_test/phenoflux_115.rda")
# clip selected site
# d_vpm <- clip_selectedSite(d_vpm)

# x[, .N, .(year(date))][N>365*0.6] %>% nrow
# d_obs <- merge(d_obs, d_vpm, by = c("site", "date"), all.x = T)

# save("d_obs", file = "Y:/R/phenofit/data/phenofit_INPUT_flux136_obs.rda")

# 1. test growing season dividing -----------------------------------------


## rm(list = ls())
# # 2. zhang yao, 2017, scientific data, VPMGPP, 8day ----------------------------

dir_prj <- getwd()# "F:/Github/phenology/phenofit/"
indir <- paste0(dir_flux, "fluxnet212/")
setwd(indir)

files <- dir(pattern = "*.csv", full.names = T)

df_GPP_vpm     <- fread("flux212_GPP_vpm(zhang2017).csv") %>%
    .[, .(site, date = ymd(date), GPP_vpm)]
df_GPP_mod     <- fread("phenoflux212_MOD17A2H_006.csv") %>%
    .[, .(site, date = ymd(date), GPP_mod = Gpp/80, QC = as.integer(Psn_QC), scale)] %>% # daily, gC m-2 d-1
    initialw_5l()
df_GPP_fluxcom <- fread("flux212_GPP_fluxcom_d8.csv") %>%
    .[, .(site, date = ymd(date),GPP_fluxcom = value)]

df_MOD13A1     <- fread("phenoflux212_MOD13A1_006.csv") %>% tidy_mod13
df_MOD13Q1     <- fread("phenoflux212_MOD13Q1_006.csv") %>% tidy_mod13

df_LAI         <- fread("phenoflux212_MCD15A3H_006.csv") %>%
    .[, .(site, date = ymd(date), LAI = Lai/10, Fpar = Fpar/100, QC = as.integer(FparLai_QC), scale)] %>%
    initialw_5l()

df_ET_mod  <- fread("phenoflux212_MOD16A2_006.csv") %>%
    .[, .(site, date = ymd(date), QC = ET_QC, ET_mod = ET/80, scale)] %>%
    initialw_5l()

df_ET_gleam    <- fread("flux212_ET_gleam_d8.csv")
df_ET_gleam %<>% .[site %in% sites, .(site, date = ymd(date), ET_gleam = value)]
df_ET_gleam <- df_ET_gleam[ET_gleam > -999, ] #_Fillvalue -999

lst <- listk(df_GPP_mod, df_GPP_vpm, df_GPP_fluxcom,
             df_ET_mod, df_ET_gleam,
             df_MOD13A1, df_MOD13Q1, df_LAI) %>%
    set_names(gsub("df_", "" ,names(.)))
lst_sm <- map(lst, ~.x[site %in% sites, ])

setwd(dir_prj)

save(lst   , file = "data_test/ET&GPP&VI_flux212.rda")
save(lst_sm, file = "data_test/ET&GPP&VI_flux115.rda")


get_ET_data <- function(){
    ## other products, MTE_ET only monthly, not compared
    # "CA-SF3" "IT-BCi", no data
    df <- reduce(list(df_PML, df_GPP_vpm,
                      df_ET_MODIS, df_GPP_MODIS,
                      df_GPP_fluxcom, df_ET_gleam), merge,
                 by = c("site", "date"), all.x = T)
}

# df_melt <- melt(df, id.vars = c("site", "date", "year", "d8"), variable.name = "model")
# df_melt <- df_melt[!is.na(value), ]

# file_vpm <- paste0(dirname(indir), "/flux212_GPP_vpm(zhang2017).csv")

# if (!file.exists(file_vpm)){
#     files <- dir(indir, full.names = T) %>% set_names(gsub(".csv","", basename(.)))
#     d_vpm <- ldply(files, fread, .id = "site")[, c(1, 3, 4)] %>%
#         set_names(c("site", "date", "GPP")) %>% as.data.table()
#     d_vpm[, date := as.Date(as.character(date), "%Y%j")]
#     d_vpm %<>% set_colnames(c("site", "date", "GPP_vpm"))
#     d_vpm$w <- 1
#     fwrite(d_vpm, file_vpm)
# } else{
#     d_vpm <- fread(file_vpm)
# }

# # main functions ----------------------------------------------------------
# clip_selectedSite <- function(INPUT){
#     d_temp <- ldply(sites, function(name){
#         di_obs <- d_obs[site == name, ]
#         date_begin <- min(di_obs$date)
#         date_end   <- max(di_obs$date)

#         INPUT[site == name & (date >= date_begin & date <= date_end), ]
#     }, .id = NULL)
#     merge(siteInfo, d_temp)#RETURN directly
# }

# load("Y:/R/phenofit/data/phenofit_INPUT_flux136_obs.rda")
# load("Y:/R/phenofit/data/phenofit_MultipleINPUT_flux212.rda")

# sites    <- unique(d_obs$site) %>% set_names(., .)
# siteInfo <- unique(d_obs[, .(site, lat, IGBP)])

# d_vpm %<>% clip_selectedSite

# # Multiple Products -------------------------------------------------------
# INPUT <- lst$MOD13A1
# INPUT_lst <- llply(lst[1:4], clip_selectedSite, .progress = "text")
# INPUT_lst %<>% c(list(GPP_vpm = d_vpm))

# save(INPUT_lst, file = "Y:/R/phenofit/data/phenofit_MultipleINPUT_flux136.rda")


