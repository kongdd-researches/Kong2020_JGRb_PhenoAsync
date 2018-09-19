rm(list = ls())
source("test/stable/load_pkgs.R")
stations <- fread(paste0(dir_flux, "station/flux_166.csv"))
stations$ID <- 1:nrow(stations)
# tasks:
# check multiple growing season regions
# whether wWHd or wHANTS

# prepare gpp input data, 04 March, 2018
# Update 12 Sep, 2018
# 1. fluxsite observations -----------------------------------------------------
df_raw <- fread(paste0(dir_flux, "fluxsites166_official_dd.csv"))
df_raw <- merge(df_raw, stations[, .(site, lat, IGBP)])
# South Hemisphere
df_raw$date %<>% ymd()

# GPP_NT has 42147 negative values, while GPP_DT has no negative value.
info_raw <- df_raw[GPP_DT >= 0, .N, .(site, year)]

nday <- 300 # 365*0.6 # 365*0.6 # 300
info1 <- info_raw[N >= nday, ]
info2 <- info_raw[N <  nday, ] #too less, all year data set to NA

# merge will drop site-year not in info1
d_obs1 <- merge(info1, df_raw, by = c("site", "year"))

# d_obs2 <- merge(info2, df_raw, by = c("site", "year"))
# d_obs2$GPP_NT <- NA
# d_obs <- rbind(d_obs1, d_obs2)

d_obs <- d_obs1
d_obs %<>% reorder_name(c("site", "IGBP", "lat","date", "year", "month", "growing", "N", "GPP_DT", "GPP_NT", "GPP_vpm"))
setkeyv(d_obs, c("site", "date"))

## Obstacles
# 1. date is not contious, because we select according to site-year. However, we
# can kept those site-year data.
#

# check if year is continues
info <- d_obs[, .(site, year)] %>% unique()
info[, flag := cumsum(c(0, diff(year) != 1)), .(site)]
info[, len  := last(year) - first(year) + 1, .(site, flag)]
info

# no continous date sites and inspect by visual
site_nocon <- info[flag > 0]$site %>% unique()
# d_obs <- d_obs[lat > 0, length(unique(site))] #remove south hemisphere
# x <- df[site == sitename, ]

sites <- unique(d_obs$site)
fprintf("left %s sites...\n", length(sites))

# clip selected site
# d_vpm <- clip_selectedSite(d_vpm)

# x[, .N, .(year(date))][N>365*0.6] %>% nrow
# d_obs <- merge(d_obs, d_vpm, by = c("site", "date"), all.x = T)

# save("d_obs", file = "Y:/R/phenofit/data/phenofit_INPUT_flux136_obs.rda")

# 1. test growing season dividing -----------------------------------------


# rm(list = ls())

# # 2. zhang yao, 2017, scientific data, VPMGPP, 8day ----------------------------
# indir <- paste0(dir_flush, "ET&GPP/fluxnet212/GPP_vpm")
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
