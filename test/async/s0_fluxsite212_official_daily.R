source("test/main_pkgs.R")

# FLUNET daily observations are
# If the percentage of daily good quality gap-filled data is less than `perc_valid`,
# the corresponding values will be set as invalid.

# tier2 has been remove; only 166 files in the current input directory
files <- dir("G:/Github/data/flux/fluxnet212_raw/raw/SUBSET/tier1",
    recursive = T, pattern = "*.SUBSET_DD_*.", full.names = T) %>%
    { set_names(., substr(basename(.), 5, 5+5)) }

# check file and station order first
uniqs <- match(names(files), st_166$site) %>% diff() %>% unique()
if (length(uniqs) != 1) stop("Check file order please!")

lst <- map(files, fread, na.strings = "-9999") #-9999 has been replaced with na

# lst2 (after tidy)
lst2 <- foreach(d = lst, lat = st_166$lat, i = icount()) %do% {
    tidy_daily(d, lat, perc_valid = 0.8)
}

df <- melt_list(lst2, 'site')
df$YYYY <- year(df$date)

# also need to remove leapyear 2-29; why?
file_official_dd = 'data/fluxsites166_official_dd.csv'
fwrite(df, file_official_dd)

## 1. select sites
df       = fread(file_official_dd)
info     = df[, .(ngpp_nt = sum(!is.na(GPP_NT)),
                  ngpp_dt = sum(!is.na(GPP_DT)),
                  max = max(GPP_DT, na.rm = TRUE)), .(site, year)]

info_left = info[ngpp_dt >= 365 * 0.7 & max >= 2, .(site, year)] # 8 site-year removed
df_north  = st_166[lat > 0, .(site)] %>% merge(info_left) %>%
    merge(df, ., by = c("site", "year")) # North Hemisphere， 136 site left (116 north)

# original 1301 site-year
# info[ngpp_nt >= 365 * 0.7]
relative_range <- function(x){
    x = x[!is.na(x)]
    diff(range(x))/mean(x)
}

## check the performance of growing season dividing
df_north[, .(range_dt = relative_range(GPP_DT), range_nt = relative_range(GPP_NT)), .(site, year)]
x = df_north[, .(cv_dt = cv_coef(GPP_DT)[3], cv_nt = cv_coef(GPP_NT)[3]), .(site, year)]

x[cv_dt >= 0.3]
saveRDS(df_north, file_GPP_north)
## 1. after filter 70% valid observation in each site-year, then 865 site-year (142 sites) left

## 2. It is difficult to accurate extract vegetation phenology for vegetation with multiple growing seasons.
# In this study, we removed those sites with num of growing seasons >= 3. For the left
# sites, we divided vegetation growing season in a emprical method by R package `phenofit`
# (supplementary text 1).

# We further masked vegetation without apparent seasonality, which is quantified by
# Coefficient of Variation (CV = sd/mean) < 0.1.
# We used following criterion
# 2. Inspect in Google Earth, with homogeneous land cover in 500-m fetch

## 数据尺度保持一致，只采用500m的数据
