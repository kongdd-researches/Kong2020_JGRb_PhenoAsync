# source("R/FLUXNET/Main.R")
source("test/main_pkgs.R")

If the percentage of measured and 
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
lst2 <- foreach(d = lst, lat = st_166$lat, i = icount(1)) %do% {
    tidy_daily(d, lat, perc_valid = 0.8)
}

df <- melt_list(lst2, 'site')
df$YYYY <- year(df$date)

# also need to remove leapyear 2-29; why?
fwrite(df, 'data/fluxsites166_official_dd.csv')
