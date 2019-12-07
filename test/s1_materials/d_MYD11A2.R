## MYD11A2 LST_Night_1km at 212 flux sites
#  Dongdong Kong, 20181007
source("test/stable/load_pkgs.R")
source('test/phenology_async/R/main_async.R')
source('test/phenology_async/R/main_thermal.R')

# need site info: st
show_description <- function(d){
    cat(attr(d, "description"))
}

indir <- "D:/Document/GoogleDrive/phenofit/phenology_async/data/"
files <- dir(indir, pattern = 'MYD11A2', full.names = T) %>%
    set_names(str_extract(basename(.), "\\d{1,4}m"))

# lst <- llply(files, fread, .progress = "text")

df <- llply(files, function(file){
    d <- fread(file)
    d[, `:=`(date = substr(`system:index`, 1, 10),
             site = str_extract(`system:index`, "(?<=_)[:alpha:]{2}-\\w{3}"))] #(?=[_]{0,1})
    d[, c("system:index", ".geo") := NULL]
    d
})
df <- melt_list(df, "scale")
setkeyv(df, c("scale", "site", "date"))

df <- df[, .(site, date, scale, QC_Night, QC_Day,
    LST_Night_1km = LST_Night_1km*0.02 - 273.15,
    LST_Day_1km   = LST_Day_1km*0.02 - 273.15)]

# add year variable, year2 considering lat
df %<>% plyr::mutate(date = ymd(date), year = year(date))
df <- merge(df, st[, .(site, lat)], by = "site") %>%
    plyr::mutate(year2 = year + ((month(date)>=7)-1)*(lat<0))

## fill missing values in df
# maxgap = 5*8 = 40d
# summary(df)
df[, LST_Night_1km := na.approx(LST_Night_1km, maxgap = 5, na.rm = F), .(site, scale)]

# df %<>% getRealDate() # real date from DOY
# df[, w := qc_StateQA(StateQA)]
# df[, c("w", "QC_flag") := qc_StateQA(StateQA)]

headvars <- c("site", "scale", "date", "t", "year", "year2", "doy", "DayOfYear", "StateQA")
df <- reorder_name(df, headvars)
df$scale %<>% factor()

Description <- paste("MYD11A2 LST_Night_1km at 212 flux sites", "\n",
                     "-------------------------------------------", "\n",
                     "Author: Dongdong Kong,", Sys.time(), "\n",
                     "Cooperate with GEE script:", "https://code.earthengine.google.com/f138b791cb307db07894a27dfaebb8e5", "\n",
                     "Rely on kongdd/public (commit='764e406')", "\n",
                     "Caution: `year2` has adjusted south hemisphesre.",
                     seq = "") %T>% cat
attr(df, "description") <- Description
saveRDS(df, file = "data_test/flux212_MYD11A2_Tnight.RDS")

################################################################################
## 2. Get thermal phenology
df0 <- df[scale == "0m"]
df0 <- ddply(df0, .(site), add_Head) # add 199907-200206 in head
df0[, year2 := year + ((month(date)>=7)-1)*(lat<0)]

# source('test/phenology_async/R/main_thermal.R')
## all sites
pheno_T_mod11a2 <- df0[, Pheno_thermal(LST_Night_1km, date, trs = c(5, 10)), .(site, scale, year2)]

# check whether still have sos_date > eos_date
merge(st[, .(site, lat)], pheno_T_mod11a2[sos_date >= eos_date])

attr(pheno_T_mod11a2, "description") <- Description
saveRDS(pheno_T_mod11a2, file = "data_test/flux212_MYD11A2_T_phenology_5d_10d.RDS")
