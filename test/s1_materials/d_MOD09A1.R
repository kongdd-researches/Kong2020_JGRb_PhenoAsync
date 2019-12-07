## cal VI from MOD09A1
#  Dongdong Kong, 20181007

indir <- "D:/Document/GoogleDrive/phenofit/phenology_async/data"
files <- dir(indir, pattern = 'MOD.*', full.names = T)

## 1. check whether csv and json are some
test_consist <- function(){
    # 1.1 first file is csv
    d_csv <- fread(files[1])

    # 1.2 second file is json
    lst <- read_json(files[2])
    d_json <- map(lst$features, "properties") %>% transpose() %>% map(fix_null) %>% map(unlist) %>% as.data.table()

    all.equal(d_csv[, .SD, .SDcols=colnames(d_json)], d_json)
    # if true, just use csv file
}


files <- dir(indir, pattern = 'MOD.*.csv', full.names = T) %>%
    set_names(str_extract(basename(.), "\\d{1,3}m"))
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

df %<>% getRealDate() # real date from DOY
# df[, w := qc_StateQA(StateQA)]
df[, c("w", "QC_flag") := qc_StateQA(StateQA)]

headvars <- c("site", "scale", "date", "t", "year", "doy", "DayOfYear", "StateQA")
df <- reorder_name(df, headvars)
df$scale %<>% factor()

Description <- paste("MOD09A1 EVI, NDVI and LSWI at 212 flux sites", "\n",
                     "-------------------------------------------", "\n",
                     "Author: Dongdong Kong,", Sys.Date(), "\n",
                     "Cooperate with GEE script:", "https://code.earthengine.google.com/17da6404b5d6d9fa1cc943e40df378da", "\n",
                     "Rely on kongdd/public (commit='764e406')", seq = "") %T>% cat
attr(df, "description") <- Description

# LWSI_year <- df[, .(LSWI_max = max(LSWI, na.rm = T)), .(site, year, scale)]
# LWSI_year[, LSWI_max := zoo::rollapply(LSWI_max, 5, nth_max, partial = T),
#           .(site, scale)] # yearly max into 5 year second maximum moving

saveRDS(df, file = "data_test/flux212_MOD09A1_VI.RDS")

# NSWI: 0.06% missing for MOD09A1

# d[, QA := StateQA]
# x[, `:=`(qc_cloud   = getBits(StateQA, 0, 1),
#          qc_shadow  = getBits(StateQA, 2, 2),
#          qc_aerosol = getBits(StateQA, 6, 7),
#          qc_snow    = getBits(StateQA, 12, 12))]

# 2. MYD09A1 -----------------------------------------------------------------

# lst <- read_json(file)$features %>% map("properties")
# lst <- lst[c(2, 1, 3:length(lst))] # for transpose, the first should be the longest
# d1 <- lst[-1] %>% transpose() %>% map(fix_null) %>% map(unlist) %>% as.data.table()

# Too many missing values, not used!
# NSWI: 41.7% missing for MYD09A1

df <- df[scale == "0m", .(site, t, date, y = EVI2, EVI, EVI2, NDVI, LSWI, w, StateQA, QC_flag)]
# df$SummaryQA %<>% factor(qc_values, qc_levels)

## tidy MOD09A1
# 1.1 make sure values in a reasonable range
df[ y > 1 | y < -0.1, y := NA]
# 1.2 remove outliers: abs(y - mean) > 3sd
df[!is.na(y), `:=`(mean = mean(y), sd = sd(y)), .(site)]
df[abs(y - mean) >= 3*sd & QC_flag != "good", y := NA_real_, .(site)]
df[, c("mean", "sd") := NULL]

df[QC_flag %in% c("cloud", "snow"), EVI := EVI2] # fix bright EVI
# with(, plot(EVI2, y)); grid(); abline(a = 0, b=1, col="red")
