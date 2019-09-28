## cal VI from MOD09A1
#  Dongdong Kong, 20181007
source("test/stable/load_pkgs.R")

show_description <- function(d){
    cat(attr(d, "description"))
}

# indir <- "D:/Document/GoogleDrive/phenofit/phenology_async/data"
# files <- dir(indir, pattern = 'MOD.*', full.names = T)

files <- "D:/Document/GoogleDrive/phenofit/MOD09A1_st3_TreeRing_EVI_LSWI_0m_buffer.csv"
# files <- dir(indir, pattern = 'MOD.*.csv', full.names = T)
files %<>% set_names(str_extract(basename(.), "\\d{1,3}m"))
# lst <- llply(files, fread, .progress = "text")

df <- llply(files, function(file){
    d <- fread(file)
    if (!all(c("date", "site" ) %in% colnames(d))){
        d[, `:=`(date = substr(`system:index`, 1, 10),
                 site = str_extract(`system:index`, "(?<=_)[:alpha:]{2}-\\w{3}"))] #(?=[_]{0,1})
    }
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

saveRDS(df, file = "data_test/st3TreeRing_MOD09A1_VI.RDS")


# phenofit ----------------------------------------------------------------
df[, y := EVI]
sites    <- unique(df$site)
sitename <- sites[1]
st       <- data.table(ID = seq_along(sites), site = sites)

nptperyear <- 46
south <- F
ypeak_min <- 0.05
wFUN_season = wTSM
wFUN_fit = wFUN_season
maxExtendMonth = 3
minExtendMonth = 1
nextent = 5

prefix <- 'phenofit_v0.1.6'
res <- list()
for (i in seq_along(sites)){
    sitename <- sites[i]
    d     <- df[site == sitename, ] # get the first site data

    res[[i]] <- get_phenofit(sitename, df, st, prefix_fig = prefix, IsPlot = F,
                             nptperyear = nptperyear,
                             brks = NULL,
                             ypeak_min = 0.05,
                             wFUN_season = wTSM, wFUN_fit = wFUN_season,
                             lambda = 15, isVarLambda = FALSE,
                             nextent = 5, maxExtendMonth = 3, minExtendMonth = 1)
}
phenofit::merge_pdf(sprintf('phenoflux_%s.pdf', prefix), indir = 'Figure/', prefix, del = F)


df_phenology <- map(res, ~.x$pheno$date %>% melt_list("meth") %>% data.table()) %>% melt_list("site")
fwrite(df_phenology, "st3_TreeRing.csv")
save(res, df_phenology, file = "st3_TreeRing.rda")
