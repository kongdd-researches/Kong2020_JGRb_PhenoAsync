source('inst/shiny/check_season/global.R')
source("test/stable/load_pkgs.R")
# source("test/phenology_async/R/s1_materials/main_phenofit.R")
load("data/phenoflux_115_gs.rda")

df <- readRDS("data_test/MOD09A1_VI_flux212.RDS")
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

# df <- lst_sm$MOD13A1
# df <- df[scale == "0m", .(site, t, date, y = EVI, w, SummaryQA)]
# df$SummaryQA %<>% factor(qc_values, qc_levels)

st[, `:=`(IGBPname = IGBP, lon = long)]

nptperyear <- as.numeric(diff(df$date[1:2])) %>% {ceiling(366/.)}
if (nptperyear == 23) lambda0 <- NULL # init_lambda will be called
if (nptperyear == 46) lambda0 <- 15

IsPlot     <- T
i          <- 1
sitename <- sites[1]

# FUN_fit <- ifelse(sp$IGBP %in% IGBP_forest, "wHANTS", "wWHIT")
wFUN <- wBisquare# "wBisquare", "wTSM"

## version v1: self brks
# fit <- get_phenofit(sitename, df, st, prefix_fig = 'phenofit_v0.1.7', IsPlot = F,
#                          brks = NULL,
#                          ypeak_min = 0.05,
#                          lambda = NULL, isVarLambda = FALSE,
#                          nextent = 2, maxExtendMonth = 2, minExtendMonth = 1)

getVI_phenofit <- function(sitename, df, prefix_fig = 'phenofit_v0.1.6'){
    i <- grep(sitename, sites)
    brks <- brks_lst[[i]]
    ## version v2: GPPobs brks
    sp <- st[site == sitename, ]
    if (sp$IGBP %in% c("CRO", "GRA")){
        maxExtendMonth = 1
    } else {
        maxExtendMonth = 2
    }
    fit <- get_phenofit(sitename, df, st, prefix_fig = prefix_fig, IsPlot = F,
                        brks = brks,
                        wFUN_fit = wFUN,
                        ypeak_min = 0.05,
                        lambda = lambda0, isVarLambda = FALSE,
                        nptperyear = nptperyear, # important
                        nextent = 2, maxExtendMonth = maxExtendMonth, minExtendMonth = maxExtendMonth/2)
    return(fit)
}

prefix <- "v0.1.8_MOD09A1_EVIc15"

sites %<>% set_names(sites)
sitename <- sites[1]
# res <- par_sbatch(sites, getVI_phenofit)
outdir <- paste0(dir_flush, 'result/phenoflux115/', prefix)
fits   <- par_sbatch(sites, getVI_phenofit,
                     df = df,
    Save = T, outdir = outdir, prefix_fig = paste0('phenoflux_', prefix))

## 2. combine figures
phenofit::merge_pdf(sprintf('phenoflux_%s_1.pdf', prefix), indir = 'Figure/', prefix, del = F)

# x <- fit$fits$AG$`2002_1`
#
# x <- fit$AG
# par(mfcol = c(3, 1))
#
# plot(x$ws$iter2, col = "red", type = "b")
# lines(x$ws$iter1, col = "blue", type = "b")
# grid()
#
# plot(x$fits$iter1, col = "blue")
# lines(x$fits$iter2, col = "red")
# grid()

# plotdata(d[42:70])

