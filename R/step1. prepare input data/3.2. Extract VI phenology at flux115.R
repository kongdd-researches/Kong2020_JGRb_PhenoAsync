source('inst/shiny/check_season/global.R')
source("test/stable/load_pkgs.R")
# source("test/phenology_async/R/step1. prepare input data/main_phenofit.R")
load("data/phenoflux_115_gs.rda")

df <- lst_sm$MOD13A1

df <- df[scale == "0m", .(site, t, date, y = EVI, w, SummaryQA)]
df$SummaryQA %<>% factor(qc_values, qc_levels)

st[, `:=`(IGBPname = IGBP, lon = long)]

nptperyear <- 23
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
        maxExtendMonth = 3
    }
    fit <- get_phenofit(sitename, df, st, prefix_fig = prefix_fig, IsPlot = F,
                        brks = brks,
                        wFUN_fit = wFUN,
                        ypeak_min = 0.05,
                        lambda = NULL, isVarLambda = FALSE,
                        nextent = 2, maxExtendMonth = maxExtendMonth, minExtendMonth = maxExtendMonth/2)
    return(fit)
}

sites %<>% set_names(sites)
# res <- par_sbatch(sites, getVI_phenofit)
outdir <- paste0(dir_flush, 'result/phenoflux115/V0.1.6_MOD13A1_EVI')
fits   <- par_sbatch(sites, getVI_phenofit,
                     df = df
    Save = T, outdir = outdir, prefix_fig = 'phenoflux_0.1.6_MOD13A1_EVI')


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
phenofit::merge_pdf('phenoflux_0.1.6_MOD13A1_EVI.pdf', indir = 'Figure/')
