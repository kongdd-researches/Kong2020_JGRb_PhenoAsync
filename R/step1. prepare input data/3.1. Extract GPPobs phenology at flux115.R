source('inst/shiny/check_season/global.R')
source("test/stable/load_pkgs.R")
# source("test/phenology_async/R/step1. prepare input data/main_phenofit.R")
load("data/phenoflux_115_gs.rda")

st[, `:=`(IGBPname = IGBP, lon = long)]
# source("test/phenology_async/R/step1. prepare input data/main_phenofit.R")

debug <- F
if (debug){
    sites0   <- c("AU-Stp", "IT-Noe", 'US-Syv', 'US-WCr')
    subfix <- "_part"
} else {
    sites0   <- st$site
    subfix <- ""
}
sites0 %<>% set_names(sites0)

file <- sprintf("Figures3_phenofit_GPPobs_v0.1.1.pdf", subfix)
# CairoPDF(file, 12, 7)
outdir <- paste0(dir_flush, 'result/phenoflux115')
fits <- par_sbatch(sites0, get_phenofit_GPPobs, df, st, brks_lst, sites, wFUN = 'wTSM',
	Save = T, outdir = outdir, prefix_fig = 'phenoflux_0.1.6_GPPobs')
# dev.off()
# .progress = "text"
# phenofit::merge_pdf('phenofit_cam133_MOD13A1_v1.pdf', indir = 'Figure/', 'phenocam133.*.pdf')
# 
# phenofit::merge_pdf('phenofit_flux166_MOD13A1_v1.pdf', indir = 'Figure/', 'phenoflux166.*.pdf')