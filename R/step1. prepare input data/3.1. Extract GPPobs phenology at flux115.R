source('inst/shiny/check_season/global.R')
source("test/stable/load_pkgs.R")
source("test/phenology_async/R/step1. prepare input data/main_phenofit.R")
load("data_test/phenoflux_115_gs.rda")

# source("test/phenology_async/R/step1. prepare input data/main_phenofit.R")

debug <- T
if (debug){
    sites0   <- c("AU-Stp", "IT-Noe", 'US-Syv', 'US-WCr')
    subfix <- "_part"
} else {
    sites0   <- st$site
    subfix <- ""
}
sites0 %<>% set_names(sites0)

file <- sprintf("Figures3_phenofit_GPPobs_v0.1.1.pdf", subfix)
CairoPDF(file, 12, 7)
fits <- par_sbatch(sites0, get_phenofit_GPPobs, df, st, brks_lst, sites, wFUN = 'wTSM')
dev.off()

# .progress = "text"
