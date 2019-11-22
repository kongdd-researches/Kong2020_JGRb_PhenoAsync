## multiple seasons
library(data.table)
library(magrittr)
library(foreach)
library(phenofit)

source("test/async/s1_dat0_divide_growing_season.R")

calendarYear = TRUE

sites2 = c(sites_multi, sites_rm) %>% set_names(., .)
sites_single = sites_single %>% set_names(., .)
info  = info_full

nptperyear = 365
version = 0.1 # test version
df_part = df_north[date >= date_start & date <= date_end, ] %>% setkeyv(c("site"))

## 2. check_season
lst_multi  = main_divide_season(df_part, info, sites_multi = sites2, sites_single, 
    calendarYear = FALSE, 
    outfile = glue("phenofit_seasons_multi_{version}.pdf"))

lst_single = main_divide_season(df_part, info, sites_multi = sites2, sites_single, 
    calendarYear = TRUE, 
    outfile = glue("phenofit_seasons_single_{version}.pdf"))

## visualization
# write_fig(g, "Figure1_phenofit_curve_fitting.pdf", 10, 6)

# ratio = 1.15
# file <- "Figure5_Phenology_Extraction_temp.pdf"
# cairo_pdf(file, 8*ratio, 6*ratio)
# temp <- get_pheno(fit$fits$ELMORE[2:6], IsPlot = T)
# dev.off()
# file.show(file)
