## multiple seasons
source("test/main_pkgs.R")
source("test/data-prepare/s1_dat0_divide_growing_season.R")

calendarYear = TRUE

# InitCluster(12)
sites2 = c(sites_multi, sites_rm) %>% set_names(., .)
sites_single = sites_single %>% set_names(., .)
info  = info_full

nptperyear = 365
varname = "GPP_NT"
version = glue("({varname}) v0.2.6.9000") # test version

## 1. divide growing seasons
lst_brks.multi  = main_divide_season(df_part, info, sites_multi = sites2, sites_single,
    calendarYear = FALSE, varname = varname,
    outfile = glue("Figure/phenofit_seasons_multi_{version}.pdf"))

# if calendarYear then single
lst_brks.single = main_divide_season(df_part, info, sites_multi = sites2, sites_single,
    calendarYear = TRUE, varname = varname,
    outfile = glue("Figure/phenofit_seasons_single_{version}.pdf"))

## 3. curve fitting and get phenology metrics
lst_brks  = c(lst_brks.single, lst_brks.multi)
TRSs = c(0.1, 0.2, 0.5, 0.6, 0.8, 0.9)

InitCluster(10)
lst_pheno = main_phenofit(lst_brks, TRS = TRSs,
    show = FALSE, outfile = glue("Figure/gpp_phenofit_pheno_{version}.pdf"))
# lst_pheno2 = main_phenofit(lst_brks[86:length(lst_brks)], TRS = TRSs,
#     show = TRUE, outfile = glue("Figure/gpp_phenofit_pheno_{version}2.pdf"))

outfile = glue("INPUT/pheno_gpp_st109 {version}.rda")
save(lst_brks.single, lst_brks.multi, lst_brks, lst_pheno,
     sites_multi, sites_single, file = outfile)

## visualization
# write_fig(g, "Figure1_phenofit_curve_fitting.pdf", 10, 6)
# ratio = 1.15
# file <- "Figure5_Phenology_Extraction_temp.pdf"
# cairo_pdf(file, 8*ratio, 6*ratio)
# temp <- get_pheno(fit$fits$ELMORE[2:6], IsPlot = T)
# dev.off()
# file.show(file)
