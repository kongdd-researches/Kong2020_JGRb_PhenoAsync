## multiple seasons
source("test/main_pkgs.R")
source("test/async/s1_dat0_divide_growing_season.R")

calendarYear = TRUE

sites2 = c(sites_multi, sites_rm) %>% set_names(., .)
sites_single = sites_single %>% set_names(., .)
info  = info_full

nptperyear = 365
version = "v0.1" # test version
df_part = df_north[date >= date_start & date <= date_end, ] %>% setkeyv(c("site"))

## 1. divide growing seasons
lst_brks.multi  = main_divide_season(df_part, info, sites_multi = sites2, sites_single,
    calendarYear = FALSE,
    outfile = glue("Figure/phenofit_seasons_multi_{version}.pdf"))

lst_brks.single = main_divide_season(df_part, info, sites_multi = sites2, sites_single,
    calendarYear = TRUE,
    outfile = glue("Figure/phenofit_seasons_single_{version}.pdf"))

## 3. curve fitting and get phenology metrics
lst_brks  = c(lst_brks.single, lst_brks.multi)
lst_pheno = main_phenofit(lst_brks, TRS = c(0.1, 0.2, 0.5, 0.6, 0.8),
    show = FALSE, outfile = glue("Figure/gpp_phenofit_pheno_{version}.pdf"))
lst_pheno2 = main_phenofit(lst_brks[86:length(lst_brks)], TRS = c(0.1, 0.2, 0.5, 0.6, 0.8),
                          show = TRUE, outfile = glue("Figure/gpp_phenofit_pheno_{version}2.pdf"))

save(lst_brks.single, lst_brks.multi, lst_brks, lst_pheno,
     sites_multi, sites_single, file = file_brks)

## visualization
# write_fig(g, "Figure1_phenofit_curve_fitting.pdf", 10, 6)

# ratio = 1.15
# file <- "Figure5_Phenology_Extraction_temp.pdf"
# cairo_pdf(file, 8*ratio, 6*ratio)
# temp <- get_pheno(fit$fits$ELMORE[2:6], IsPlot = T)
# dev.off()
# file.show(file)
