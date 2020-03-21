## multiple seasons
source("test/main_pkgs.R")
source("test/async/s1_dat0_divide_growing_season.R")

calendarYear = TRUE

# InitCluster(12)
sites2 = c(sites_multi, sites_rm) %>% set_names(., .)
sites_single = sites_single %>% set_names(., .)
info  = info_full

nptperyear = 365
version = "v0.9002" # test version

## 1. divide growing seasons
lst_brks.multi  = main_divide_season(df_part, info, sites_multi = sites2, sites_single,
    calendarYear = FALSE,
    outfile = glue("Figure/phenofit_seasons_multi_{version}.pdf"))

# if calendarYear then single
lst_brks.single = main_divide_season(df_part, info, sites_multi = sites2, sites_single,
    calendarYear = TRUE,
    outfile = glue("Figure/phenofit_seasons_single_{version}.pdf"))

## 3. curve fitting and get phenology metrics
lst_brks  = c(lst_brks.single, lst_brks.multi)
TRSs = c(0.1, 0.2, 0.5, 0.6, 0.8, 0.9)

InitCluster(10)
lst_pheno = main_phenofit(lst_brks, TRS = TRSs,
    show = FALSE, outfile = glue("Figure/gpp_phenofit_pheno_{version}.pdf"))
# lst_pheno2 = main_phenofit(lst_brks[86:length(lst_brks)], TRS = TRSs,
#     show = TRUE, outfile = glue("Figure/gpp_phenofit_pheno_{version}2.pdf"))

outfile = "INPUT/pheno_gpp_st109 (GPP_DT) {version}.rda"
saveRDS(lst_brks.single, lst_brks.multi, lst_brks, lst_pheno,
     sites_multi, sites_single, file = outfile)

## reproduce those figures
{
    load("INPUT/pheno_gpp_st109 (GPP_DT).rda")
    lst_brks.dt1 = lst_brks

    load("INPUT/pheno_gpp_st109 (GPP_DT)-backup.rda")
    lst_brks.dt0 = lst_brks

    ind = 1
    # all.equal(lst_brks.dt0[ind], lst_brks.dt[ind])

    ## check the version difference

    l0 = lst_brks.dt0$`DE-Seh`$INPUT
    l1 = lst_brks.dt1$`DE-Seh`$INPUT
}

date = l0$t
d_diff <- listk(l0, l1) %>% map("y0") %>% as.data.table() %>% cbind(date, .) %>%
    plyr::mutate(diff = l1-l0, I = 1:length(l1)) %>% .[diff != 0, ]

sitename = "DE-Seh"
d = df_part[site == sitename, .(site, t = date, y = GPP_DT, w = 1 - is.na(GPP_DT))]


south <- FALSE
nptperyear = 365
dnew  <- add_HeadTail(d, south = south, nptperyear)

browser()
{
    INPUT <- check_input(dnew$t, dnew$y, dnew$w, QC_flag = NULL, nptperyear,
                         maxgap = ceiling(nptperyear/12*1.5),
                         south = south,
                         date_start = d$t[1],
                         date_end = last(d$t))
    plot_input(INPUT)
}

plot_input(l1)
plot_input(l0)

## visualization
# write_fig(g, "Figure1_phenofit_curve_fitting.pdf", 10, 6)
# ratio = 1.15
# file <- "Figure5_Phenology_Extraction_temp.pdf"
# cairo_pdf(file, 8*ratio, 6*ratio)
# temp <- get_pheno(fit$fits$ELMORE[2:6], IsPlot = T)
# dev.off()
# file.show(file)
