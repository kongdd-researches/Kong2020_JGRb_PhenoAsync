#' Get GPPobs phenology dat
#' @examples
#' get_phenofit(df, st, brks_lst, sites, wFUN = 'wTSM')
get_phenofit <- function(sitename,
    df, st, brks_lst, sites, wFUN = 'wTSM'){
    # sitename <- sites[i]
    i <- grep(sitename, sites)
    brks2 <- brks_lst[[i]]

    ## 1. prepare inputs
    d   <- df[site == sitename, .(t = date, GPP_DT, GPP_NT, w = 1)] #%T>% plotdata(365)
    d$y <- rowMeans(d[, .(GPP_DT, GPP_NT)], na.rm = T)
    d[y < 0, y := 0] # for GPP_NT

    sp      <- st[site == sitename, ]

    # parameters for season_3y
    INPUT <- get_input(df, st, sitename)
    
    ## 3. Get daily curve fitting result
    wFUN <- get(wFUN)
    fit  <- curvefits(INPUT, brks2,
                      methods = c("AG", "zhang", "beck", "elmore"), #,"klos",, 'Gu'
                      debug = F,
                      wFUN = wFUN,
                      nextent = 5, maxExtendMonth = 2, minExtendMonth = 1/3,
                      # qc = as.numeric(dnew$SummaryQA), 
                      minPercValid = 0.2,
                      print = print)
    fit$INPUT   <- INPUT
    fit$seasons <- brks2

    ## check the curve fitting parameters
    params <- getparam(fit)
    # print(str(params, 1))
    # print(params$AG)

    ## Get GOF information
    stat  <- ldply(fit$fits, function(fits_meth){
        ldply(fits_meth, statistic.phenofit, .id = "flag")
    }, .id = "meth")
    fit$stat <- stat
    print(head(stat))

    # pheno: list(p_date, p_doy)
    p <- lapply(fit$fits, ExtractPheno)
    pheno  <- map(p, tidyFitPheno, origin = INPUT$t[1]) %>% purrr::transpose()
    fit$pheno  <- pheno

    # print(fit$fits$AG$`2002_1`$ws)
    ## visualization
    # svg("Figure1_phenofit_curve_fitting.svg", 11, 7)
    # Cairo::CairoPDF(file_pdf, 11, 6) #
    # dev.off()
    g <- plot_phenofit(fit, d, INPUT$titlestr)
    grid::grid.newpage(); grid::grid.draw(g)# plot to check the curve fitting
    return(fit)
}
