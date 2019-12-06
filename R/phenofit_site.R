#' Extract Vegetation Phenology at site scale
#'
#' @inheritParams phenofit::check_input
#' @inheritParams phenofit::curvefits
#' @param ... other parameters to [phenofit::curvefits()]
#'
#' @export
phenofit_site <- function(y, t, w, QC_flag, nptperyear = 36,
    brks = NULL,
    wFUN = wTSM, 
    fineFit = TRUE, 
    TRS = c(0.1, 0.2, 0.5, 0.6, 0.8), 
    maxExtendMonth = 1.5, minExtendMonth = 0.5,
    minPercValid = 0,
    verbose = TRUE,
    lambda = NULL, lg_lambdas = seq(1, 4, 0.1),
    methods = c("AG", "Zhang", "Beck", "Elmore"),
    prefix = "", titlestr = NULL,
    IsPlot.brks = FALSE,
    write.fig = TRUE, show = FALSE,
    ymin = 0.1, wmin = 0.1,
    wsnow = 0.8,
    use.y0 = FALSE, ...)
{
    # Parameters
    # lambda   <- 5    # non-parameter Whittaker, only suit for 16-day. Other time-scale should assign a lambda.
    # nptperyear <- 36   # How many points for a single year
    ymax_min   <- 0.1  # the maximum ymax shoud be greater than `ymax_min`
    rymin_less <- 0.8  # trough < ymin + A*rymin_less
     #wTSM #wBisquare # Weights updating function, could be one of `wTSM`, 'wBisquare', `wChen` and `wSELF`.

    ## 2.1 load site data
    south      = FALSE
    print      = FALSE # whether print progress
      # for brks

    ## 2.2 Check input data
    d <- data.table(y, t, date = t, w, QC_flag)

    dnew  <- add_HeadTail(d, south, nptperyear = nptperyear) # add additional one year in head and tail
    INPUT <- check_input(dnew$t, dnew$y, dnew$w, dnew$QC_flag,
                         nptperyear, south,
                         maxgap = nptperyear/4, alpha = 0.02,
                         ymin = ymin, wmin = wmin, wsnow = wsnow)

    ## 2.3 Divide growing seasons
    if (is.null(lambda)) lambda <- v_curve(INPUT, lg_lambdas)$lambda
    # print(lambda)
        brks2 <- season_mov(INPUT,
                       FUN = smooth_wWHIT, wFUN = wFUN,
                       maxExtendMonth = 3, 
                       # minExtendMonth = minExtendMonth,
                       wmin = wmin,
                       IsOptim_lambda = TRUE,
                       lambda = lambda,
                       r_min = 0.1,
                       IsPlot = IsPlot.brks, IsPlot.OnlyBad = FALSE, print = FALSE, ...)
      if (!is.null(brks)) brks2$dt = brks$dt
    # }
    # plot_season(INPUT, brks2)
    # brks

    d_fit <- pheno <- NULL
    if (fineFit) {
        ## 2.4 Curve fitting
        fit  <- curvefits(INPUT, brks2,
                      methods = methods, #,"klos",, 'Gu'
                      wFUN = wFUN,
                      iters = 2,
                      nextend = 2,
                      wmin = wmin,
                      maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                      minPercValid = minPercValid,
                      print = verbose, verbose = FALSE,
                      use.y0 = use.y0,
                      ...)

        ## check the curve fitting parameters
        l_param <- get_param(fit)
        d_fit   <- get_fitting(fit)
        # d_gof   <- get_GOF(fit)

        ## visualization
        if (write.fig) {
            file_pdf = glue("Figure/{prefix}{titlestr}.pdf")
            check_dir(dirname(file_pdf))

            g <- plot_phenofit(d_fit, brks2, titlestr)
            # grid::grid.newpage(); grid::grid.draw(g)# plot to check the curve fitting
            write_fig(g, file_pdf, 11, 6, show = show)
        }

        ## 2.5 Extract phenology
        l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = F) #%>% map(~melt_list(., "meth"))
        pheno <- l_pheno$doy %>% melt_list("meth")
    }
   
    list(brks = brks2, fit = d_fit, pheno = pheno)
}


phenofit_site_3by3 <- function(df, sitename){
    ws = seq(0.1, 1, 0.05) %>% set_names(., .)

    lst_fit = foreach(wmin = ws, k = icount()) %do% {
        runningId(k)
        d = df[site == sitename & group == j, ]
        d[QC_flag == "aerosol", w := wmin]
        l = phenofit_site(d$y, d$t, d$w, d$QC_flag, nptperyear = 92,
                          .check_season = TRUE,
                          titlestr = title, show = FALSE)
        l
    }

    d_gof <- map(lst_fit, function(l) {
        d2 = merge(l$fit, d_gpp)
        d2[QC_flag == "good", as.list(GOF(GPP_DT, ziter2, include.r = TRUE)), .(meth)]
    }) %>% melt_list("wmin")

    d_good  = lst_fit$`0.1`$fit[QC_flag != "aerosol",]
    d_other = lst_fit$`1`$fit[QC_flag == "aerosol",]
    d_fit = rbind(d_good, d_other) %>% .[order(t, meth)]

    # d_gof$wmin %<>% as.numeric()
    # ggplot(d_gof, aes(wmin, R2, color = meth))+ geom_point() + geom_line() +
    #     # geom_hline(yintercept = 0.81) +
    #     labs(x = "wmin (good)")
}
