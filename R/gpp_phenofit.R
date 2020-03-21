# source("test/main_phenofit.R")
#' @importFrom phenofit check_season curvefits get_pheno get_fitting get_GOF
#' plot_phenofit
#' @export
tidy_seasons <- function(l, rtrough_max = 0.6, r_min = 0.1, ypeak_min = 1, show = TRUE) {
    INPUT <- l$INPUT
    brks <- l$brks
    titlestr <- l$titlestr

    # year_grps <- names(brks)
    lst_dt <- foreach(dt = brks$dt) %do% {
        dt <- data.frame(dt) %>% data.table()
        check_season(dt, rtrough_max = rtrough_max, r_min = r_min)
        dt <- dt[y_peak != -9999.0 & (len > 45 & len < 650), ]
        dt
    }
    dt <- do.call(rbind, lst_dt)
    check_season(dt, rtrough_max = rtrough_max, r_min = r_min)
    # dt <- dt[y_peak != -9999.0 & (len > 45 & len < 650), ]
    dt <- dt[y_peak >= ypeak_min & (len > 45 & len < 650), ]

    brks$dt <- dt
    if (show) plot_season(INPUT, brks, title = titlestr, show.legend = FALSE)
    dt
}

#' @export
main_phenofit <- function(lst_brks, TRS = c(0.1, 0.2, 0.5, 0.6, 0.8),
                          show = TRUE,
                          verbose = TRUE,
                          outfile) {
    if (show) {
        Cairo::CairoPDF(outfile, 10, 8)
        par(mfrow = c(5, 1), oma = c(3, 1, 1, 1) / 4)
        on.exit({ dev.off(); SumatraPDF(outfile) })
    }

    lst_pheno <- foreach(l = lst_brks, i = icount()) %dopar% {
        runningId(i)
        first.fig = i == 1
        fit <- curvefits(l$INPUT, l$brks,
            methods = c("AG", "Zhang", "Beck", "Elmore"), # ,"klos",, 'Gu'
            wFUN = wBisquare,
            nextend = 2, maxExtendMonth = 3, minExtendMonth = 1, minPercValid = 0.2,
            print = verbose, verbose = FALSE
        )

        ## check the curve fitting parameters
        l_param <- get_param(fit)
        # print(str(l_param, 1))
        # print(l_param$AG)

        d_fit <- get_fitting(fit)
        ## Get GOF information
        d_gof <- get_GOF(fit)
        # fit$stat <- stat
        # print(head(d_gof))
        # print(fit$fits$AG$`2002_1`$ws)
        # print(fit$`2002_1`$fFIT$AG$ws)

        # theme = coord_cartesian(xlim = c(ymd("2000-04-01"), ymd("2017-07-31")))
        # write_fig(expression({
        if (show) {
            g <- plot_phenofit(d_fit, l$brks, l$titlestr,
                title.ylab = "NDVI", "Time",
                shape = "point", cex = 0.4,
                theme = NULL
            )
            if (!first.fig) grid::grid.newpage()
            grid::grid.draw(g) # plot to check the curve fitting
        }
        ## 2.5 Extract phenology
        l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = F) # %>% map(~melt_list(., "meth"))
        l_pheno
    }
    lst_pheno
}

main_divide_season <- function(df_part, info, sites_multi, sites_single,
                               ypeak_min = 1,
                               calendarYear = FALSE,
                               show = TRUE,
                               outfile = "phenofit_multi_seasons_v4.pdf") {
    sites <- if (calendarYear) sites_single else sites_multi

    ## 1. get all peaks
    lst <- foreach(sitename = sites, i = icount()) %do% {
        # browser()
        runningId(i)
        # sitename = "IT-Ro1"
        # sitename = "DE-Kli"
        sp <- info[site == sitename, ]
        d <- df_part[site == sitename, .(site, t = date, y = GPP_DT, w = 1 - is.na(GPP_DT))] # GPP_NT,
        # d <- df_part[site == sitename, .(site, t = date, y = GPP_NT, w = 1 - is.na(GPP_NT))] # GPP_NT,

        tryCatch(
            {
                l <- divide_seasons(d, sp, 365,
                    # lambda = 100, optim by v_curve
                    # .movmean = TRUE,
                    iters = 1,
                    # wFUN = wBisquare,
                    .v_curve = TRUE,
                    r_min = 0.0, r_max = 0.2,
                    calendarYear = calendarYear,
                    rm.closed = FALSE, is.continuous = FALSE,
                    .check_season = FALSE
                )
                l$titlestr <- with(sp[1, ], sprintf(
                    "%s, %s, [%.2f, %.2f] lambda = %.1f",
                    site, IGBP, lon, lat, l$lambda
                )) # %dth, ID
                l
                # write_fig(expression({with(l, plot_season(INPUT, brks, title = sitename))}), "a.pdf", 10, 4)
            },
            error = function(e) {
                message(sprintf("[e] %d %s: %s", i, sitename, e$message))
            }
        )
    }

    if (show) {
        Cairo::CairoPDF(outfile, 10, 8)
        par(mfrow = c(5, 1), oma = c(3, 1, 1, 1) / 4)
        on.exit({ dev.off(); SumatraPDF(outfile) })
    }

    grps <- seq_along(lst) # [1:10]
    # grps = 14
    for (i in grps) {
        Ipaper::runningId(i)
        l <- lst[[i]]
        if (calendarYear) {
            brks <- l$brks
            if (show) plot_season(l$INPUT, brks, title = l$titlestr, show.legend = FALSE)
        } else {
            rtrough_max <- 0.5
            if (sites[i] == "IT-Ro2") rtrough_max <- 0.4
            lst[[i]]$brks$dt <- tidy_seasons(l, rtrough_max = rtrough_max, r_min = 0.1, ypeak_min = ypeak_min, show = show)
        }
    }
    lst
}

#' get_fitting2
#'
#' @param l list with the element of c(`brks`, `fit`, `pheno`)
#' @export
get_fitting2 <- function(l){
    x <- rbind(l$brks$whit[, .(t, y, ziter1, ziter2, meth = "whit")],
               l$dfit[, .(t, y, ziter1, ziter2, meth)])
    y = dcast(unique(x)[, -3], t+y~meth, value.var = "ziter2", fun=mean, na.rm = TRUE)
    y
}
