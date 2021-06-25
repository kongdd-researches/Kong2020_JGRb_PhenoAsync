# devtools::load_all("/mnt/i/Research/phenology/rTIMESAT.R")
#' @param d a data.frame with the columns of `[]`
TIMESAT_process <- function(d, nptperyear = 365, p_trs = 0.1) {
    options <- list(
        ylu = c(0, 9999), # Valid data range (lower upper)
        qc_1 = c(0.0, 0.2, 0.2), # Quality range 1 and weight
        qc_2 = c(0.2, 0.5, 0.5), # Quality range 2 and weight
        qc_3 = c(0.5, 1.0, 1), # Quality range 3 and weight
        A = 0.1, # Amplitude cutoff value
        output_type = c(1, 1, 0), # Output files (1/0 1/0 1/0), 1: seasonality data; 2: smoothed time-series; 3: original time-series
        seasonpar = 0.2, # Seasonality parameter (0-1)
        iters = 2, # No. of envelope iterations (3/2/1)
        FUN = 1, # Fitting method (1/2/3): (SG/AG/DL)
        half_win = floor(nptperyear / 5 * 2) + 1, # half Window size for Sav-Gol.
        meth_pheno = 1, # (1: seasonal amplitude, 2: absolute value, 3: relative amplitude, 4: STL trend)
        trs = c(1, 1) * p_trs # Season start / end values
    )
    
    # data("MOD13A1")
    sitename <- "rTS"
    # sitename <- "CA-NS6"
    # d <- subset(MOD13A1$dt, date >= as.Date("2004-01-01") & date <= as.Date("2010-12-31") & site == sitename)
    dat = d[format(t, "%m-%d") != "02-29"]
    r <- TSF_main(
        y = dat$y, qc = dat$w, nptperyear,
        jobname = sitename, options, cache = FALSE,
        dat$t
    )
    r
}

TIMESAT_plot <- function(d, r) {
    d_pheno  = r$pheno
    ggplot(d, aes(t, y)) +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, group = I, fill = crop),
            ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        geom_line(color = "grey50") +
        geom_line(data = cbind(yfit = r$fit[, 2], d[, .(t)]), aes(t, yfit), color = "purple") +
        geom_point(data = d_pheno, aes(time_start, val_start), color = "blue") +
        geom_point(data = d_pheno, aes(time_end, val_end), color = "blue") +
        geom_point(data = d_pheno, aes(time_peak, val_peak), color = "red")
}
