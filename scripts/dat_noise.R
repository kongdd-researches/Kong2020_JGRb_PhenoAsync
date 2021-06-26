# source("test/main_pkgs.R")
library(lubridate)
library(rTIMESAT)
library(phenofit)
library(ggplot2)
devtools::load_all()
# load_all("/mnt/i/Research/phenology/rTIMESAT.R")
# devtools::load_all("../phenofit.R")
# devtools::load_all("/mnt/i/Research/phenology/rTIMESAT.R")

simu_VI <- function(SOS = 50, EOS = 100, rate = 0.1, mx = 0.6, year = 2010, wmin = 0.2) {
    par <- c(0.1, mx, SOS, rate, EOS, rate)

    t <- seq(1, 365, 8)
    w <- rep(1, length(t))

    noise <- rnorm(n = length(t), mean = 0, sd = 0.05)
    I_noise <- noise < 0
    noise[!I_noise] <- 0
    w[I_noise] <- wmin
    y0 <- doubleLog.Beck(par, t)
    y  <- y0 + noise
    data.table(year, doy = t, t = as.Date(sprintf("%d%03d", year, t), "%Y%j"),
               y, y0, w)
}

{
    set.seed(0)
    d1_a <- simu_VI(150, 250, 0.1, year = 2010)
    d1_b <- simu_VI(150, 250, 0.15, year = 2011)

    # two growing season
    d2_1 <- simu_VI(50, 120, 0.05, year = 2012)
    d2_2 <- simu_VI(180, 250, 0.1, year = 2012)
    d2_a = rbind(d2_1[doy < 150, ], d2_2[doy >= 150, ])

    d2_1 <- simu_VI(50, 120, 0.1, year = 2013)
    d2_2 <- simu_VI(180, 250, 0.05, year = 2013)
    d2_b = rbind(d2_1[doy < 150, ], d2_2[doy >= 150, ])

    dat = rbind(d1_a, d1_b, d2_a, d2_b)
    # dat$w %<>% as.factor()

    ggplot(dat, aes(t, y)) +
        geom_line(aes(y = y0), color = "black") +
        geom_line(aes(y = y), color = "green")
        # geom_point(aes(color = w, shape = w))
}

# floor(45/7)
{
    nptperyear = 46
    r = TIMESAT_process(dat, nptperyear, half_win = 6, p_trs = 0.02)
    p_TS = TIMESAT_plot(dat, r)
    p_TS
}

r = phenofit_process(dat$y, dat$t, dat$w, nptperyear = nptperyear, title = "a.pdf")
phenofit_plot(r)
# l <- divide_seasons(dat, 46, is.plot = TRUE, maxExtendMonth = 2)
# l_TSM <- divide_seasons(dat, 46, iters = 3, is.plot = TRUE,
#                     wFUN = wTSM,
#                     # wFUN = wBisquare_julia,
#                     lambda = 10,
#                     .v_curve = FALSE,
#                     show.legend = F)
#
# l_bisquare <- divide_seasons(dat, 46, iters = 3, is.plot = TRUE,
#                         # wFUN = wTSM,
#                         wFUN = wBisquare_julia,
#                         lambda = 10,
#                         .v_curve = FALSE,
#                         show.legend = F)
