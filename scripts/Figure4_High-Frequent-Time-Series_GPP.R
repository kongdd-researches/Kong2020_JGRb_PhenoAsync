## multiple seasons
source("test/main_pkgs.R")
source("test/data-prepare/s1_dat0_divide_growing_season.R")
library(ggnewscale)
library(patchwork)
library(metR)

devtools::load_all("../phenofit.R")
devtools::load_all("../rTIMESAT.R")

df_part$date %<>% as_date()
calendarYear = TRUE

## 1. Prepare INPUT data -------------------------------------------------------
{
    df = readRDS("../rfluxnet.R/OUTPUT/fluxsites166_official_SUBSET_DD_raw.RDS")

    dat = df[site == "IT-CA1"]
    sitename = "IT-CA1"
    d_DT = df[site == sitename, .(date, GPP_DT, GPP_DT_QC, RECO_DT, RECO_DT_QC)]
    d_NT = df[site == sitename, .(date, GPP_NT, GPP_NT_QC, RECO_NT, RECO_NT_QC)]
    brks_qc = c(-Inf, 0.3, 0.5, 0.7, Inf)
    d = list(DT = d_DT, NT = d_NT) %>%
        map(~ set_names(.x, c("date", "GPP", "GPP_QC", "RECO", "RECO_QC"))) %>%
        melt_list("type") %>%
        mutate(qc_lev = cut(GPP_QC, brks_qc)) %>%
        .[type == "NT"]
    dat_gpp = d[date >= "2012-01-01", .(t = date, y = GPP, w = GPP_QC)]
    ggplot(dat_gpp[w >= 0, ], aes(t, y)) +
        geom_line()

    dat = df[site == "US-ARM"]
    brks_qc = c(-Inf, 0.3, 0.5, 0.7, Inf)
    brks_year = make_date(2003:2013)
    brks_mid  = c(
        make_date(2003:2013, 4, 30),
        make_date(2003:2013, 7, 31),
        make_date(2003:2013, 10, 31))
    minor_breaks = seq(make_date(2003, 01), make_date(2013, 12), by = "month")

    d_DT = df[site == "US-ARM", .(date, GPP_DT, GPP_DT_QC, RECO_DT, RECO_DT_QC)]
    d_NT = df[site == "US-ARM", .(date, GPP_NT, GPP_NT_QC, RECO_NT, RECO_NT_QC)]
    d = list(DT = d_DT, NT = d_NT) %>%
        map(~set_names(.x, c("date", "GPP", "GPP_QC", "RECO", "RECO_QC"))) %>%
        melt_list("type") %>%
        mutate(qc_lev = cut(GPP_QC, brks_qc)) %>%
        .[type == "DT"]
    dat_gpp = d[date >= "2008-01-01", .(t = date, y = GPP, w = GPP_QC)]

    d_ribbon = make_dt(
        xmin = make_date(2008, 5 , 1), xmax = make_date(2008, 11, 1 ), crop = "Corn",
        xmin = make_date(2008, 11, 1), xmax = make_date(2009, 6 , 10), crop = "Wheat",
        xmin = make_date(2009, 10, 1), xmax = make_date(2010, 6 , 10), crop = "Wheat",
        xmin = make_date(2010, 10, 1), xmax = make_date(2011, 6 , 30), crop = "Canola",
        xmin = make_date(2011, 10, 31), xmax = make_date(2012, 5 , 30), crop = "Wheat"
    ) %>% cbind(I = 1:nrow(.), .)
    d_interface = make_dt(
        x = make_date(2008, 5 , 1), type = "HC & FZ",
        x = make_date(2008, 10, 31), type = "FZ",
        x = make_date(2009,  1, 31), type = "FZ",
        x = make_date(2009,  10, 1), type = "FZ",

        x = make_date(2010,  6, 7), type = "BN",
        x = make_date(2011,  5, 31), type = "HC",
        # x = make_date(2011,  8, 31), type = "TL",
        x = make_date(2011,  10, 31), type = "FZ",
        ncol = 2
    ) %>% set_names(c("date", "oper")) %>%
        merge(d[, .(date, GPP)])
    d_interface$GPP[1] %<>% add(5)
}


l <- with(dat_gpp, check_input(t, y, rep(1, length(y)), nptperyear = 365))
# l <- as.list(dat_gpp)
# l$nptperyear = 365
r = season(l, "smooth_wWHIT",
           wFUN = wBisquare_julia,
           lambda = 3000,
           IsPlot = TRUE)
# with(dat_gpp, check_input()

{
    # load_all("../phenofit.R")
    # with(dat_gpp, lambda_vcurve_jl(y, w, lg_lambda_max = 5))
    # with(dat_gpp, lambda_cv_jl(y, w, lg_lambda_max = 5))
    wBisquare_julia <- function(y, yfit, w, ..., wmin = 0.2,
                                trs_high = 0.3,
                                trs_low  = trs_high,
                                trs_bg = 0.2,
                                .toUpper = TRUE)
    {
        julia_init()
        if (missing(w)) w  <- rep(1, length(y))
        wnew = JuliaCall::julia_call("wBisquare", y, yfit, w,
                                     wmin = wmin,
                                     trs_high = trs_high, trs_low  = trs_low, trs_bg = trs_bg)
        return(wnew)
    }

    r_kong = with(dat_gpp, process_phenofit(y, t, w = rep(1, length(y)),
                                            nptperyear = 365,
        rFUN = "smooth_wSG", frame = 5,
        wFUN = wBisquare_julia,
        lambda = 5000,
        .v_curve = FALSE,
        methods = c("AG", "Zhang", "Beck", "Elmore", "Gu"), #, "AG2"
        iters = 3,
        maxExtendMonth = 2,
        minExtendMonth = 5/30,
        outfile = "a.pdf", overwrite = TRUE,
        run.curvefit = F))
}

# Figure1: growing season division
{
    theme_rm_axisx <- theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
    anno_cropInfo <- function(show.arrow = TRUE) {
        deltaY = 5
        geoms = list(
            geom_arrow(data = d_interface, aes(date, y = GPP + deltaY, dx = 0, dy = -0.8),
                   pivot = 0, arrow.type = "open", size = 0.5, color = "red"),
            geom_text(data = d_interface, aes(date, y = GPP + deltaY, label = oper), color = "red", vjust = 0, size = 3),
            scale_x_date(breaks = brks_year, minor_breaks = minor_breaks,
                         limits = make_date(c(2008, 2013)),
                         expand = c(0, 0)),
            # geom_vline(xintercept = brks_mid, color = "red", linetype = 2, size = 0.2) +
            geom_vline(xintercept = brks_year, color = "yellow3"),
            theme_bw() + theme(
                panel.grid.minor.x = element_line(),
                axis.title.y = element_text(size = 11),
                legend.position = c(1, 1),
                legend.justification = c(1, 1),
                legend.title = element_blank(),
                legend.background = element_rect(fill = "transparent")
            )
        )
        if (show.arrow) geoms else geoms[-(1:2)]
    }
}

## TIMESAT simulations
# load_all("../rTIMESAT.R")
{
    dat_TS = dat_gpp[format(t, "%m-%d") != "02-29"]
    r_TS = TIMESAT_process(dat_TS, 365, half_win = 30)
    r_TS$fit$meth %<>% factor(c("SG", "AG", "DL"))
}

{
    load_all("../phenofit.R")
    r_kong = with(dat_gpp, process_phenofit(y, t, w, nptperyear = 365,
                                            # wFUN = wBisquare_julia,
                                            # lambda = 100,
                                            methods = c("AG", "Zhang", "Beck", "Elmore", "Gu"), #, "AG2"
                                            iters = 3,
                                            maxExtendMonth = 2,
                                            minExtendMonth = 5/30,
                                            .v_curve = TRUE,
                                            outfile = "a.pdf", overwrite = TRUE,
                                            run.curvefit = T))

}

{
    library(gggrid)
    l_col = "grey60"
    d_pheno = r_TS$pheno[meth == "SG"]
    p_TS = ggplot(dat_TS, aes(t, y)) +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax,
                                       group = I, fill = crop),
                  ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        geom_line(color = l_col) +
        geom_line(data = r_TS$fit[meth == "SG"], aes(t, z, color = meth)) +
        scale_color_manual(values = c("black")) +
        # scale_color_manual(values = c("black", "blue", "red"), drop = F, guide = guide_legend(order = 1)) +
        geom_point(data = d_pheno, aes(time_start, val_start), color = "blue") +
        geom_point(data = d_pheno, aes(time_end, val_end), color = "blue") +
        geom_point(data = d_pheno, aes(time_peak, val_peak), color = "red") +
        labs(y = expression("GPP (gC"~m^-2~d^-1*")")) +
        anno_cropInfo(show.arrow = F)
    # p_SG    = plot_rough("smooth_wSG") + theme_rm_axisx
    # p_HANTS = plot_rough("smooth_wHANTS", maxExtendMonth = 4) + theme_rm_axisx

    p_WHIT = plot_rough(r_kong$brks) +
    # p_WHIT = plot_rough("smooth_wWHIT", show.arrow = F) +
        anno_text("(a)"~italic(phenofit), 0.01, 0.95) + xlab("Date") +
        anno_cropInfo() +
        theme_rm_axisx
    # write_fig(p1, "a.pdf", 10, 3*1, show = F)
    ## smooth_whit
    # write_fig(p1 / p_SG / p_HANTS / p_WHIT / p_TS, "GS_dividion_V0_3methods.pdf", 10, 9, show = FALSE)
    # write_fig(p_WHIT / (p_TS + anno_text("(b) TIMESAT", 0.01, 0.95)),
    #           "Figure4_GS-division_High-Frequent-Time-Series_GPP.pdf", 8, 5, show = TRUE)

    base_size = 12
    mytheme = theme(
        axis.text = element_text(size = 12, family = "Times"),
        plot.title = element_text(size = 14, family = "Times"),
        axis.title = element_text(size = 14, family = "Times"),
        # legend.position = "bottom",
        legend.key.size = unit(0.6, 'cm'),
        legend.text = element_text(size = 12, family = "Times"),
        legend.margin =  margin(1, 1, 1, 1),
        legend.title = element_blank(),
        legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1),
        plot.margin = margin(0, 1, -4, 1)*2)

    p_kong = with(tidy_phenofit(r_kong),
                  plot_phenofit2(d_obs, d_season, d_fit)) +
        # ggplot() +
        ggnewscale::new_scale_fill() +
        # anno_cropInfo(show.arrow = FALSE) +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL,
                                       xmin = xmin, xmax = xmax, group = I, fill = crop),
                  ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = T) +
        scale_fill_discrete(guide = guide_legend(order = 2)) +
        guides(shape = "none") +
        mytheme +
        labs(x = "Time", y = expr_GPP,
             color = "Fitting:",
             title = expression("(a)"~italic(phenofit)))
    # write_fig(p_kong)
    d_obs    = dat_gpp
    d_season = r_TS$pheno[meth == 'SG'] %>% rename(flag = season)
    d_fit    = r_TS$fit

    p_TS =
        plot_phenofit2(d_obs, d_season, d_fit) +
        mytheme +
        ggnewscale::new_scale_fill() +
        # anno_cropInfo(show.arrow = FALSE) +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL,
                                       xmin = xmin, xmax = xmax, group = I, fill = crop),
                  ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        # anno_text("(b) TIMESAT", 0.01, 0.95) +
        labs(x = "Time", y = expr_GPP,
             color = "Fitting:",
             title = "(b) TIMESAT")

    write_fig(p_kong / (p_TS),
              "Figure4_CurveFitting_High-Frequent-Time-Series_GPP_V2.pdf",
              9, 6.5, show = TRUE)
}
# plot_season(dat_gpp, r$brks)
# p1 <- ggplot(d, aes(date, GPP)) +
#     geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax,
#                                    group = I, fill = crop),
#               ymin = -Inf, ymax = Inf, alpha = 0.2) +
#     geom_line(color = l_col) +
#     # geom_point(data = d[GPP_QC < 0.7], aes(shape = qc_lev, color = qc_lev), size = 1) +
#     # scale_shape_manual(values = c(4, 8, 16)) +
#     labs(y = expression("GPP (gC"~m^-2~d^-1*")")) +
#     anno_cropInfo() +
#     anno_text("(a) Original time series", 0.01, 0.95) +
#     theme_rm_axisx
#
# ## 2. test about growing season dividing performance
# write_fig({
#     l <- divide_seasons(dat2, 365, is.plot = TRUE, .v_curve = TRUE, iters = 3)
# }, "GS_dividiong.pdf", 10, 3, show = FALSE)

# ggplot(dat, aes(date, P)) + geom_line()
# add_dn(d) %>% group_by(type, year, d8) %>%
#     summarise( across(GPP:RECO_QC, ~mean(.x, na.rm = TRUE)) ) %>%
#     mutate(date = dn2date(year, d8))
