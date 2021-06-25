devtools::load_all("../phenofit.R")

## multiple seasons
source("test/main_pkgs.R")
source("test/data-prepare/s1_dat0_divide_growing_season.R")
library(ggnewscale)
library(patchwork)

df_part$date %<>% as_date()
calendarYear = TRUE

df = readRDS("../rfluxnet.R/OUTPUT/fluxsites166_official_SUBSET_DD_raw.RDS")
{
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
    dat2 = d[date >= "2008-01-01", .(t = date, y = GPP, w = GPP_QC)]

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

## TIMESAT simulations
{
    process_
    d_pheno = r$pheno
    # print(str(r))
}

# Figure1: growing season division
{
    theme_rm_axisx <- theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
    anno_cropInfo <- function(show.arrow = TRUE) {
        geoms = list(
            geom_arrow(data = d_interface, aes(date, y = GPP + 5, dx = 0, dy = -0.8),
                   pivot = 0, arrow.type = "open", size = 0.5, color = "red"),
            geom_text(data = d_interface, aes(date, y = GPP + 5, label = oper), color = "red", vjust = 0, size = 3),
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

{
    l_col = "grey50"
    p1 <- ggplot(d, aes(date, GPP)) +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax,
                                       group = I, fill = crop),
                  ymin = -Inf, ymax = Inf, alpha = 0.2) +
        geom_line(color = l_col) +
        # geom_point(data = d[GPP_QC < 0.7], aes(shape = qc_lev, color = qc_lev), size = 1) +
        # scale_shape_manual(values = c(4, 8, 16)) +
        labs(y = expression("GPP (gC"~m^-2~d^-1*")")) +
        anno_cropInfo() +
        anno_text("(a) Original time series", 0.01, 0.95) +
        theme_rm_axisx

    p_TS = ggplot(dat_TS, aes(t, y)) +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax,
                                       group = I, fill = crop),
                  ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        geom_line(color = "grey50") +
        geom_line(data = cbind(yfit = r$fit[, 2], dat2), aes(t, yfit), color = "purple") +
        geom_point(data = d_pheno, aes(time_start, val_start), color = "blue") +
        geom_point(data = d_pheno, aes(time_end, val_end), color = "blue") +
        geom_point(data = d_pheno, aes(time_peak, val_peak), color = "red") +
        labs(y = expression("GPP (gC"~m^-2~d^-1*")")) +
        anno_cropInfo(show.arrow = F) +
        theme_rm_axisx
    # p_SG    = plot_rough("smooth_wSG") + theme_rm_axisx
    # p_HANTS = plot_rough("smooth_wHANTS", maxExtendMonth = 4) + theme_rm_axisx
    p_WHIT  = plot_rough("smooth_wWHIT", show.arrow = F) +
        anno_text("(c)"~italic(phenofit), 0.01, 0.95) + xlab("Date")
    # write_fig(p1, "a.pdf", 10, 3*1, show = F)
    ## smooth_whit
    # write_fig(p1 / p_SG / p_HANTS / p_WHIT / p_TS, "GS_dividion_V0_3methods.pdf", 10, 9, show = FALSE)
    write_fig(p1 / (p_TS + anno_text("(b) TIMESAT", 0.01, 0.95)) / p_WHIT ,
              "Figure1_GS_dividion_final.pdf", 8, 5, show = FALSE)
}

# ## 2. test about growing season dividing performance
# write_fig({
#     l <- divide_seasons(dat2, 365, is.plot = TRUE, .v_curve = TRUE, iters = 3)
# }, "GS_dividiong.pdf", 10, 3, show = FALSE)

# ggplot(dat, aes(date, P)) +
#     geom_line()
# add_dn(d) %>% group_by(type, year, d8) %>%
#     summarise( across(GPP:RECO_QC, ~mean(.x, na.rm = TRUE)) ) %>%
#     mutate(date = dn2date(year, d8))

if (0) {
    # InitCluster(12)
    sites2 = c(sites_multi, sites_rm) %>% set_names(., .)
    sites_single = sites_single %>% set_names(., .)
    info = info_full

    nptperyear = 365
    varname = "GPP_NT"
    version = glue("({varname})v0.2.9.9000") # test version

    ## 1. divide growing seasons
    lst_brks.multi = main_divide_season(df_part, info,
        sites_multi = sites2, sites_single,
        calendarYear = FALSE, varname = varname,
        outfile = glue("Figure/phenofit_seasons_multi_{version}.pdf")
    )

    # if calendarYear then single
    lst_brks.single = main_divide_season(df_part, info,
        sites_multi = sites2, sites_single,
        calendarYear = TRUE, varname = varname,
        outfile = glue("Figure/phenofit_seasons_single_{version}.pdf")
    )

    ## 3. curve fitting and get phenology metrics
    lst_brks = c(lst_brks.single, lst_brks.multi)
    TRSs = c(0.1, 0.2, 0.5, 0.6, 0.8, 0.9)

    InitCluster(10, kill = FALSE)
    lst_pheno = main_phenofit(lst_brks,
        TRS = TRSs,
        show = FALSE, outfile = glue("Figure/gpp_phenofit_pheno_{version}.pdf")
    )
    # lst_pheno2 = main_phenofit(lst_brks[86:length(lst_brks)], TRS = TRSs,
    #     show = TRUE, outfile = glue("Figure/gpp_phenofit_pheno_{version}2.pdf"))

    outfile = glue("INPUT/pheno_gpp_st109 {version}.rda")
    save(lst_brks.single, lst_brks.multi, lst_brks, lst_pheno,
        sites_multi, sites_single,
        file = outfile
    )
}

## visualization
# write_fig(g, "Figure1_phenofit_curve_fitting.pdf", 10, 6)
# ratio = 1.15
# file <- "Figure5_Phenology_Extraction_temp.pdf"
# cairo_pdf(file, 8*ratio, 6*ratio)
# temp <- get_pheno(fit$fits$ELMORE[2:6], IsPlot = T)
# dev.off()
# file.show(file)
