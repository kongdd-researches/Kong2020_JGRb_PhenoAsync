devtools::load_all("../phenofit.R")

## multiple seasons
source("test/main_pkgs.R")
source("test/data-prepare/s1_dat0_divide_growing_season.R")

satellite = "MOD09A1"
df = tidy_modis_09A1(satellite)
df[, dhour2 := solartime::computeDayLength(date, lat)]
df[, dhour_norm := (dhour2/max(dhour2))^2, .(site)]
df[, EVI_pc := dhour_norm * EVI] # Bauerlea

# & group == 5
sitename = "US-ARM"; grp = 5
d = df[site == sitename & date >= "2008-01-01" & date <= "2012-12-31" & group == grp,
       .(t, y = EVI_pc, QC, QC_flag, w)]
d[is.na(y), y := -0.05]
# d = df[site == sitename & group == j, ]
{
    anno_cropInfo2 <- function(show.arrow = TRUE) {
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

    p_gpp = ggplot(dat2, aes(t, y)) +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax,
                                       group = I, fill = crop),
                  ymin = -Inf, ymax = Inf, alpha = 0.2) +
        geom_line(color = "grey50") +
        scale_x_date(limits = make_date(c(2008, 2013)), expand = c(0, 0)) +
        scale_y_continuous(limits = c(-4, 17)) +
        anno_cropInfo2()
    p_vi <- ggplot(d, aes(t, y)) +
        geom_line(color = "red") +
        geom_point(aes(color = QC_flag, shape = QC_flag), size = 0.8) +
        scale_shape_manual(values = phenofit:::qc_shapes) +
        scale_color_manual(values = phenofit:::qc_colors) +
        scale_x_date(limits = make_date(c(2008, 2013)), expand = c(0, 0)) +
        labs(y = "EVI")

    g = ggplot_multiAxis(p_gpp, p_vi, show = FALSE)
    write_fig(g, "Figure2_EVI.pdf", 10, 5, show = F)
}



g_phenofit = as.grob(function(){
    l <- divide_seasons(d, 46, iters = 3, is.plot = TRUE, show.legend = F)
})
write_fig(g_phenofit, "Figure2_GS_division_EVI.pdf")

write_fig({
    l <- divide_seasons(dat, 46, iters = 3, is.plot = TRUE,
                        wFUN = wTSM,
                        # wFUN = wBisquare_julia,
                        lambda = 10,
                        .v_curve = FALSE,
                        show.legend = F)
}, "Figure2_GS_division_EVI_phenofit.pdf")

{
    # load_all("/mnt/i/Research/phenology/rTIMESAT.R")
    r_TS <- TIMESAT_process(dat, 46, p_trs = 0.05, half_win = 10)
    g_TS <- TIMESAT_plot(d, r_TS, base_size = 14)
    g = plot_grid(g_phenofit, g_TS, ncol = 1)
    write_fig(g, "Figure2_GS_division_EVI.pdf")
}

library(ggplotify)
library(cowplot)
library(patchwork)

d = data.table(t, y, w = as.factor(w))
