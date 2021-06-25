plot_rough <- function(rFUN = "smooth_wSG", show.arrow = FALSE, ...) {
    l <- divide_seasons(dat2, 365, is.plot = TRUE,
                        rFUN = rFUN,
                        .v_curve = TRUE, iters = 3, ...)
    d_rough = l$brks$whit %>% dplyr::select(t, y, starts_with("ziter")) %>%
        melt(c("t", "y"), variable.name = "ziter")

    p <- ggplot(d, aes(date, GPP)) +
        # geom_point() +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax,
                                       group = I, fill = crop),
                  ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        geom_line(color = "grey60") + # aes(color = cut(GPP_QC, brks_qc), group = 1)
        # geom_point(data = d[GPP_QC < 0.7], aes(shape = qc_lev, color = qc_lev), size = 1) +
        # facet_wrap(~type, nrow = 2) +
        scale_shape_manual(values = c(4, 8, 16)) +
        geom_vline(xintercept = brks_year, color = "yellow3") +
        # geom_vline(xintercept = brks_mid, color = "red", linetype = 2, size = 0.2) +
        anno_cropInfo(show.arrow = show.arrow) +
        new_scale_color() +
        geom_line(data = d_rough,aes(t, value, color = ziter)) +
        scale_color_manual(values = c("blue", "green", "red")) +
        geom_point(data = l$brks$dt, aes(beg, y_beg), color = "blue") +
        geom_point(data = l$brks$dt, aes(end, y_end), color = "blue") +
        geom_point(data = l$brks$dt, aes(peak, y_peak), color = "red") +
        labs(y = expression("GPP (gC"~m^-2~d^-1*")"))
    p
}
