#' plot_TIMESAT
#' @importFrom lubridate make_date
#' @import ggplot2
#' @export
plot_TIMESAT <- function(d_obs, r, base_size = 12) {
    d_pheno  = r$pheno[meth == "SG"]
    date_begin = d_obs$t %>% first() %>% {make_date(year(.), 1, 1)}
    date_end   = d_obs$t %>% last() %>% {make_date(year(.), 12, 31)}
    brks_year = seq(date_begin, date_end, by = "year")

    # d_obs <- listk(t, y, w, QC_flag) %>% as.data.table()
    if (!("QC_flag" %in% colnames(d_obs))) {
        d_obs %<>% mutate(QC_flag = ifelse(w >= 0.5, "good", "cloud"))
    }
    
    qc_name = "QC Flag:"
    I_qc = 1:4
    r$fit$meth %<>% factor(c("SG", "AG", "DL"))

    ggplot(d_obs, aes(t, y)) +
        # geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, group = I, fill = crop),
        #     ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        geom_rect(data = d_pheno, aes(x = NULL, y = NULL, xmin = time_start, xmax = time_end, group = season),
            ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F, linetype = 1,
            fill = alpha("grey", 0.2),
            color = alpha("grey", 0.4)) +
        geom_point(aes(color = QC_flag, shape = QC_flag, fill = QC_flag)) + 
        scale_color_manual(values = qc_colors[I_qc], drop = F, guide = guide_legend(order = 1)) +
        scale_fill_manual(values = qc_colors[I_qc], drop = F, guide = guide_legend(order = 1)) +
        scale_shape_manual(values = qc_shapes[I_qc], drop = F, guide = guide_legend(order = 1)) + 
        labs(color = qc_name, fill = qc_name, shape = qc_name) + 
        guides(shape = guide_legend(override.aes = list(size=3), order = 1)) + 
        ggnewscale::new_scale_color() + 
        # geom_line(color = "black", size = 0.4) +
        geom_line(data = r$fit, aes(t, z, color = meth)) +
        geom_point(data = d_pheno, aes(time_start, val_start), color = "blue") +
        geom_point(data = d_pheno, aes(time_end, val_end), color = "blue") +
        geom_point(data = d_pheno, aes(time_peak, val_peak), color = "red") +
        geom_vline(xintercept = brks_year, color = "yellow3") +
        theme_bw(base_size = base_size) +
        theme(
            axis.text = element_text(color = "black"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linetype = "dashed", size = 0.2)
        ) +
        scale_color_manual(values = c("black", "blue", "red"), drop = F, guide = guide_legend(order = 2)) +
        scale_x_date(limits = c(date_begin, date_end), expand = c(0, 0)) 
}

