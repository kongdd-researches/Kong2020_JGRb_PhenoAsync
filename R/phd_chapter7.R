#' @import broom
pheno_impact <- function(d1, outfile = NULL, devices = c("pdf", "jpg")) {
    d1$metric %<>% factor(c("TRS1.sos", "TRS1.eos", "TRS1.los"), c("生长季开始时间", "生长季结束时间", "生长季长度") %>% char2expr())
    d1$variable %<>% factor(
        c("GPP_DT", "GPP_NT", "NEE", "LE_CORR", "H_CORR"),
        c(expression("总初级生产力 ("*gC~m^-2~a^-1*")"),
          expression("总初级生产力 ("*gC~m^-2~a^-1*")"),
          expression("净初级生产力 ("*gC~m^-2~a^-1*")"),
          expression("蒸散发 (" * mm~a^-1*")"),
          expression("感热 (" * MJ~m^-2~a^-1*")")) %>% char2expr()
    )
    d1[, value2 := mark_outlier(value), .(site, LC, variable, metric)]

    if (!is.null(outfile)) {
        fontsize <- 20
        theme_set(theme_gray(base_size = fontsize, base_family = "TimesSimSun"))
        p <- ggplot(d1[value > -1e4], aes(pheno, value2, color = LC, shape = LC)) +
            facet_grid(variable ~ metric, scales = "free",
                    labeller = label_parsed) +
            geom_vline(xintercept = 0, color = "grey60") +
            geom_hline(yintercept = 0, color = "grey60") +
            geom_point(alpha = 0.9) +
            geom_smooth(method = "lm") +
            stat_fit_tidy(method = "lm",
                        label.x = c(rep(0.98, 2), rep(0.02, 3)),
                        label.y = c(0.02, 0.12, 0.22, 0.85, 0.95) %>% rev(),
                        size = 7,
                        # label.y = 0.02, geom = "label_npc",
                        label.size = NA,
                        method.args = list(formula = y ~ x), parse = TRUE,
                        mapping = aes(label = sprintf('Slope~"="~"%+5s"*", p="~"%0.2f"', # ~ mm ~ a^-1 ~ d^-1
                                                        sprintf("%.2f", stat(x_estimate)),
                                                        stat(x_p.value)))) +
            theme(legend.position = "bottom") +
            labs(y = NULL, x = "物候变化天数", color = "植被类型", shape = "植被类型") +
            coord_cartesian(xlim = c(-45, 40))
        p <- facet_tag(p, size = 8) +
            theme(
                strip.text = element_text(size=fontsize-1),
                strip.background = element_blank(),
                plot.margin = margin(-5, 0, 0, 0),
                legend.box.margin = margin(-5, 0, 0, 0),
                legend.text = element_text(size = 18),
                axis.text = element_text(color = "black", size = 17),
                axis.title.x = element_text(margin = margin(0.2, 0, -0.3, 0, "cm"))
                )
        write_fig(p, outfile, 15, 14, use.cairo_pdf = TRUE, devices = devices)
    }

    # 计算tbl
    tbl <- ddply(d1, .(variable, metric, LC), function(d) {
        m <- lm(value2~pheno, d)
        tidy(m)[-1, c(2, 5)]
    })

    slope <- data.table(tbl) %>% dcast(metric + variable ~ LC, value.var = "estimate") %>% .[order(metric)]
    slope[, 3:7] %<>% round(2)

    pvalue <- data.table(tbl) %>% dcast(metric + variable ~ LC, value.var = "p.value") %>% .[order(metric)]
    pvalue[, 3:7] %<>% round(3)

    r <- listk(slope, pvalue)
    if (!is.null(outfile)) write_list2xlsx(r, gsub(".pdf", "", paste0("tbl_", outfile)))
    r
}
