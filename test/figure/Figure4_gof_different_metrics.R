# run after Figure3
## Figure4: 绘制不同阶段的差异（phenological metrics）
st = st_212[site %in% sites, .(site, lat, IGBP, LC)] #%>% summary()

d = df[abs(diff) < 60 & meth %in% c("Elmore", "Beck") & product %in% c("Terra_EVI", "Terra_NDVI", "Combined_LAI"),
       as.list(GOF(y_obs,y_sim, include.r = FALSE)), .(type_VI, type_period, variable, site)]
d$variable %<>% factor(metrics_all)
d <- d[!is.na(variable), ]
d$type_VI %<>% factor(names_VI)
d_melt <- melt(d, c("type_VI", "type_period", "variable", "site"), variable.name = "index") %>%
    .[index %in% c("RMSE", "Bias", "MAE"),] %>% merge(st[, .(site, IGBP, LC)])

d_mean <- d_melt[, median(value, na.rm = TRUE), .(variable, index, type_period, type_VI)][, .(y = mean(V1)), .(type_period, type_VI, index)]
n <- nrow(d_mean)/2
d_1 = d_mean[type_period == "Green-up period"] %>% cbind(x = rep(c(0.4, 10.4), each = n))
d_2 = d_mean[type_period == "Withering period"] %>% cbind(x = rep(c(10.6, 20.4), each = n))

if (FALSE){
    p2 <- ggplot(d_melt[type_period != "others"], aes(variable, value, fill = type_period)) +
        stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.5) +
        geom_boxplot2(notch = FALSE, outlier.shape = NA, coef = 0, width = 0.8, size = 0.5) +
        geom_line(data = d_1, aes(x, y, fill = NULL), color = "blue", size = 1.1, linetype = 1) +
        geom_line(data = d_2, aes(x, y, fill = NULL), color = "red", size = 1.1, linetype = 1) +
        geom_text(data = d_1[1:n,], aes(x, y, label = round(y, 1), fill = NULL), hjust = -0.1, vjust = -0.35,
                  color = "blue", fontface = 2, family = "times", size = 5) +
        geom_text(data = d_2[(n+1):(2*n),], aes(x, y, label = round(y, 1), fill = NULL), hjust = 1.1, vjust = -0.35,
                  color = "red", fontface = 2, family = "times", size = 5) +
        # stat_summary(fun.data = stat_hline, geom = "hline", size = 0.5) +
            theme(legend.position = c(0.492, 1.03),
              legend.justification = c(0, 1),
              # legend = unit(1, "cm"),
              legend.box.background = element_blank(),
              legend.background = element_blank(),
              panel.grid.major = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        guides(fill = guide_legend(nrow = 1)) +
        labs(x = NULL, y = NULL) +
        scale_fill_manual(values = colors_period) +
        facet_grid(index~type_VI, scales = "free")
        # geom_hline(yintercept = 25, color = "red", size = 1.2, linetype = 2)
    outfile = glue("Figure4 (abandoned)_RMSE of different period.pdf")
    write_fig(p2, outfile, 12, 7)
}

## Figure4:
## 方差分析
{
    names_lc = levels(st$LC)
    info <- foreach(indexName = indexNames %>% set_names(., .)) %do% {
        d <- foreach(name_VI = names_VI) %do% {
            temp <- foreach(lc = names_lc) %do% {
                d = d_melt[type_VI == name_VI & LC == lc]
                m <- aov( value ~ type_period , d)
                TukeyHSD(m)[[1]]
            }
            x = do.call(rbind, temp)
            cbind(rownames(x), LC = names_lc, data.table(x))
        } %>% Ipaper::melt_list("type_VI")
        d
    }
}

{
    gof_index = c("Bias", "MAE", "RMSE")
    d_lab = expand.grid(LC = levels(d_melt$LC), index = gof_index)
    d_lab$label = sprintf("(%s)", letters[1:nrow(d_lab)])
    d_melt$index %<>% factor(gof_index)
    lwd = 0.5
    temp = foreach(indexName = names_VI, i = icount()) %do% {
        runningId(i)
        p <- ggplot(d_melt[type_VI == indexName & index %in% indexNames],
                    aes(variable, value, fill = type_period)) +
            stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.4, size = 0.25) +
            geom_boxplot2(outlier.shape = NA, coef = 0, size = 0.25) +
            geom_vline(xintercept = c(2, 5, 7, 9, 12) + 0.5, linetype = 3, size = 0.05) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                  panel.grid = element_blank(),
                  legend.position = "none") +
            facet_grid(index~LC, scale = "free") +
            labs(x = NULL, y = NULL) +
            geom_text(data = d_lab, aes(-Inf, Inf, fill = NULL, label = label), hjust = -0.3, vjust = 1.4,
                      size = 6, fontface = 2, family = "Times") +
            geom_hline(data = data.frame(index = "Bias", yintercept = 0),
                       aes(yintercept = yintercept), color = "blue", size = lwd) +
            geom_hline(data = data.frame(index = "Bias", yintercept = c(-1, 1)*15),
                       aes(yintercept = yintercept), color = "red", linetype = 2, size = lwd) +
            geom_hline(data = data.frame(index = "MAE", yintercept = c(1)*20),
                       aes(yintercept = yintercept), color = "red", linetype = 2, size = lwd) +
            geom_hline(data = data.frame(index = factor("RMSE", levels(d_melt$index)),
                                         yintercept = 20),
                       aes(yintercept = yintercept), color = "red", linetype = 2, size = lwd)
        outfile = glue("Figure4_gof_landcover_{indexName}.pdf")
        write_fig(p, outfile, 14, 7)
    }
}
