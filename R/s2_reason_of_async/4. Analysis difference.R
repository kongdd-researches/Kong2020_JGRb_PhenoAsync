source('test/phenology_async/R/s2_reason_of_async/res_phenology.R')

# df[site == sitename, .(site, year, GPP, P, VPD, )][, lapply(.SD, mean, na.rm = T), .(site, IGBP, year)]

# d[, value := value + ]
pdat <- d_diff[abs(value) < 50]
# pdat <- d[!(site %in% sites_rm)]
p_diff <- ggplot(pdat, aes(index, value, fill = phase)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_hline(yintercept = 0, color = "blue", linetype = 2, size = 1) +
    geom_hline(yintercept = c(-15, 15), color = "red", linetype = 1, size = 1) +
    geom_hline(yintercept = c(-8, 8), color = "red", linetype = 2, size = 1) +
    facet_wrap(~IGBP)

write_fig(p_diff, sprintf("Figure1.1_EVI-GPP_%s_diff.pdf", prefix), 9, 6)


## 2. performance index
d_pheno <- map(pheno, ~melt(.x, id.vars = c("flag", "origin", "meth", "site"), variable.name = "index")) %>%
    melt_list("model") %>%
    dcast(flag+origin+meth+site+index~model, value.var = "value")

info <- ddply_dt(d_pheno, .(GOF(GPPobs, MOD13A1_EVI)), .(site))
info[order(-RMSE)][1:20, ]


sites_rm <- info[RMSE >= 60, site]


## select which site is more significant
d_gof <- d[, as.list(GOF(value*0, value)), .(site, phase)] %>%
    merge(st[, .(site, IGBP, lat)]) %>%
    .[order(IGBP), ]

pdat_phase <- d[, .(diff = mean(value, na.rm = T)), .(site, phase)] %>%
    # dcast(site~phase, value.var = "diff") %>%
    merge(st[, .(site, IGBP, lat)])

xlab <- st[, .N, IGBP] #%>% rbind(data.table(IGBP = "all", N = nrow(st)))
xlab[, label:=sprintf("%s\n(n=%2d)", IGBP, N)]

lwd <- 0.8
colors <- scales::hue_pal()(3) %>% rev


    # geom_jitter(width = 0.2)

p <- ggplot(d_gof[RMSE < 80], aes(IGBP, Bias, color = phase)) +
    geom_boxplot() +
    geom_point(position = position_jitterdodge(jitter.width = 0.2), show.legend = F) +
    ylab("Phenological metrics of EVI - GPP") +
    scale_x_discrete(breaks = xlab$IGBP, labels = xlab$label) +
    geom_hline(yintercept = 0, color = "blue", linetype = 2, size = lwd) +
    geom_hline(yintercept = c(-15, 15), color = "red", linetype = 2, size = lwd) +
    theme_light(base_size = fontsize, base_family = "Arial") +
    theme(legend.position = c(1-0.01, 0.01), legend.justification = c(1, 0),
          panel.grid.major = element_line(linetype = 2),
          panel.grid.minor = element_blank(),
          legend.title=element_blank(),
          axis.text = element_text(color = "black")) +
    scale_color_manual(values = colors)

p

write_fig(p, sprintf("Figure1.2_EVI-GPP_%s.pdf", prefix), 9, 6)

# st[, .(site, IGBP)][, .N, .(IGBP)]
