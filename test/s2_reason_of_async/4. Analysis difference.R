source("test/stable/load_pkgs.R")
library(Ipaper)

prefixs <- c("v0.1.6_MOD13A1_EVI", "v0.1.8_MOD09A1_EVI2", "v0.1.8_MOD09A1_EVIc15")
prefix  <- prefixs[1]
# lst_MOD13A1_EVI <- get_sbatch(paste0(dir_flush, 'result/phenoflux115/'))
lst_MOD13A1_EVI <- get_sbatch(paste0(dir_flush, 'result/phenoflux115/', prefix))

lst_GPPobs <- get_sbatch(paste0(dir_flush, 'result/phenoflux115/v0.1.6_GPPobs'))

df_GPPobs      <- map(lst_GPPobs, ~ melt_list(.x$pheno$doy, "meth")) %>%
    melt_list("site") %>% data.table()
df_MOD13A1_EVI <- map(lst_MOD13A1_EVI, ~ melt_list(.x$pheno$doy, "meth")) %>%
    melt_list("site") %>% data.table()

pheno <- list(df_GPPobs, df_MOD13A1_EVI) %>%
    set_names(c("GPPobs", "MOD13A1_EVI"))

# check order
pheno %>% map(~.x[, .(flag, origin, meth, site)]) %>% {all.equal(.[[1]], .[[2]])}

## 1. difference
head <- pheno[[1]][, .(flag, origin, meth, site)]
d_diff <- pheno %>% map(~.x[, 3:21]) %>% {.[[2]] - .[[1]]} %>% cbind(head, .) %>%
    dplyr::select(-starts_with("ZHANG")) # rm inflection method

d <- melt(d_diff, id.vars = c("flag", "origin", "meth", "site"), variable.name = "index")

metric_spring <- contain(d_diff, "sos|UD|SD|Greenup|Maturity")
metric_autumn <- contain(d_diff, "eos|DD|RD|Senescence|Dormancy")

d$index %<>% factor(c(metric_spring, "DER.pop", metric_autumn))
d$phase <- "spring"
d[index == "DER.pop", phase := "pop"]
d[index %in% metric_autumn, phase := "autumn"]

d$phase %<>% factor(c("spring", "pop", "autumn"))

# d[, value := value + ]
pdat <- d[abs(value) < 50]
# pdat <- d[!(site %in% sites_rm)]
ggplot(pdat, aes(index, value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_hline(yintercept = 0, color = "blue", linetype = 2, size = 1) +
    geom_hline(yintercept = c(-15, 15), color = "red", linetype = 2, size = 1)



## 2. performance index
d_pheno <- map(pheno, ~melt(.x, id.vars = c("flag", "origin", "meth", "site"), variable.name = "index")) %>%
    melt_list("model") %>%
    dcast(flag+origin+meth+site+index~model, value.var = "value")

info <- ddply_dt(d_pheno, .(GOF(GPPobs, MOD13A1_EVI)), .(site))
info[order(-RMSE)][1:20, ]


sites_rm <- info[RMSE >= 60, site]


## select which site is more significant
d_gof <- ddply_dt(d, .(GOF(value*0, value)), .(site, phase)) %>%
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




