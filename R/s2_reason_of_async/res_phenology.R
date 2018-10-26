source("test/stable/load_pkgs.R")

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

## 1. difference (EVI - GPP)
head <- pheno[[1]][, .(flag, origin, meth, site)]
df_diff <- pheno %>% map(~.x[, 3:21]) %>% {.[[2]] - .[[1]]} %>% cbind(head, .) %>%
    dplyr::select(-matches("ZHANG|TRS6|TRS2")) # rm inflection method

df_diff <- melt(df_diff, id.vars = c("flag", "origin", "meth", "site"), variable.name = "index")
df_diff$index %<>% factor(c(metric_spring, "DER.pop", metric_autumn))
df_diff[, `:=`(phase = metric_phase(index))]

df_diff %<>% merge(st[, .(site, IGBP)])
df_diff[, year2 := as.numeric(str_extract(flag, "\\d{4}"))]

## 2. melt phenology data
df_p <- melt_list(pheno, "type") %>%
    dplyr::select(-matches("ZHANG|TRS6|TRS2")) %>%
    # gather(index, value, -c("flag", "origin", "meth", "site", "type")) %>%
    melt(c("flag", "origin", "meth", "site", "type"), variable.name = "index") %>%
    merge(st[, .(site, lat, IGBP)])
df_p$phase <- metric_phase(df_p$index)

df_p <- df_p[!(IGBP %in% c("CRO", "EBF"))]

# add anormaly data
d_mean <- df_p[, .(mean = mean(value, na.rm = T)), .(site, type, index)]
df_p %<>% merge(d_mean, by = c("site", "type", "index"))
df_p[, anorm := value - mean]
df_p$index %<>% factor(metrics)

## 3. mete
df_mete <- d_mod09a1

info_site  <- df_mete[, .(n = length(unique(year))), .(site)][n >= 5][order(-n)]
sites_long <- st[site %in% info$site]$site %>% set_names(., .)

save(df_p, df_diff, df_mete, info_site, sites_long, st, file = "flux115_GPP&EVI_phenology.rda")


# - Rn should not use
#
varnames <- .(Rs, VPD, Prcp, T, EVI, NDVI, LSWI, #, VPD_trans
    Wscalar, Tscalar, APAR) %>% names()

byname <- .(site, IGBP, year2) %>% names()
d_diff <- df_diff[, .(diff=mean(value, na.rm = T), nyear = length(unique(year2))), .(site, IGBP, year2, phase)] %>%
    spread(phase, diff)
d_mete <- df_mete[, lapply(.SD, mean, na.rm = TRUE), byname,.SDcols = varnames]

d <- merge(d_diff, d_mete, by = byname)[!is.na(autumn), ]

# 3.1 PLSR model
corr <- cor(d[, autumn], d[, ..varnames], use = "pairwise.complete.obs")
r <- plsreg1(d[!is.na(autumn), ..varnames] %>% as.matrix(), d[!is.na(autumn), .(autumn)])
r_coef <- data.table(var = varnames,
    vip = r$VIP %>% .[nrow(.), ],
    coef = r$std.coefs,
    corr = corr[1, ])
r_coef[vip >= 0.8]

# 3.2 linear model
l <- lm(autumn~., na.omit(d[!is.na(autumn), .SD, .SDcols = c("autumn", varnames)]))
lo <- stepAIC(l)

GOF(d$autumn, predict(lo, d))
plot(d$autumn, predict(lo, d)); grid(); abline(a = 0, b = 1, col = "red")


check_overall <- function(){
    p_overall.spring <- d[abs(autumn) < 100] %>% melt(c("site", "IGBP", "year2", "nyear", "spring", "pop", "autumn")) %>%
        ggplot(aes(value, spring)) +
        geom_point() +
        facet_wrap(~variable, scales = "free") +
        geom_smooth(method = "lm") +
        stat_poly_eq(formula = y~x,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        rr.digits = 2,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~"), color = "red"),
                        parse = TRUE)

    p_site.spring <- d[abs(autumn) < 100, lapply(.SD, mean), .(site, IGBP),.SDcols = colnames(d)[-(1:4)]] %>%
        melt(c("site", "IGBP", "spring", "pop", "autumn")) %>%
        ggplot(aes(value, spring)) +
        geom_point() +
        facet_wrap(~variable, scales = "free") +
        geom_smooth(method = "lm") +
        stat_poly_eq(formula = y~x,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        rr.digits = 2,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~"), color = "red"),
                        parse = TRUE)

    p_overall.autumn <- d[abs(autumn) < 100] %>% melt(c("site", "IGBP", "year2", "nyear", "spring", "pop", "autumn")) %>%
        ggplot(aes(value, autumn)) +
        geom_point() +
        facet_wrap(~variable, scales = "free") +
        geom_smooth(method = "lm") +
        stat_poly_eq(formula = y~x,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        rr.digits = 2,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~"), color = "red"),
                        parse = TRUE)

    p_site.autumn <- d[abs(autumn) < 100, lapply(.SD, mean), .(site, IGBP),.SDcols = colnames(d)[-(1:4)]] %>%
        melt(c("site", "IGBP", "spring", "pop", "autumn")) %>%
        ggplot(aes(value, autumn)) +
        geom_point() +
        facet_wrap(~variable, scales = "free") +
        geom_smooth(method = "lm") +
        stat_poly_eq(formula = y~x,
                        eq.with.lhs = "italic(hat(y))~`=`~",
                        rr.digits = 2,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~"), color = "red"),
                        parse = TRUE)

    write_fig(p_overall.spring, 'Figures1.1_spring async vs mete (overall).pdf', 12, 9, show = F)
    write_fig(p_site.spring   , 'Figures1.1_spring async vs mete (site).pdf', 12, 9, show = F)
    write_fig(p_overall.autumn, 'Figures1.1_autumn async vs mete (overall).pdf', 12, 9, show = F)
    write_fig(p_site.autumn   , 'Figures1.1_autumn async vs mete (site).pdf', 12, 9, show = F)
}
