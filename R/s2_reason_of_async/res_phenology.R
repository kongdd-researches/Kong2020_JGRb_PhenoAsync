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
d_diff <- pheno %>% map(~.x[, 3:21]) %>% {.[[2]] - .[[1]]} %>% cbind(head, .) %>%
    dplyr::select(-matches("ZHANG|TRS6|TRS2")) # rm inflection method

d_diff <- melt(d_diff, id.vars = c("flag", "origin", "meth", "site"), variable.name = "index")
d_diff$index %<>% factor(c(metric_spring, "DER.pop", metric_autumn))
d_diff[, `:=`(phase = metric_phase(index))]

d_diff %<>% merge(st[, .(site, IGBP)])

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