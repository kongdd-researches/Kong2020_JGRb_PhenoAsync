# source('test/phenology_async/R/s2_reason_of_async/d_reason_of_async(INPUT).R')
# Modified to apply in site*dn
load("data_test/flux115_async_input.rda")

## Global variables
varnames   <- c("EVI", "NDVI", "T", "Prcp", "Rs", "VPD", "APAR", "GPP", paste0("GPP_t", 1:3))[1:8]
predictors <- c("EVI", "Rs", "T", "Prcp", "VPD", "APAR")#[-6]#[-c(1, 2)]
response   <- "GPP"

# Elasticity method -------------------------------------------------------
sitename <- "AT-Neu"#"CH-Oe2" #"AT-Neu"
d <- d_mod09a1 # all sites' data
x <- d[site == sitename] #  & SummaryQA <= 1, .SD, .SDcol = c("site", "date", "ydn", varnames)
# Too much missing values in Prcp, which lead lm failed.

## 2. filter sites with more than 5 year data
info <- d[, .(n = length(unique(year))), .(site)][n >= 5][order(-n)]
sites_long <- st[site %in% info$site]$site

#
info_async <- read.xlsx("table1.over_decouple.xlsx") %>% data.table()
info_async_long <- info_async[site %in% sites_long]

d_long <- d[site %in% sites_long]

by     <- c("IGBP") # site
res <- dlply(d_long, by , Elasticity_GPP, predictors)
sites_ck     <- sapply(res, length) %>% .[ . == 0] %>% names() %T>% print

df_elastic   <- rm_empty(res) %>% transpose() %>% map(~melt_list(.x, by))
pdat_elastic <- melt_list(df_elastic[1:4], "type") %>% melt(c(by, "dn", "type")) %>%
    spread("type", "value")
pdat_elastic$IGBP %<>% factor(IGBPnames_006)


ggplot(pdat_elastic[pvalue < 0.1], aes(dn, perc_abs)) +
    geom_point(aes(color = coef >= 0, shape = pvalue > 0.1)) +
    # geom_line() +
    # geom_smooth() +
    facet_grid(sprintf("%s~variable", by) %>% as.formula()) +
    theme(legend.position = "bottom") +
    scale_y_continuous(limits = c(0, 1))



# # p1 <- ggplot(pdat, aes(dn, perc_abs)) +
#     geom_point(aes(color = coef >= 0)) +
#     geom_line() +
#     facet_wrap(~variable, ncol = 1) +
#     theme(legend.position = "bottom")
#
# p2 <- ggplot(pdat, aes(dn, coef)) +
#     geom_point(aes(color = coef >= 0)) +
#     geom_line() +
#     facet_wrap(~variable, ncol = 1) +
#     theme(legend.position = "bottom")
#
# library(gridExtra)
# grid.arrange(p1, p2, ncol = 2)
#
# d_avg <- x[, .SD, .SDcols = c("year", "dn", varnames[-2], "APAR")] %>% melt(c("year", "dn"))
#
# p_evi <- ggplot(x, aes(dn, EVI, color = year)) +
#     geom_point() + geom_smooth()
#
# # p_gpp <-
#     ggplot(d_avg, aes(dn, value, color = year)) +
#     geom_point() + geom_smooth(formula = y ~ x^2) +
#     facet_wrap(~variable, ncol = 1, scales = "free_y")
#
# grid.arrange(p_evi, p_gpp, ncol = 1)
