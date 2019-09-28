library(ggstance)
source('test/phenology_async/R/s2_reason_of_async/res_phenology.R')

## error sites
# "IT-Noe" "US-Ton" "US-Var" "IT-SRo" "US-Me2"

stat_bar <- function(x, ...){
    mean = mean(x, na.rm = T)
    sd = sd(x, na.rm = T)
    list(ymin = mean - sd, ymax = mean + sd, ymean = mean)
}

stat_barh <- function(x, ...){
    mean = mean(x, na.rm = T)
    sd = sd(x, na.rm = T)
    list(xmin = mean - sd, xmax = mean + sd, xmean = mean)
}

alpha = 0.4
################################################################################

## 1. over all
df_overall <- dplyr::select(df_p, -matches("anorm|mean")) %>% spread( type, value)

# elmore(n=6114, 67less) has the best performance than zhang, AG, BECK(n=6181);
df_overall[, as.list(GOF(GPPobs, MOD13A1_EVI)), .(meth, phase)]
#    meth  phase     RMSE       NSE        R2      MAE        AI       Bias    Bias_perc         R
# 7: ELMORE spring 33.90632 0.7711678 0.8158284 22.23327 0.9468143 -1.2178891 -0.009365671 0.9032322
# 8: ELMORE autumn 36.20461 0.7622759 0.8282366 26.43690 0.9427783 15.8021529  0.057905862 0.9100750
# 9: ELMORE    pop 35.79330 0.7163782 0.8058379 22.76960 0.9389629  0.6064000  0.003139887 0.8976848
df_overall[meth == "ELMORE", as.list(GOF(GPPobs, MOD13A1_EVI)), .(meth, phase)]

p1 <- ggplot(df_overall[meth == "ELMORE"], aes(MOD13A1_EVI, GPPobs, color = lat < 0)) +
    geom_point(alpha = alpha) +
    geom_abline(slope = 1, color= "red") +
    facet_wrap(~phase, scales = "free") +
    labs(y="Over all", x = NULL)

## 2. anormaly
df_anorm <- df_p %>% dplyr::select(-matches("value|mean")) %>% spread(type, anorm)
df_anorm[, as.list(GOF(GPPobs, MOD13A1_EVI)), .(meth, phase)][order(meth)]

p2 <- ggplot(df_anorm[meth == "ELMORE"], aes(MOD13A1_EVI, GPPobs, color = lat < 0)) +
    geom_point(alpha = alpha) +
    geom_abline(slope = 1, color= "red") +
    facet_wrap(~phase, scales = "free")+
    labs(y="Anormaly", x = NULL)

## 3. site mean

## GOF of different sites
# info <- df_p[meth == "ELMORE", as.list(GOF(GPPobs, MOD13A1_EVI)), .(meth, phase, site)]
xname <- "MOD13A1_EVI"
yname <- "GPPobs"
by    <- .(site, phase, lat) %>% names()

data_bar <- merge(df_p[type == xname, as.list(stat_barh(value)), by],
      df_p[type == yname, as.list(stat_bar(value)), by])

p3 <- ggplot(data_bar, aes(xmean, ymean, color = lat < 0)) +
    geom_point(alpha = alpha) +
    geom_errorbar( aes(x = xmean, ymin = ymin, ymax = ymax), alpha = alpha) +
    geom_errorbarh(aes(y = ymean, xmin = xmin, xmax = xmax), alpha = alpha) +
    facet_wrap(~phase, scales = "free")+
    geom_abline(slope = 1, color= "black") +
    labs(y="Site mean", x = NULL)

## 4. IGBP mean
by    <- .(IGBP, phase) %>% names()

data_bar <- merge(df_p[type == xname, as.list(stat_barh(value)), by],
      df_p[type == yname, as.list(stat_bar(value)), by])

p4 <- ggplot(data_bar[IGBP != "SAV"], aes(xmean, ymean, color = IGBP)) +
    geom_point(aes(shape = IGBP)) +
    geom_errorbar( aes(x = xmean, ymin = ymin, ymax = ymax)) +
    geom_errorbarh(aes(y = ymean, xmin = xmin, xmax = xmax)) +
    facet_wrap(~phase, scales = "free")+
    geom_abline(slope = 1, color= "black") +
    labs(y="Site mean", x = NULL)

## combine
fontsize <- 14
left   <- textGrob(expression(GPP[EC]), gp=gpar(fontsize=fontsize, fontface = "bold"), rot = 90)
bottom <- textGrob(expression(EVI), gp=gpar(fontsize=fontsize, fontface = "bold"))
ps <- grid.arrange(p1, p2, p3, p4, left=left, bottom=bottom, ncol=1)

write_fig(ps, file = "Figure1_How_strong_of_async_v1.pdf", width = 10, height = 9, show = T)
