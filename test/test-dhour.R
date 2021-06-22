library(phenofit)
library(foreach)
library(iterators)
library(Ipaper)
# ------------------------------------------------------------------------------
df <- readRDS("INPUT/flux8d_assembly_st166.RDS")
df[, dhour2 := solartime::computeDayLength(date, lat)]
df[, dhour_norm := (dhour2/max(dhour2))^2, .(site)]

# Ma XuanLong, 2013, RSE; Monteith and Unsworth, 2013
df$PAR_TOA = df[, .(lat, doy = yday(date))][, MJ_2W(cal_Ra(lat, doy))*0.4 ]
df$GPPobs = df$GPP
# df[, as.list(GOF(GPP, GPP_vpm))]

IGBPs = unique(df$IGBP)
lst = foreach(IGBPname = IGBPs, i = icount()) %do% {
    runningId(i)
    d = df[IGBP == IGBPname, ]

    par0 = c(A = 17, p = 2)
    goal(par0, d)
    opt <- optim(par0, goal, df = d, lower = c(10, 0), upper = c(30, 5), method = "L-BFGS-B")
    opt$par

    d$GPP_pheno = get_GPP(opt$par, d)
    d
}

res[, as.list(GOF(GPPobs, EVI.whit*PAR_TOA))][, .(RMSE, NSE, R2, R, Bias_perc)]
res[, as.list(GOF(GPPobs, dhour))][, .(RMSE, NSE, R2, R, Bias_perc)]
res[, as.list(GOF(GPPobs, Rs))][, .(RMSE, NSE, R2, R, Bias_perc)]

res = do.call(rbind, lst)[group == 5, ]
res[, as.list(GOF(GPPobs, GPP_pheno))][, .(RMSE, NSE, R2, R, Bias_perc)]
res[, as.list(GOF(GPPobs, GPP_pheno))][, .(RMSE, NSE, R2, R, Bias_perc)]

res[, as.list(GOF(LE_CORR, EVI.whit))][, .(RMSE, NSE, R2, R, Bias_perc)]
res[, as.list(GOF(LE_CORR, dhour_norm))][, .(RMSE, NSE, R2, R, Bias_perc)]
res[, as.list(GOF(LE_CORR, Rs))][, .(RMSE, NSE, R2, R, Bias_perc)]
res[, as.list(GOF(LE_CORR, PAR_TOA)), .(IGBP)][, .(IGBP, RMSE, NSE, R2, R, Bias_perc)]
# res[, as.list(GOF(LE_CORR, PAR_TOA))][, .(RMSE, NSE, R2, R, Bias_perc)]
# res[, as.list(GOF(LE_CORR, dhour_norm))][, .(RMSE, NSE, R2, R, Bias_perc)]

library(tidyverse)
library(ggrepel)
library(tidyverse)

ggplot(res, aes(Rs, GPPobs)) +
    geom_density2d(alpha = 0.2) +
    geom_hex()
ggplot(res, aes(dhour_norm, GPPobs)) +
    stat_density2d(aes(fill = ..density..^0.25), geom = "tile", contour = FALSE, n = 200) +
    scale_fill_continuous(low = "white", high = "dodgerblue4") +
    geom_point(alpha = 0.1, shape = 20)

with(res, smoothScatter(Rs * EVI, GPPobs))
with(res, smoothScatter(Rs * (EVI.whit - 0.08), GPPobs))
with(res, smoothScatter(PAR_TOA * (EVI.whit - 0.08), GPPobs))
with(res, smoothScatter(dhour_norm * (EVI.whit - 0.08), GPPobs))
with(res, smoothScatter(VPD, GPPobs))

{
    d_PC = res[, as.list(GOF(GPPobs, GPP_pheno)), .(IGBP)][, .(IGBP, RMSE, NSE, R2, R, Bias_perc)]
    d_EVI = res[, as.list(GOF(GPPobs, EVI)), .(IGBP)][, .(IGBP, RMSE, NSE, R2, R, Bias_perc)]

    d = melt_list(listk(PC = d_PC, EVI = d_EVI), "type") %>%
        melt(c("type", "IGBP")) %>%
        dcast(IGBP+variable~type)

    lims = c(0, 1)
    data = d[variable == "R2"]
    ggplot(data, aes(EVI, PC)) +
        geom_point() +
        # facet_wrap(~variable, scales = "free") +
        geom_abline(slope = 1) +
        geom_text_repel(aes(label = IGBP))+
        coord_equal(xlim = lims, ylim = lims, expand = FALSE)
}

plot(d_pheno)
{
    # df[, as.list(GOF(GPP, GPP_sim))] %>% print()
    # df[, as.list(GOF(GPP, EVI.whit*dhour_norm * 17))]

    df[, as.list(GOF(GPP, EVI.whit*dhour_norm * 17))]
    # df[, as.list(GOF(GPP, EVI.whit*dhour_norm * 20)), .(IGBP)]
}

## 测试AVHRR的结果
library(data.table)
library(lubridate)
df <- fread("N:/Research/PML_V2/PML_V2/data/FLUXNET_OBS_AVHRR_LAI_15D_for_PMLV2-AVHRR.csv")
df[df == -9999] = NA

df[, date_end := as.Date(sprintf("%d%03d", year, doy), "%Y%j")]
df %<>% mutate(date_begin = date_end - ddays(14))


df$dhour2 <- df[, (solartime::computeDayLength(date_begin, lat) + solartime::computeDayLength(date_end, lat))]

# 为何要用LAI, EVI2可能会效果更好
{
    opt <- optim(c(A = 3, p = 2), goal, lower = c(1, 0), upper = c(10, 5), method = "L-BFGS-B")
    par = opt$par
    print(par)
    get_gof(par[1], par[2])
}

df[, as.list(GOF(GPPobs, LAI*dhour_norm*3.2))]
df[, as.list(GOF(GPPobs, LAI*dhour_norm*3.6))]
df[, as.list(GOF(GPPobs, MODGPP))]

## MODIS veg forcing
df <- fread("N:/Research/PML_V2/PML_V2/data/PML_inputs_8d_dynamicLC_v20180312_102sp (80%).csv")
df$date %<>% ymd()
df$GPPobs <- df[, .(GPP_NT, GPP_DT)] %>% as.matrix() %>% rowMeans(na.rm = TRUE)
df$dhour2 <- df[, (solartime::computeDayLength(date, lat) + solartime::computeDayLength(date + ddays(7), lat))]
p = 2
df[,  dhour_norm := (dhour2/max(dhour2))^p, .(site)]
df[, as.list(GOF(GPPobs, LAI*dhour_norm*3.2))]
