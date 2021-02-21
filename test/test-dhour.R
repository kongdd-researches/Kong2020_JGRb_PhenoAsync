library(phenofit)
library(foreach)
library(iterators)

# ------------------------------------------------------------------------------
df <- readRDS("INPUT/flux8d_assembly_st166.RDS")
df[, dhour2 := solartime::computeDayLength(date, lat)]
df[, dhour_norm := (dhour2/max(dhour2))^2, .(site)]
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

res = do.call(rbind, lst)
res[, as.list(GOF(GPPobs, GPP_pheno))][, .(RMSE, NSE, R2, R, Bias_perc)]
res[, as.list(GOF(GPPobs, GPP_pheno)), .(IGBP)][, .(IGBP, RMSE, NSE, R2, R, Bias_perc)]
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
