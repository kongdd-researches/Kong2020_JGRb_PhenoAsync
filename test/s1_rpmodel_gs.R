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

## 1. 提取最大光合速率
## 2. 提取气孔导度
df$ppfd =

##
library(rpmodel)
out_pmodel <- rpmodel(
    tc             = 20,           # temperature, deg C
    vpd            = 1000,         # Pa,
    co2            = 400,          # ppm,
    fapar          = 1,            # fraction  ,
    ppfd           = 300,          # mol/m2/d,
    # patm           = 101325      # Pa
    elv            = 0,            # m.a.s.l.,
    # kphio          = 0.049977,   # quantum yield efficiency as calibrated for setup ORG by Stocker et al. 2020 GMD,
    # beta           = 146,        # unit cost ratio a/b,
    c4             = FALSE,
    method_optci   = "prentice14",
    method_jmaxlim = "smith19",
    do_ftemp_kphio = TRUE,        # corresponding to setup ORG
    do_soilmstress = FALSE,        # corresponding to setup ORG
    verbose        = TRUE
)
