# source("test/main_pkgs.R")
suppressMessages({
    library(reshape2)

    library(plyr)
    library(tidyverse)
    library(data.table)

    library(magrittr)
    library(lubridate)
    library(jsonlite)
    library(openxlsx)
    library(purrr)

    library(Ipaper)
    library(phenofit)

    library(ggrepel)
    library(Cairo)
    library(solartime)

    ## load data
    library(sf2) # rpkgs/sf2
    library(foreach)
    library(iterators)
    library(glue)
    library(rfluxnet)
    library(ppcor)

    library(grid)
    library(gridExtra)
    library(gtable)
    library(ggpmisc)
})

# IGBP  N
#  1:  GRA 18
#  2:   MF  7
#  3:  CRO 12
#  4:  ENF 26
#  5:  OSH  3
#  6:  WET  9
#  7:  DBF 15
#  8:  SAV  2
#  9:  CSH  1
# 10:  WSA  1
classify_lc <- function(st){
    st$LC <- "NA"
    st[IGBP %in% "ENF", LC := "ENF"]
    st[IGBP %in% "GRA", LC := "Grassland"]
    st[IGBP %in% "CRO", LC := "Cropland"]
    st[IGBP %in% c("MF", "DBF"), LC := "Forest"]
    st[IGBP %in% c("OSH", "CSH", "SAV", "WSA", "WET"), LC := "Shrubland"]
    lc_names = c("Cropland", "Grassland", "Shrubland", "Forest", "ENF")
    st$LC %<>% factor(lc_names)
    st
}

melt_pheno <- function(lst) {
    res <- foreach(l = lst, i = icount()) %do% {
        # print(i)
        temp <- map(l, "pheno") %>% rm_empty()
        if (!is_empty(temp)) {
            Ipaper::melt_list(temp, "group")
        } else NULL
    } %>% rm_empty()
    # browser()
    Ipaper::melt_list(res, "site")
}

## 每年仅挑取最大GSL的season, GSL = TRS5.EOS - TRS5.SOS
filter_primary <- function(df){
    df[, GSL := TRS5.eos - TRS5.sos]
    groups = .(sate, type_VI, site, meth, group, origin) %>% names() %>% intersect(names(df))
    I_sel <- df[, order(-GSL), groups]$V1 == 1 & df$GSL > 30
    df[I_sel, ]
}

stat_sd <- function(x, ...) {
    x <- x[!is.na(x)]
    y  <- mean(x)
    y2 <- median(x)

    ymin = quantile(x, probs = 0.1)[[1]]
    ymax = quantile(x, probs = 0.9)[[1]]
    sd <- sd(x)
    # c(y = y, y2 = y2, ymin = y-sd, ymax = y+sd, sd = sd)
    c(y = y, y2 = y2, ymin = ymin, ymax = ymax, sd = sd)
}

dir_root = "n:/Research/phenology/"
st_166 <- st_flux166
st_212 <- fread(path.mnt("n:/Research/phenology/rfluxnet/data-raw/st_flux212.csv")) %>% classify_lc()


file_official_dd = glue("{dir_root}rfluxnet/OUTPUT/fluxsites166_SUBSET_official_daily.csv")
file_FULLSET_dd  = glue("{dir_root}rfluxnet/OUTPUT/fluxsites166_FULLSET_8-day_v20191216 (80%).csv")
file_GPP_north   = "INPUT/df_north.RDS"
file_brks_DT     = "INPUT/pheno_gpp_st109 (GPP_DT).rda"
file_brks_NT     = "INPUT/pheno_gpp_st109 (GPP_NT).rda"

# MODIS DATA
file_MYD11A2       <- "INPUT/fluxnet212/flux212_MYD11A2_Tnight.RDS"
file_MYD11A2_pheno <- "INPUT/fluxnet212/flux212_MYD11A2_T_phenology_5d_10d.RDS"

names_VI = c("NDVI", "EVI", "LAI") %>% set_names(., .)
file_pheno_prim <- "INPUT/pheno_flux95_prim.rda"
file_pheno_prim_DT <- "INPUT/pheno_flux95_prim (DT).rda"
file_pheno_prim_NT <- "INPUT/pheno_flux95_prim (NT).rda"

# update in 20191229
metrics_select <- c(
    "TRS1.sos", "TRS2.sos", "TRS5.sos", "DER.sos", "TRS6.sos", "TRS8.sos", "TRS9.sos",
    "TRS9.eos", "TRS8.eos", "TRS6.eos", "DER.eos", "TRS5.eos", "TRS2.eos", "TRS1.eos")

metrics_period <- c(
    "Starting", "Starting", "Fast-growing", "Fast-growing", "Fast-growing", "Maturity", "Maturity",
    "Senescence", "Senescence", "Fast-senescence", "Fast-senescence", "Fast-senescence", "Ending", "Ending")

nmetrics = length(metrics_select)/2
metric_spring <- metrics_select[ 1:nmetrics]
metric_autumn <- metrics_select[-(1:nmetrics)]
