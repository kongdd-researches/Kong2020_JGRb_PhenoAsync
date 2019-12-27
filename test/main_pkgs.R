# source("test/main_pkgs.R")
suppressWarnings({
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
    ## load data
    library(sp2)
    library(foreach)
    library(iterators)
    library(glue)
    library(rfluxnet)
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
    Ipaper::melt_list(res, "site")
}

## 每年仅挑取最大GSL的season, GSL = TRS5.EOS - TRS5.SOS
filter_primary <- function(df){
    df[, GSL := TRS5.eos - TRS5.sos]
    groups = .(type_VI, site, meth, group, origin) %>% names() %>% intersect(names(df))
    I_sel <- df[, order(-GSL), groups]$V1 == 1 & df$GSL > 30
    df[I_sel, ]
}

st_166 <- st_flux166
st_212 <- fread(path.mnt("E:/Research/phenology/DATA/flux/station/st_flux212.csv")) %>% classify_lc()

file_official_dd = 'E:/github/hydro/rfluxnet/OUTPUT/fluxsites166_SUBSET_official_daily.csv'
file_FULLSET_dd  = "E:/github/hydro/rfluxnet/OUTPUT/fluxsites166_FULLSET_8-day_v20191216 (80%).csv"
file_GPP_north   = "INPUT/df_north.RDS"
file_brks        = "INPUT/pheno_gpp_st109.rda"

# MODIS DATA
file_MYD11A2       <- "INPUT/fluxnet212/flux212_MYD11A2_Tnight.RDS"
file_MYD11A2_pheno <- "INPUT/fluxnet212/flux212_MYD11A2_T_phenology_5d_10d.RDS"

