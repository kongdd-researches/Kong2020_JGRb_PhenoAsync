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
    # library(sp2)
    library(foreach)
    library(iterators)
})

st_166 <- fread("E:/Research/phenology/DATA/flux/station/st_flux166.csv")
st_212 <- fread("E:/Research/phenology/DATA/flux/station/st_flux212.txt")

file_GPP_north = "INPUT/df_north.RDS"
