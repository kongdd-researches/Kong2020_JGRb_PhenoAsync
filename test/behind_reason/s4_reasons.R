## Global variables
varnames   <- c("EVI", "NDVI", "T", "Prcp", "Rs", "VPD", "APAR", "GPP", paste0("GPP_t", 1:3))[1:8]
predictors <- c("EVI", "Rs", "TA", "Prcp", "VPD", "APAR")#[-6]#[-c(1, 2)]
response   <- "GPP"

check_sensitivity(df_d8, predictors)
