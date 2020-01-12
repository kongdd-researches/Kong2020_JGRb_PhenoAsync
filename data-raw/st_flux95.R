## code to prepare `DATASET` dataset goes here
st = data.table::fread("data-raw/st_flux95.csv")
st$LC %<>% factor(c("Cropland", "Grassland", "Shrubland", "Forest", "ENF"))
usethis::use_data(st, overwrite = TRUE)
