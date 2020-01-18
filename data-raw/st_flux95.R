## code to prepare `DATASET` dataset goes here
st = data.table::fread("data-raw/st_flux95.csv")
st$LC %<>% factor(c("Cropland", "Grassland", "Shrubland", "Forest", "ENF"))
usethis::use_data(st, overwrite = TRUE)

# sp <- st_212[site %in% st$site, .(site, IGBP, lon, lat)] %>% df2sp()
# writeOGR(sp, "PhenoAsync_95st.shp", "95st", driver = "ESRI Shapefile")
