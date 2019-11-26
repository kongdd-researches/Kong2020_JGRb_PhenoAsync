library(xml2)
library(httr)
library(data.table)
library(purrr)

p <- GET("https://ameriflux-data.lbl.gov/AmeriFlux/SiteSearch.svc/SiteMapData/Fluxnet") %>% 
    content()
check_str <- . %>% ifelse(is.null(.), "", .)
st_212 <- map(p, function(l) {
    with(l, data.table(site = SITE_ID, name = SITE_NAME, 
                       lon = l$GRP_LOCATION$LOCATION_LONG, 
                       lat = l$GRP_LOCATION$LOCATION_LAT, 
                       elev = l$GRP_LOCATION$LOCATION_ELEV %>% check_str(), 
                       IGBP = l$IGBP, 
                       MAP = l$GRP_CLIM_AVG$MAP %>% check_str(), 
                       MAT = l$GRP_CLIM_AVG$MAT %>% check_str(), 
                       tier1 = check_str(l$Tier1),
                       tier2 = check_str(l$Tier2), 
                       url   = URL_AMERIFLUX ))
}) %>% do.call(rbind, .)
# %>% unlist() %>% paste(collapse = ", ")
# save(st_212, file = "flux.rda")

load("sites.rda")

library(iterators)
library(glue)
lst = foreach(site = sites, i = icount()) %do% {
    runningId(i)
    url = glue("https://ameriflux-data.lbl.gov/BADM/DOI/FLUXNET2015/{site}")
    p = GET(url) %>% content()
    x = list(doi = p$values$DOI_INFO, refer = p$values$DOI_REFERENCES)
}
names(lst) <- sites


