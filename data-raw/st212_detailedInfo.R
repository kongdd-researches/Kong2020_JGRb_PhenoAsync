library(httr)
library(xml2)
library(jsonlite)
library(Ipaper)
library(data.table)
library(foreach)
library(iterators)


tidy_siteInfo <- function(x) {
    site  = x$SITE_ID
    name  = x$SITE_NAME
    url   = x$URL_AMERIFLUX
    lon   = x$GRP_LOCATION$LOCATION_LONG
    lat   = x$GRP_LOCATION$LOCATION_LAT
    elev  = x$GRP_LOCATION$LOCATION_ELEV
    IGBP  = x$IGBP
    tier1 = x$Tier1
    tier2 = x$Tier2
    MAT   = x$GRP_CLIM_AVG$MAT
    MAP   = x$GRP_CLIM_AVG$MAP
    listk(site, name, lon, lat, elev, MAT, MAP, IGBP, tier1, tier2, url) %>%
        replace_null(unlist = FALSE) %>% as.data.table()
}

# 1. all site info
url_meta <- "https://ameriflux-data.lbl.gov/AmeriFlux/SiteSearch.svc/SiteMapData/Fluxnet"
p <- POST(url_meta) %>% content()

st212 = map(p, tidy_siteInfo) %>% do.call(rbind, .)
fwrite(st212, "data-raw/st212.csv")

st95 <- st212[site %in% st$site, ]
write_list2xlsx(listk(st95), "data-raw/st95.xlsx")


# 2. get references
lst <- foreach(site = st212$site, i = icount()) %do% {
    runningId(i)
    url = glue("https://ameriflux-data.lbl.gov/BADM/DOI/FLUXNET2015/{site}")

    tryCatch({
        x = GET(url) %>% content()
    }, error = function(e) {
        message(sprintf('%s', e))
    })
}

load("data-raw/st212_lst.rda")
lst_refer = map(lst[st$site], ~.x$values$DOI_REFERENCES)
d_refer = map(lst_refer, ~.x$`1`$REFERENCE_PAPER) %>% replace_null() %>% data.table(site = st$site, reference = .)
# res = map(lst, ~.x$values$DOI_INFO[[1]] %>% as.data.table) %>% do.call(rbind, .)
author <- d_refer$reference %>% str_extract("\\w*(?=,)")
d = map(lst_refer, select_reference) %>% do.call(rbind, .) %>% cbind(site = names(lst_refer), .)
# fwrite(refer, "refer.csv")

d95 <- st95[, .(site, name, lon, lat, IGBP)] %>% merge(d, all.x = TRUE) %>%
    cbind(I = 1:nrow(.), .)
fwrite(d95, "data-raw/st95_refer.csv")
write_list2xlsx(listk(d95), "data-raw/st95_refer.xlsx")
# I_bad  <- which.null(first_refer) #%>% names() #%>% code_ChrVec()
# I_bad2 <- map(first_refer, ~substr(.x, 1, 1) == " ") %>% replace_null() %>% which()
# sites_bad <- c(I_bad, I_bad2) %>% sort() %>% names() %>% unique()
# sites_bad <- c("BE-Bra", "CN-Cng", "CN-Ha2", "CZ-BK2", "DE-Akm", "DE-Obe", "DE-Spw", "IT-Ren",
#                "IT-SR2")
