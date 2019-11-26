

l <- read_json("G:/CSIRO/ET&GPP/fluxnet212/raw-json/phenoflux212_MCD15A3H_006_463m_buffer.geojson")$features
x <- l[[1]]


#' tidy_gee_json
#'
#' @examples
#' \dontrun{
#' l = read_json("phenoflux212_MCD15A3H_006_463m_buffer.geojson")$features
#' x = l[[1]]
#' tidy_gee_json(x)
#' }
#' @export
tidy_gee_json <- function(x, varnames = c("Fpar", "FparLai_QC", "Lai"))
{
    x$properties[] %>% map(unlist) %>%
        c(x$properties[c("site", "date")], .) %>% as.data.table() %>%
        cbind(Id_near = 1:nrow(.), .)
}

tidy_gee_json2 <- function(x, varnames = c("Fpar", "FparLai_QC", "Lai"))
{
    x$properties[] %>% map(unlist) %>%
        c(x$properties[c("site", "date")], .) %>%
        map_int(length)
        # as.data.table() %>%
        # cbind(Id_near = 1:nrow(.), .)
}

load_pkgs <- function(ncluster = 4, pkgs){
    l_ply(1:ncluster, function(i){
        for(i in pkgs)
            library(i, character.only = TRUE)
    }, .parallel = TRUE)
}
ncluster = 4
# load_pkgs(ncluster, )
pkgs = c("purrr", "magrittr")
lst2 = llply(l[1:6], tidy_gee_json2, .progress = "text",
             .parallel = TRUE, .paropts = list(.packages = pkgs))

lst = llply(l, tidy_gee_json, .progress = "text")
