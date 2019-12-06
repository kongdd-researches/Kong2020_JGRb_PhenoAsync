
#' tidy_gee_json
#'
#' @examples
#' \dontrun{
#' l = read_json("phenoflux212_MCD15A3H_006_463m_buffer.geojson")$features
#' x = l[[1]]
#' tidy_gee_json(x)
#' }
#' @export
gee_data.table <- function(x, varnames = c("Lai", "Fpar", "FparLai_QC"))
{
    x$properties[varnames] %>% map(unlist) %>%
        c(x$properties[c("site", "date")], .) %>% as.data.table() %>%
        cbind(Id_near = 1:nrow(.), .)
}

#' @export
gee_var_len <- function(x, varnames = c("Lai", "Fpar", "FparLai_QC"))
{
    probs = x$properties
    probs[varnames] %>%
        map(length) %>% c(probs[c("site", "date")], .) %>%
        as.data.table()
        # cbind(Id_near = 1:nrow(.), .)
}

## 重新下载数据，制作3*3 grids
#' writeOGR_3by3
#' 
#' @param st data.table with the columns at least of `site`, `lon`, and `lat`.
#' @param scale in the unit of `m`
#' 
#' @examples 
#' st = st_212[, .(site, lon, lat, IGBP)]
#' writeOGR_3by3(st, 500, "agripheno")
#' @export
writeOGR_3by3 <- function(st, scale = 500, prefix = "st212", outdir = "INPUT/shp"){
    # st = st_212[, .(site, lon, lat, IGBP)]

    cellsize = scale/500 * 1/240

    lon      = c(-1, 0, 1) %>% rep(3) %>% multiply_by(cellsize)
    lat      = c(1, 0, -1) %>% rep(each = 3) %>% multiply_by(cellsize)
    adj_mat  = cbind(lon, lat)

    grps = 1:nrow(adj_mat) %>% set_names(., .)
    lst = foreach(i = grps) %do% {
        d = st
        delta_x = adj_mat[i, 1]
        delta_y = adj_mat[i, 2]

        d$lon %<>% add(delta_x)
        d$lat %<>% add(delta_y)
        d
    }

    df = melt_list(lst, "group")[order(site), ]
    # df
    sp = df2sp(df)
    outfile = glue("{outdir}/{prefix}-{scale}m-3by3.shp")
    rgdal::writeOGR(sp, outfile, "sites", driver = "ESRI Shapefile")
    df
}
