
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
writeOGR_3by3 <- function(outdir = "INPUT/shp", scale = 500){
    st = st_212[, .(site, lon, lat, IGBP)]

    cellsize = scale/500 * 1/240

    lon      = c(-1, 0, 1) %>% rep(3) %>% multiply_by(cellsize)
    lat      = c(1, 0, -1) %>% rep(each = 3) %>% multiply_by(cellsize)
    adj_mat  = cbind(lon, lat)

    grps = 1:nrow(adj_mat) %>% set_names(., .)
    lst = foreach(i = grps) %do% {
        st = st_212[, .(site, lon, lat, IGBP)]
        delta_x = adj_mat[i, 1]
        delta_y = adj_mat[i, 2]

        st$lon %<>% add(delta_x)
        st$lat %<>% add(delta_y)
        st
    }
    df = melt_list(lst, "group")
    # df
    sp = df2sp(df)
    outfile = glue("{outdir}/st212-{scale}m-3by3.shp")
    rgdal::writeOGR(sp, outfile, "sites", driver = "ESRI Shapefile")
    df
}
