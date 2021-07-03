# # works for multiple projects
# load_all <- function (path = ".", reset = TRUE, recompile = FALSE, 
#     export_all = TRUE, helpers = TRUE, quiet = FALSE, ...) 
# {
#     if (inherits(path, "package")) {
#         path <- path$path
#     }
#     save_all()
#     check_dots_used()

#     pkgload::load_all(path = ".", reset = reset, recompile = recompile, 
#         export_all = export_all, helpers = helpers, quiet = quiet, ...)
#     # pkgload::load_all(path = "../phenofit", reset = reset, recompile = recompile, 
#     #     export_all = export_all, helpers = helpers, quiet = quiet, ...)
# }

# suppressWarnings({
#     environment(load_all) <- environment(devtools::build)
#     assignInNamespace("load_all", load_all, ns="devtools")  
# })

#' @export
mark_outlier <- function(x, nsd = 3) {
    sd <- sd(x, na.rm = TRUE)
    mean <- mean(x, na.rm = TRUE)
    max <- mean + nsd * sd
    min <- mean - nsd * sd
    x[x > max | x < min] <- NA_real_
    x
}

dn2date <- function(year, dn, days = 8) {
    as.Date(sprintf("%d%03d", year, (dn - 1)*days + 1), "%Y%j")
}

make_dt <- function(..., ncol = 3) {
    x = list(...)
    n = length(x)
    nrow = floor(n/ncol)
    lapply(1:nrow, function(i) {
        ind = seq((i-1)*ncol + 1, i*ncol)
        x[ind] %>% as.data.table()
    }) %>% do.call(rbind, .)
}

add_gridLine <- function(dates, col = "grey60", lty = 3, ...) {
    years <- year(dates)
    date_beg <- ymd( min(years) *1e4 + 0101 )
    date_end <- ymd( max(years) *1e4 + 0101 )
    
    t_grids  <- seq.Date(date_beg, date_end, by = "year")
    abline(v = t_grids, col = col, lty = lty, ...)
}
