get_fitting.fFITs <- function (fFITs)
{
    t <- fFITs$data$t
    I <- match(t, fFITs$tout)
    Ix <- which(!is.na(I))
    I <- I[Ix]
    t <- t[Ix]
    iters <- length(fFITs$fFIT[[1]]$zs)
    df <- fFITs$fFIT %>% map(function(x) {
        d_z <- map_dfc(x$zs, ~.[I]) %>% set_colnames(paste0("ziter",
            1:iters))
        cbind(t, d_z)
    }) %>% melt_list("meth") %>% as.data.table() %>% unique()

    # tryCatch({
        # rm duplicated value
        if ("QC_flag" %in% colnames(fFITs$data)) {
            x = fFITs$data[Ix][, .(y = mean(y), QC_flag = QC_flag[1]), .(t)]
        } else {
            x = fFITs$data[Ix][, .(y = mean(y)), .(t)]
        }
        df <- merge(x, df, id = "t")
        df$t %<>% as.Date(date.origin)
        df
    # }, error = function(e) {
    #     browser()
    # })
}

get_fitting <- function (fit)
{
    llply(fit, get_fitting.fFITs) %>% melt_list("flag")
}

date.origin <- as.Date("2000-01-01")
