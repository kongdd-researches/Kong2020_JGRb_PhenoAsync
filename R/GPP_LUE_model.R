aggregate_dn <- function(data, nday = 16){
    nptperyear <- ceiling(365/nday)
    byname     <- c("site", "year", paste0("d", nday))

    res <- data[, lapply(.SD, mean, na.rm = T), byname, .SDcols = vars]
    colnames(res)[3] <- "dn"

    res <- merge(st[, .(site, IGBP, lat)], res, by = "site") %>%
        .[, `:=`(date = as.Date(sprintf("%d%03d", year, (dn-1)*nday+1), "%Y%j"),
                 ydn  = (year - 2000)*nptperyear + dn)] %>% #dn ID order
        ddply(.(site), addPredictor_tn)  %>%
        reorder_name(c("site", "IGBP", "date", "year", "year2", "d16", "d8", "ydn"))
    res[, year2 := as.integer(year + ((month(date)>=7)-1)*(lat<0))]
    res
}

#' cal_LSWImax
#' works for site
cal_LSWImax <- function(x){
    site = x$site[1]

    tryCatch({
        x <- x[date >= sos_date & date <= eos_date]
        res <- x[QC_flag == "good", .(LSWI_max = max(LSWI)), .(site, year2)]

        res[, LSWI_max := zoo::rollapply(LSWI_max, 5, nth_max, partial = T),
             .(site)] # 5 year second maximum moving
        return(res)
    }, error = function(e){
        message(sprintf("[%s] %s", site, e$message))
    })
}
