#' add previous time step GPP as a new variable
#' @param x Data.table with the column of ydn and GPP
addPredictor_tn <- function(x){
    I0 <- x$ydn
    I_1  <- match(I0 - 1, I0)
    I_2  <- match(I0 - 2, I0)
    I_3  <- match(I0 - 3, I0)

    x$GPP_t1 <- x$GPP[I_1]
    x$GPP_t2 <- x$GPP[I_2]
    x$GPP_t3 <- x$GPP[I_3]
    x
}

aggregate_dn <- function(data, nday = 16, st){
    vars_com <- c("site", "date", "year", "doy", "d16", "d8")
    vars <- setdiff(colnames(data), vars_com)

    nptperyear <- ceiling(365/nday)
    byname     <- c("site", "year", paste0("d", nday))

    res <- data[, lapply(.SD, mean, na.rm = T), byname, .SDcols = vars]
    colnames(res)[3] <- "dn"
    # cal Tscalar at here
    res <- merge(st[, .(site, IGBP, lat)], res, by = "site")
    res[, Tscalar := cal_Tscalar(TA, IGBP), .(IGBP)]

    res <- plyr::mutate(res,
        date  = as.Date(sprintf("%d%03d", year, (dn - 1)*nday + 1), "%Y%j"),
        year2 = as.integer(year + ((month(date) >= 7) - 1)*(lat < 0)),
        ydn   = (year - 2000)*nptperyear + dn) %>% #dn ID order
        ddply(.(site), addPredictor_tn) %>%
        reorder_name(c("site", "IGBP", "date", "year", "year2", "d16", "d8", "ydn"))
    res
}

#' nth_max
#'
#' The nth maximum value
#' @examples
#' x <- c(12.45,34,4,0,-234,45.6,4)
#' nth_max(x)
nth_max <- function(x, n = 2){
    len <- length(x)
    if (sum(!is.na(x)) <= n){
        min(x, na.rm = T)
    } else {
        sort(x, decreasing = T)[n]
    }
    # i   <- len-n+1
    # sort(x, partial=i, na.last=T)[i]
}

#' cal_LSWImax
#' works for site
LUE_LSWImax <- function(x){
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

#' Light use efficiency GPP model
#' @name LUE
NULL

#' @description
#' `LUE_Tscalar`: Air temperature constrain
#'
#' @rdname LUE
#' @export
LUE_Tscalar <- function(T, IGBP){
    IGBPname <- c("ENF", "EBF", "DNF", "DBF", "MF" , "CSH",
                "OSH", "WSA", "SAV", "GRA", "WET", "CRO",
                "URB", "CNV")
    Tmin <- c(-1, -2, -1, -1, -1, -1, 1, -1, 1, 0, -1, -1, 0, 0)
    Tmax <- c(40, 48, 40, 40, 48, 48, 48, 48, 48, 48, 40, 48, 48, 48)
    Topt <- c(20, 28, 20, 20, 19, 25, 31, 24, 30, 27, 20, 30, 27, 27)

    I <- match(IGBP[1], IGBPname)
    Tscalar <- (T-Tmax[I])*(T-Tmin[I]) / ( (T-Tmax[I])*(T-Tmin[I]) - (T - Topt[I])^2 )
    clamp(Tscalar, lims = c(0, 1))
}

#' @description
#' `LUE_Wscalar`: Water constrain
#'
#' @param d_mod09a1 data.frame with the columns of `site, date, year2, LSWI, QC_flag`
#' @param pheno_T data.frame with the columsn of `site, year2, sos_date, eos_date`.
#'
#' @rdname LUE
#' @export
LUE_Wscalar <- function(df, pheno_T) {
    df1 <- merge(df[, .(site, group, date, year2, LSWI, QC_flag)],
        pheno_T[, .(site, group, year2, sos_date, eos_date)], by = c("site", "group", "year2"))
    # d <- df1[site == sitename]

    d_LSWImax <- ddply(df1, .(site, group), cal_LSWImax)
    d_LSWImax[LSWI_max < 0.1, LSWI_max := 0.1]

    res <- merge(df, d_LSWImax, by = c("site", "group", "year2"), all.x = TRUE) %>%
        mutate(Wscalar = clamp((1 + LSWI) / (1 + LSWI_max), lims = c(0, 1)))
    res
}

# aggregate daily to 8-day, 16-day
process_gpp <- function(df_gpp, st_212){
    data <- df_gpp[, .(site, date, Rs = SW_IN, TA, TS, SWC, VPD, Prcp = P,
                      GPP_NT, GPP_DT, GPP = rowMeans(cbind(GPP_NT, GPP_DT), na.rm = TRUE))] %>%
        add_dn(days = c(8, 16))

    vars <- c("GPP_NT", "GPP")
    data[, (vars) := lapply(.SD, clamp_min), .SDcols = vars] # clamp_min to 0

    # aggregate daily to 8-day, 16-day
    data_d8  <- aggregate_dn(data, nday =  8, st_212)
    data_d16 <- aggregate_dn(data, nday = 16, st_212)
    list(d8 = data_d8, d16 = data_d16)
}
