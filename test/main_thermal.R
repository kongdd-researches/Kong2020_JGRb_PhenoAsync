#' n_continue
#' @param x Numeric vector
#' @param n Find the position of first n continous true \code{con}.
#' @param type 'sos' means the left I (spring case),
#' 'eos' means the right I (autumn case)
n_continue <- function(x, trs = 0, n = 3, type = 'sos'){
    half_win <- floor(n/2)
    pop <- which.max(x) # whether T.pop or NDVI.pop?
    len <- length(x)

    # 1th: copy the idea of YaoZhang
    if (type == 'sos'){
        con <- x >= trs
        I <- zoo::rollapply(con, n, prod, partial=T) %>% {which(.==1)}
        I <- I[I < pop]
        I0 <- max(1, first(I) - half_win) # to get first data, `-half-win` is essential

        # If all T < 5deg, get LSWImax on the day of NDVImax - 1.
        if (is.na(I0)) I0 <- pop-1
    } else if (type == 'eos') {
        con <- x <= trs
        I <- zoo::rollapply(con, n, prod, partial=T) %>% {which(.==1)}
        I <- I[I > pop]
        I0 <- max(1, first(I) - half_win - 1)

        # if all T >=10deg, eos = len
        if (is.na(I0)) I0 <- len
    }

    # 2th solution: run function
    # r <- rle(con)
    # I <- c(0, cumsum(r$lengths))+1

    # I_true <- which(r$values)
    # I0 <- I_true[r$lengths[I_true] >= n]
    I0
    # list(pos = I0, val = x[I0])
}

#' Pheno_thermal
#' 
#' Thermal phenology based on temperature data.
#' 
#' @param x Numeric vector, temperature
#' @param t Date vector, corresponding date of \code{x}
#' @param n Integer, continous days of T >= trs[1] or T <= trs[2]
#' @param adjust If true: 
#' 1. high latitude boreal regions will be fixed. If \code{all(T <= trs[1])}, 
#' growing season is set to Jun-Aug.
#' 2. sos >= eos will be fixed, sos = pop-1; eos = pop+1.
Pheno_thermal <- function(x, t, trs = c(5, 10), n = 3, adjust = T){
    if (length(trs) == 1) trs[2] <- trs[1]
    pop <- which.max(x) # whether T.pop or NDVI.pop?

    # print(trs)
    sos = n_continue(x, trs=trs[1], n, type='sos')
    eos = n_continue(x, trs=trs[2], n, type='eos')
    
    # 2.1 for high latitude boreal regions
    if (adjust && all(x <= trs[1], na.rm = T)){
        I <- which(month(t) %in% 6:8)
        sos <- first(I)
        eos <- last(I)
    }

    # 2.2 if sos >= eos
    if (adjust && sos >= eos){
        sos <- pop - 1
        eos <- pop + 1
    }

    # index to date
    list(sos_date = t[sos], sos_val = x[sos], 
         eos_date = t[eos], eos_val = x[eos])
}

add_Head <- function(d){
    nptperyear <- ceiling(365/as.numeric(difftime(d$date[2], d$date[1], units = "days")))
    d_head <- d[(nptperyear*0+1):(nptperyear*3+0)]
    nyear  <- floor(nrow(d_head)/nptperyear)

    d_head$date %<>% `-`(years(nyear))
    d_head[, year := year(date)]

    res <- rbind(d_head, d)
    res
}