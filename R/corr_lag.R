# 异步滞后检测程序，仅适用于单站点
#' Lag correlation between two variables
#'
#' @param x,y A numeric vector
#' @param index A numeric vector, the subscript of x, e.g. `ydn`
#' @param dates Dates vector, dates of x
#' @param doy_lims constrain index in the range of `period`
#' @param lag The step of lag time
#'
#' @return
#' - `cor`: pearson correlation of lag step
#' - `lag`:
#'  * positive: y is earlier;
#'  * negative: x is earlier.
#'
#' @examples
#' # examples
#' x = sin(1:365)
#' y = x
#' index = 1:365
#' dates = seq(as.Date('2010-01-01'), as.Date('2010-12-31'), 'day')
#' corr_lags(x, y, index, dates, lag = 1)
#'
#' @note
#' correlation needs to be adjusted because the length of observation is different
#'
#' @export
corr_lag <- function(x, y = x, lag = 1, index = NULL, dates = NULL, doy_lims,
    verbose = FALSE)
{
    if (missing(index) || is.null(index)) index = seq_along(x)

    if (is(dates[1], "Date")) {
        if (!missing(doy_lims)) {
            doys = yday(dates)
            # how to escape leap year?
            if (lag > 0) {
                doy_lims[2] %<>% add(lag)
            } else {
                doy_lims[1] %<>% add(lag)
            }
            ind = doys >= doy_lims[1] & doys <= doy_lims[2]
            x = x[ind]
            y = y[ind]
            index = index[ind]
            # dates = dates[index]
        }
    }
    # browser()
    # I0 <- x$ydn
    y_lag <- find_x_lag(y, lag, index)
    if (verbose) {
        if (!missing(dates))
        d = data.table(date = dates, index, x, y_lag)
        print(d)
    }

    # cbind(value, value_lag)
    tryCatch({
        cor(x, y_lag, use = "pairwise.complete.obs")
    }, error = function(e) {
        message(sprintf('%s', e))
        NA_real_
    })
}

#' @param ... other parameters to [corr_lag()]
#'
#' @rdname corr_lag
#' @export
corr_lags = function(x, y, lags = -5:5, index, dates = NULL, doy_lims, ...) {
    corr = foreach(lag = lags, i = icount(), .combine = c) %do% {
        corr_lag(x, y, lag, index, dates, doy_lims, ...)
    }
    df = data.table(lag = lags, corr)
    df.max = df[which.max(corr), ]
    list(df = df, max = df.max)
}

#' @rdname corr_lag
#' @export
find_x_lag <- function(x, lag, index) {
    I_lag <- match(index - lag, index)
    x[I_lag]
}

get_dn <- function(dates, days = 8){
    yday(dates) %>% {
        ceiling(. / days)
    }
}
