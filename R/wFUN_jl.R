

wBisquare_julia <- function(y, yfit, w, ..., wmin = 0.2, 
    trs_high = 0.7,
    trs_low  = 0.4,
    trs_bg = 0.2, 
    .toUpper = TRUE)
{
    if (missing(w)) w  <- rep(1, length(y))
    wnew = JuliaCall::julia_call("phenofit.wBisquare", y, yfit, w,
        wmin = wmin,
        trs_high = trs_high,
        trs_low  = trs_low,
        trs_bg = trs_bg)
    return(wnew)
}
