
get_gof <- function(par, df) {
    A = par[1]
    p = par[2]

    df[,  dhour_norm := (dhour2/max(dhour2))^p, .(site)]
    gof <- df[, as.list(GOF(GPP, PAR_TOA/100 * (EVI.whit - 0.08) * dhour_norm * A))]
    # -gof$NSE
    gof
}

get_GPP <- function(par, df) {
    A = par[1]
    p = par[2]
    
    df[, dhour_norm := (dhour2 / max(dhour2))^p, .(site)]
    
    GPPsim <- df[, (EVI.whit - 0.08) * dhour_norm * A]
    GPPsim
}


goal <- function(par, ...) {
    gof = get_gof(par, ...)
    -gof$NSE#R2
}

