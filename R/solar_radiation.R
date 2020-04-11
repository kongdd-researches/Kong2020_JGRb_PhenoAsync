#' Extraterrestrial radiation
#' 
#' @param lat latitude
#' @param doy doy is the number of the day in the year between 1 (1 January) 
#' and 365 or 366 (31 December)
#' 
#' @return Ra, MJ m-2 d-1
#' @export 
cal_Ra <- function(lat, doy) {
    # solar declination, rad (1 rad = 57.2957795 deg)
    delta <- 0.409*sin(0.0172*doy-1.39)
    # relative distance Earth-Sun, []
    dr <- 1 + 0.033*cos(0.0172*doy)
    # sunset hour angle, rad
    latr <- lat/57.2957795 # (180/pi)
    sset <- -tan(latr)*tan(delta)
    omegas <- sset*0
    omegas[abs(sset)<=1] <- acos(sset[abs(sset)<=1])
    # correction for high latitudes
    omegas[sset<(-1)] <- max(omegas)
    # Ra, MJ m-2 d-1
    Ra <- 37.6*dr*(omegas*sin(latr)*sin(delta)+cos(latr)*cos(delta)*sin(omegas))
    Ra <- ifelse(Ra<0,0,Ra)
    Ra
}

# W = J/s
MJ_2W  <- function(x) {
    x/86400*1e6
}

W2_MJ <- function(x) {
    x/1e6*86400
}

w2mm <- function(LE, tmean) {
    Cp <- 4.2 * 0.242 # specific heat at constant pressure, 1.013 [kJ kg-1 0C-1]
    lamada <- 2500 - 2.2 * tmean
    LE / lamada * 86400 * 10^-3 # W M-2 to mm
}
