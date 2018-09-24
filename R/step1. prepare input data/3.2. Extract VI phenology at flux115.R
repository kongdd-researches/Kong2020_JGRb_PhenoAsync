d     <- df[site == sitename & scale == "0m", .(t, date, y = EVI, w, SummaryQA)] #%T>% plotdata(365)
sp    <- st[site == sitename, ]

south <- sp$lat < 0 
d_new <- add_HeadTail(d, south = south)

nptperyear <- 23
INPUT <- with(dnew, check_input(t, y, w, nptperyear, maxgap = ceiling(nptperyear/12*1.5)))
INPUT$south <- sp$lat < 0
plotdata(INPUT)

# parameters for season_3y
threshold_max = 0.1
nf = 1

FUN_fit       <- "wWHIT"
threshold_max <- ifelse(cv_coef(d$y)[3] >= 1, 0.1, 0.2) # experience param
# FUN_fit <- ifelse(sp$IGBP %in% IGBP_forest, "wHANTS", "wWHIT")
wFUN <- wTSM# "wBisquare"
maxExtendMonth <- ifelse(sp$IGBP == "EBF", 2, 2)

# wFUN <- "wBisquare", "wTSM", threshold_max = 0.1, IGBP = CSH
# INPUT <- get_input(df, st, sitename)
brks2  <- season_3y(INPUT, south = INPUT$south, rFUN = get(FUN_fit),
               wFUN = wFUN,
             IsPlot = IsPlot,
             lambda = 10,
             iters = 2,
             minpeakdistance = nptperyear/6,
             MaxPeaksPerYear = 3,
             MaxTroughsPerYear = 4,
             ypeak_min = 0.08, 
             IsOnlyPlotbad = FALSE
## Get curve fitting
