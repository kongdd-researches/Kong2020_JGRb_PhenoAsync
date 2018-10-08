
# In the order of IGBPname_006
epsilon_C3 <- 0.42
epsilon_C4 <- c(rep(0, 8), rep(0.63, 4), 0, 0.63)

cal_Tscalar <- function(T, IGBP){
    IGBPname <- c("ENF", "EBF", "DNF", "DBF", "MF" , "CSH",
                "OSH", "WSA", "SAV", "GRA", "WET", "CRO",
                "URB", "CNV")
    Tmin <- c(-1, -2, -1, -1, -1, -1, 1, -1, 1, 0, -1, -1, 0, 0)
    Tmax <- c(40, 48, 40, 40, 48, 48, 48, 48, 48, 48, 40, 48, 48, 48)
    Topt <- c(20, 28, 20, 20, 19, 25, 31, 24, 30, 27, 20, 30, 27, 27)

    I <- match(IGBP, IGBPname)
    Tscalar <- (T-Tmax[I])*(T-Tmin[I]) / ( (T-Tmax[I])*(T-Tmin[I]) - (T - Topt[I])^2 )
}

# make sure Tscalar and Wscaler in the range of 0-1
clamp <- function(x, lims = c(0, 1)){
    x[x < lims[1]] <- lims[1]
    x[x > lims[2]] <- lims[2]
    x
}
# param <- data.table(IGBPname, Tmin, Tmax, Topt)
# Tscalar <- (T-Tmax)*(T-Tmin) / ( (T-Tmax)*(T-Tmin) - (T - Topt)^2 )
# Wscaler <- (1 + LSWI) / (1 + LSWI_max)

## 1 tidy MOD09A1
df <- readRDS("data_test/MOD09A1_VI_flux212.RDS")
df <- df[scale == "0m", .(site, t, date, EVI, EVI2, NDVI, LSWI, w, StateQA, QC_flag)]

# 1.1 make sure values in a reasonable range
df[ y > 1 | y < -0.1, y := NA]
# 1.2 remove outliers: abs(y - mean) > 3sd
df[!is.na(y), `:=`(mean = mean(y), sd = sd(y)), .(site)]
df[abs(y - mean) >= 3*sd & QC_flag != "good", y := NA_real_, .(site)]
df[, c("mean", "sd") := NULL]

df[QC_flag %in% c("cloud", "snow"), EVI := EVI2] # fix bright EVI
# with(, plot(EVI2, y)); grid(); abline(a = 0, b=1, col="red")

## 
LWSI_max <- df[, .(LSWI_max = max(LSWI, na.rm = T)), .(site, year, scale)]
LWSI_max[, LSWI_max := zoo::rollapply(LSWI_max, 5, nth_max, partial = T),
          .(site, scale)] # 5 year second maximum moving

df <- merge(df, LWSI_max)
df[, Wscaler := (1 + LSWI) / (1 + LSWI_max)]





