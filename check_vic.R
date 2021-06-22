{
    load("D:/Documents/WeChat Files/wxid_udml2ofatec521/FileStorage/File/2021-02/the soil param for hanjiang.RData")
    df = data.table(soil)

    # df[MOIST_1 < WpFT1, .(MOIST_1, WpFT1, MOIST_2, WpFT2, MOIST_3, WpFT3)]
    # df[MOIST_2 < WpFT2, .(MOIST_1, WpFT1, MOIST_2, WpFT2, MOIST_3, WpFT3)]
    # df[MOIST_3 < WpFT3, .(MOIST_1, WpFT1, MOIST_2, WpFT2, MOIST_3, WpFT3)]

    ind = df[, which(MOIST_1 < WpFT1 | MOIST_2 < WpFT2 | MOIST_3 < WpFT3)]
    delta = 0.05
    df[ind, `:=`( MOIST_1 = WpFT1 + delta,
                  MOIST_2 = WpFT2 + delta,
                  MOIST_3 = WpFT3 + delta)]
    soil = df
    load("D:/Documents/WeChat Files/wxid_udml2ofatec521/FileStorage/File/2021-02/the forcing param for hanjiang.RData")
    load("D:/Documents/WeChat Files/wxid_udml2ofatec521/FileStorage/File/2021-02/the veg param for hanjiang.RData")
}

df[WpFT1 > WcrFT1, ]
df[WpFT2 > WcrFT2, ]
df[WpFT3 > WcrFT3, ]

library(VICmodel)
{
    library(lubridate)
    vic_params <- function(options){
        global_params <- getOption("VIC_global_params")


        for (i in seq_along(options)) {
            value = options[[i]]
            name = names(options)[i]

            if (name == "date_start") {
                global_params$start_year  <- year(value)
                global_params$start_month <- month(value)
                global_params$start_day   <- day(value)
            } else if (name == "date_end") {
                global_params$end_year  <- year(value)
                global_params$end_month <- month(value)
                global_params$end_day   <- day(value)
            } else {
                global_params[[name]] <- value
            }
        }
        options(list(VIC_global_params = global_params))
        invisible()
    }


    # Set the global options for a 7-days run.
    vic_params(list(
        date_start = "2000-01-01",
        date_end = "2000-12-31",
        step_per_day = 1,
        snow_step_per_day = 1,
        runoff_step_per_day = 1
    ))

    out_info <- list(
        # wb = list(timescale = 'hour', aggpar = 6,
        #           outvars = c('OUT_RUNOFF', 'OUT_BASEFLOW', 'OUT_SOIL_MOIST'),
        #           aggtypes = c('sum', 'sum', 'end')),
        eb = list(timescale = 'day', aggpar = 1,
                  outvars = c('OUT_SWE', 'OUT_SOIL_TEMP'),
                  aggtypes = c('avg', 'min'))
    )
    res = vic(forcing, soil, veg, output_info = out_info, parall = FALSE)
}


"BULKDN1" = d_HWSD$T_BULK_DEN,
"BULKDN2" = d_HWSD$T_BULK_DEN, "BULKDN3" = d_HWSD$S_BULK_DEN,
"PARTDN1"

df[, ]

