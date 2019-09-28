#' @import data.table magrittr fasttime
NULL
# lubridate

# Update 13 Sep, 2018
# -------------------
# It's the 24-hour data used to aggregate daily meteorological and biological 
# variables, not daytime. Daytime was only used to get day time hour (dhour).  
# 
# Update 30 Nov, 2017; Dongdong Kong
#   Fixed Soil heat flux G error, G can have negative values.
#   NETRAD also could is negative; unchaneged due to only daytime flux considered
# 
# If daily validate obs < 50%, then set it and NA. This restriction has been writen in
# 	Rcpp mean_perc and sum_perc function
timename <- c('TIMESTAMP_START','TIMESTAMP_END')

# get_dailyData <- function(dir, type = c("HH", "HR")){
get_hourly_csv <- function(file_csv, outdir = "data/hourly/"){
    outfile <- check_file(file_csv, outdir)
    if (outfile == "") return;
    
    tryCatch({
        dt <- fread(file_csv, sep = ",", header = T, showProgress = F, verbose = F) #%>% as.data.frame()
        xt <- tidy_hourly(dt)
        fwrite(data.table(xt), file = outfile)
    }, 
    error = function(e) message(sprintf("%s", e)))
}

# manipulate hourly data after the processing of get_hourly
get_daytime_csv <- function(file_csv, outdir = "data/daytime/"){
	outfile <- check_file(file_csv, outdir)
    if (outfile == "") return;

    dt      <- fread(file_csv, sep = ",")
    x_daily <- tidy_daytime(dt)
	fwrite(data.table(x_daily), file = outfile)
	# return(x_daily)
}

# combine 2 steps: tidy hourly data and aggregate into daytime
get_daytime <- function(file_csv, outdir = "data/daytime/"){
    outfile <- check_file(file_csv, outdir)
    if (outfile == "") return();

    dt <- fread(file_csv, sep = ",", header = T, showProgress = F, verbose = F) #%>% as.data.frame()
    # dt      <- fread(file_csv, sep = ",")
    
    xt      <- tidy_hourly(dt)  # hourly data
    x_daily <- tidy_daytime(xt) # daytime data
    
    fwrite(data.table(x_daily), file = outfile)
}


# 1. negative values are set to NAN              | vars_noNeg
# 2. negative and zero values are set to NAN     | vars_0na
# 3. QC > 2 set to NAN.
tidy_hourly <- function(dt){
    # df <- read.csv(file_csv)
    date <- dt[, 1:2]
    # df <- suppressMessages(read.csv(unz(file, file_csv))) 
    Id_na <- which(is.na(match(vars_all, colnames(dt))))
    # df[, vars_all[Id_na]] <- NA #this step must be data.frame variables
    if (length(Id_na) > 0) dt[, (vars_all[Id_na]) := NA]
    
    x_val <- dt[, vars_val, with = F]

    # qc could be -9999 sometimes
    x_qc  <- dt[, vars_QC , with = F]
    x_val[x_qc  > 2] <- NA
    
    # manipulate NA values
    for (i in seq_along(vars_val)) {
        var <- vars_val[i]
        
        I_0 <- numeric() #error fix, 17 Dec' 2017
        if (var %in% var_no_negative){
            # variables x < 0 set to be 0
            I_0 <- which(x_val[[i]] < 0)
        }
        # if (var %in% vars_0na) {
        #     # variables x <= 0 set to be 0
        #     I_0 <- which(x_val[[i]] < 0)
        # } else if (var %in% vars_noNeg) {
        #     # variables x < 0 set to be 0
        #     I_0 <- which(x_val[[i]] < 0)
        # }
        # else variables (TA_F, TS_F, NEE) x < -9000 set to be NA 
        I_na <- which (x_val[[i]] <= -9000 | x_qc[[i]] > 2) #should be caution the short form
        
        if (length(I_0 ) > 0) set(x_val, i = I_0 , j = i, value=NA) # 09 Mar, 2018
    	if (length(I_na) > 0) set(x_val, i = I_na, j = i, value=NA)
    }
    x <- cbind(date, x_val) #add tiemstamp variables, fixed error here
}

tidy_daytime <- function(dt){
    ## 1. transform timestamp
    fmt_date   <- "%Y%m%d%H%M"
    time_begin <- as.character(dt$TIMESTAMP_START) %>% fast_strptime(fmt_date)
    time_end   <- as.character(dt$TIMESTAMP_END)   %>% fast_strptime(fmt_date)

    deltaT   <- unique(difftime(time_end, time_begin, units = "hours"))  # for HH dt = 1/2, HR dt = 1
    # print(deltaT)

    date <- date(time_begin)
    # date_end   <- date(time_end)
    time_begin %<>% format("%H:%M") #%S
    time_end   %<>% format("%H:%M")

    vars_date <- c("date", "time_begin", "time_end") #date variables
    dt[, (vars_date) := list(date, time_begin, time_end)]
    dt[, c("TIMESTAMP_START", "TIMESTAMP_END") := NULL]
    
    ## 2. vars can't have negative values, have been manipulated in get_daily
    ## 3. get daily prcp
    # prcp <- dt[, .(P_F = sum(P_F, na.rm = T)), by = date] #mm into mm/day
    prcp <- dt[, .(P_F = sum_perc(P_F)), by = date]
    ## 4. day light hours according to shortwave incoming radiation > 5/W/m2
    dt_dlight <- dt[SW_IN_F >= 5, ] #modified 29112017

    # dhour     <- dt_dlight[, .N, by = date]$N * deltaT  #day light hours
    dhour     <- dt_dlight[, .(dhour = .N * deltaT), by = date]
    dt_dlight <- dt
    
    # mean of all the variables except precipitation, and date variables
    x_daily   <- dt_dlight[, lapply(.SD, mean_perc), by = date,
                           .SDcols = setdiff(names(dt_dlight), c("P_F", vars_date))]
    x_daily[, ':='(VPD_F = VPD_F * 0.1)]        # hPa to kPa
    x_daily <- merge(dhour, x_daily, by = "date", all = T) #add dhour parameter
    
    # 5. Convert Carbon flux units (from umol/m2/s to g/m2/d)
    #    Only for Var = NEE, GPP, RE
    SDcols <- names(x_daily) %>% {.[grep("GPP_|NEE_|RECO_", .)]}
    x_daily[, (SDcols) := lapply(.SD, function(x) x <- x*1.0368), 
            .SDcols = SDcols] # 12*86400/10^6 = 1.0368

    # merge precipitation into data.table and rename colnames
    x_daily %<>% merge(prcp, by = "date")
    setnames(x_daily, names(x_daily), 
             gsub("_F$|_F_MDS$|_F_MDS_1|_VUT_REF", "", names(x_daily)) %>% 
             gsub("NETRAD", "Rn", .)) #rename Net Radiation
    setcolorder(x_daily, c("date","dhour", sort(names(x_daily[, -c("date", "dhour")]))))
    
    # 6 check the date continuity of x_daily, If not continue, then fill NAN 
    #   values in it.
    date <- x_daily$date
    vals <- diff(date)
    if (length(unique(vals)) != 1){
        # message(sprintf("%s", basename(file_csv)))
        print(rle(as.numeric(vals)))
        
        dateBegin <- date[1]
        dateEnd   <- date[length(date)]
        dates_new <- seq.Date(dateBegin, dateEnd, by = "day")
        days      <- difftime(dateEnd, dateBegin, "days") + 1
        cols      <- ncol(x_daily)
        tmp       <- matrix(NA, nrow = days, ncol = cols) %>% 
            set_colnames(names(x_daily))
        tmp[match(date, dates_new), ] <- as.matrix(x_daily)
        # tmp[, "date"] <- dates_new
        x_daily <- data.table(tmp)
        x_daily$date <- dates_new
        # y$date <- dates
        # print(unique(diff(as.Date(y$date))))
    }
    return(x_daily)
}
