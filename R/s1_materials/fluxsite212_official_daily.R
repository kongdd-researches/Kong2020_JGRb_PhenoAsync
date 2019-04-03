source("R/FLUXNET/Main.R")
library(plyr)
library(Ipaper)

stations_166 <- fread("F:/Github/data/flux/flux_166.csv")
stations     <- fread("F:/Github/data/flux/flux-212.txt")

# remove tier2 data
I <- match(stations_166$site, stations$site)
I        <- match(stations_166$site, substr(basename(files_DD), 5, 10))

files <- dir("Z:/fluxsite212/raw/SUBSET/tier1/", recursive = T, pattern = "*.SUBSET_DD_*.", full.names = T)
files <- files[I] %>% set_names(stations_166$site)

file <- files[1]

## fixed the error of newdata and growing season flag variable, in North and 
# South Hemisphere (20180902)

## Known issues
# 2015 patch is not applied. LE, LH will lost some good values.

#' For every day, Only 80% valid measures were left, others were set to NA. 
tidy_dd_official <- function(file, lat){
    dt <- fread(file, na.strings = "-9999") #-9999 has been replaced with na
    
    date <- ymd(as.character(dt$TIMESTAMP))
    n    <- length(date)
    
    # add newdate to make sure its complete year for North and South Hemisphere
    if (lat < 0){
        date_begin <- ymd(sprintf("%d-%d-%d", year(date[1]) - 1, 7, 1))
        date_end   <- ymd(sprintf("%d-%d-%d", year(date[n]) + 1, 6, 30))
    } else {
        date_begin <- ymd(sprintf("%d-%d-%d", year(date[1]), 1, 1))
        date_end   <- ymd(sprintf("%d-%d-%d", year(date[n]), 12, 30))
    }
    newdate <- seq.Date(date_begin, date_end, by = "day")
    
    ## 1. add Year, MM, growing
    year  <- year(newdate)
    month <- month(newdate)
    
    # I_summer <- month %in% 6:8
    # I_winter <- month %in% c(12, 1, 2)
    
    # North Hemisphere, growing: 4-10 (Wang Xuhui, 2011, PNAS)
    # South Hemisphere, growing: 10, 11, 12, 1, 2, 3, 4
    
    if (lat < 0){
        growing <- month <= 4 | month >= 10
        year <- year - 1 + (month >= 7)
    }else{
        growing <- month %in% 4:10
    }
    growing <- as.numeric(growing)
    
    # 2. select valid obs
    # df <- suppressMessages(read.csv(unz(file, file_csv))) 
    Id_na <- which(is.na(match(vars_all, colnames(dt))))
    # df[, vars_all[Id_na]] <- NA #this step must be data.frame variables
    if (length(Id_na) > 0) dt[, (vars_all[Id_na]) := NA]
    
    x_val <- dt[, vars_val, with = F]
    x_qc  <- dt[, vars_QC , with = F]
    x_val[x_qc  < 0.8] <- NA
    
    I <- match(newdate, date)
    date <- newdate
    # rename
    setnames(x_val, gsub("_F$|_F_MDS$|_F_MDS_1|_VUT_REF", "", names(x_val)) %>% 
                 gsub("NETRAD", "Rn", .))
    df   <- cbind(date, year, month, growing,  x_val[I, ])
    df #quickly return    # 3. remove all na variables in tail and head
    # flag <- rowSums(is.na(as.matrix(x_val))) < ncol(x_val)
    # 
    # ## remove small segments in values
    # r <- rle(flag)
    # # print(r$lengths[r$values])
    # r$values[r$values][r$lengths[r$values] <= 3]  <- FALSE
    # flag <- inverse.rle(r)
    # 
    # I <- which(flag) %>% {first(.):last(.)}
    # df[I, ] #quickly return
}
# xlst <- mapply(tidy_dd, files_DD, stations_166$lat)

# check file and station order first
basename(files) %>% substr(5, 10) %>% match(stations_166$site) %>% diff() %>% unique()

xlst <- mlply(data.frame(file = files, lat = stations_166$lat, stringsAsFactors = F), 
              tidy_dd_official, .progress = "text")
names(xlst) <- stations_166$site

df <- phenofit::melt_list(xlst, 'site')
df$YYYY <- year(df$date)

# remove site-years of tier2 ----------------------------------------------
sites <- c('AU-ASM' ,'AU-TTE' ,'AU-WOM' ,'CZ-BK1' ,'CZ-BK2' ,'FR-GRI' ,'NL-LOO' ,'US-PRR' ,'ZA-KRU')
years <- c(2014, 2014, 2013, 2009, 2007, 2014, 2014, 2014, 2011)
expr  <- sprintf('(site == "%s" & YYYY >= %s)', sites, years) %>% paste(collapse = " | \n")

date  <- df$date
# in tier2, or leapyear 2-29 then delete it
I_del <- eval(parse(text = expr), envir = df) | (month(date) == 2 & day(date) == 29)

# I_del <- with(df, (site == "AU-ASM" & year >= 2014) | 
#                (site == "AU-TTE" & year >= 2014) | 
#                (site == "AU-WOM" & year >= 2013) | 
#                (site == "CZ-BK1" & year >= 2009) | 
#                (site == "CZ-BK2" & year >= 2007) | 
#                (site == "FR-GRI" & year >= 2014) | 
#                (site == "NL-LOO" & year >= 2014) | 
#                (site == "US-PRR" & year >= 2014) | 
#                (site == "ZA-KRU" & year >= 2011))
df <- df[!I_del, 1:(ncol(df) - 1)]

# also need to remove leapyear 2-29;
fwrite(df, 'data/fluxsites166_official_dd.csv')

## add years and growing season