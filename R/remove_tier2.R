#' @export
remove_tier2_years <- function(df){
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
    df    
}

# RemoveTier2 (previous name)
# move tier2 into a separate directory
move_tier2 <- function(){
    files <- dir("Z:/fluxsite212/raw/all", recursive = T, pattern = "*.SUBSET_[HR|HH]_*.", full.names = T)
    
    stations_166 <- fread("F:/Github/MATLAB/PML/data/flux_166.csv")
    I    <- match(stations_166$site, substr(basename(files), 5, 10))
    dirs <- dirname(files)[-I]
    file.rename(dirs, paste0("Z:/fluxsite212/raw/tier2/", basename(dirs)))
    ## remove tier2
    
    files <- dir("Z:/fluxsite212/daily/", full.names = T, pattern = "*.csv$")
    I     <- match(stations_166$site, substr(basename(files), 5, 10))
    file.remove(files[-I]) #only 166 left    
    files <- files[I] %>% set_names(stations_166$site)
}
