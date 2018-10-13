# source('test/phenology_async/R/s2_reason_of_async/d_reason_of_async(INPUT).R')
load("data_test/flux115_async_input.rda")

df <- d_mod09a1

stat_mete <- function(x){
    levels <- c("small", "normal", "high")

    qval <- quantile(x,  probs = seq(0.25, 0.75, 0.25), na.rm = T)
    flag <- rep("normal", length(x))

    flag[x <= qval[1]] <- levels[1]
    flag[x >= qval[3]] <- levels[3]
    factor(flag, levels)    
}


melt_metrics <- function(d){
    vars <- intersect(metrics, colnames(d))
    res <- melt(d, measure.vars = vars, variable.name = "index")
    res$phase <- metric_phase(res$index)
    res
} 


sitename  <- "AT-Neu"
x <- df[site == sitename]
d_stat <- x[, .(Prcp = sum(Prcp)*8, 
    Rs = mean(Rs, na.rm = TRUE), 
    T  = mean(T , na.rm = TRUE)), .(year2)]
vars <- colnames(d_stat)[-1]
d_stat[, (sprintf("%s_flag", vars)) := lapply(.SD, stat_mete), .SDcols = vars]; d_stat



dp_diff <- d_diff[site == sitename & meth == "ELMORE", .SD, .SDcols = c(1:8)[-7]] %>% spread(index, value)
dp_diff %<>% plyr::mutate(year2 = as.numeric(substr(flag, 1, 4)))

# merge(dp_diff, d_stat, by = "year2")[stat != "normal"]

a <- merge(dp_diff, d_stat, by = "year2") %>% melt_metrics()

ggplot(a, aes(phase, value, color = stat)) + geom_boxplot()



dp_doy <- df_p[site == sitename & meth == "ELMORE", 1:9] %>% spread(index, value)
dp_doy %<>% plyr::mutate(year2 = as.numeric(substr(flag, 1, 4)))

merge(dp_doy, d_stat, by = "year2")[stat != "normal"][order(type, year2)] # stat, 


vars_com <- c("site", "type", "flag", "origin", "meth", "IGBP", "year2", "Prcp", "stat")
