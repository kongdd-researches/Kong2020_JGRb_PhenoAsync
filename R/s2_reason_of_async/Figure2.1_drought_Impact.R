# source('test/phenology_async/R/s2_reason_of_async/d_reason_of_async(INPUT).R')
library(pls)
source('test/phenology_async/R/main_async.R')
source('test/phenology_async/R/main_reason.R')
load("data_test/flux115_async_input.rda")
load("flux115_GPP&EVI_phenology.rda")

# MAIN FUNCTIONS ----------------------------------------------------------
sitename     <- "AU-DaP"
# sitename   <- "AT-Neu"
method       <- "ELMORE"
metric_async <- c("TRS1.eos", "TRS5.eos", "GU.DD", "GU.RD")
varnames <- .(Rn, Rs, VPD, VPD_trans, Prcp, T, EVI, NDVI, LSWI,
    Wscalar, Tscalar, APAR) %>% names()

res <- llply(sites_long, lda_analysis, .progress = "text") %>% rm_empty()

d_aov_index    <- map(res, ~.x$aov$info.sign$metric) %>% melt_list("site")
d_aov_duration <- map(res, ~.x$aov$info.sign$duration) %>% melt_list("site")



#'
#' This function only suit for one growing season region.
lda_analysis <- function(sitename){
    tryCatch({
        ## load data
        x <- df_mete[site == sitename]
        dpi       <- df_p[site == sitename & meth == method]
        dpi_async <- df_diff[site == sitename & meth == "ELMORE", .(async = mean(value, na.rm = T)), .(flag, year2)]

        res_aov <- aov_MeanDiff(dpi)

        # SCRIPTS -----------------------------------------------------------------
        his_pheno <- dpi[, .(value = median(value, na.rm = T)), .(type, index)] %>%
            spread(type, value)

        his_gpp <- his_pheno %$% set_names(GPPobs, index) %>% as.list()
        his_evi <- his_pheno %$% set_names(MOD13A1_EVI, index) %>% as.list()

        d_period     <- define_period(his_evi)
        d_period_lst <- transpose(d_period) %>% set_names(d_period$period)

        dpi_mete_period <- ddply(x, .(year2), function(d){
            d$dn <- d$dn[1] - 1 + seq_along(d$dn)
            ldply(d_period_lst, function(p){
                d[dn >= p$dn_begin & dn <= p$dn_end, lapply(.SD, mean, na.rm = TRUE), .SDcols = varnames]
            }, .id = "period")
        })

        dpi_MetePeriodInput <- ddply(dpi_mete_period, .(year2), function(d) unlist(d[, -(1:2)]) )
        dpi_Input.lda <- merge(dpi_async, dpi_MetePeriodInput)

        period_names <- dpi_mete_period[year2 == year2[1], period]
        ## PLS prediction

        corr <- dpi_Input.lda %>% dplyr::select(-matches("year|flag")) %>%
        {cor(.[, 1], .[, -1], use = "pairwise.complete.obs")} %>%
        {set_names(as.numeric(.), colnames(.))}
        r <- dpi_Input.lda %>% dplyr::select(-matches("year|flag")) %>%
            plsr(async ~ ., data = .)
        vip  <- VIP_pls(r) %>% .[nrow(.), ]
        coef <- coef(r)
        info_pls <- data.table(name = names(vip), vip, coef, corr) %>%
            .[, `:=`(var = str_extract(name, "[:alpha:]{1,}"),
                     period_id = str_extract(name, "\\d{1,}"))]
        info_pls[, period_name := period_names[as.integer(period_id)]]
        info_pls.sign <- info_pls[vip >= 0.8] #%>%
        # dplyr::select(-c("vip", "name", "period_id")) %>%
        # spread(period_name, coef)
        # print(info_pls.sign)
        list(aov = res_aov, pls = info_pls, pls.sign = info_pls.sign)
    }, error = function(e){
        message(sprintf("[%s] %s", sitename, e$message))
    })
}











# EVI has a long plateau period


d_stat <- x[, .(Prcp = sum(Prcp)*8,
    Rs = mean(Rs, na.rm = TRUE),
    T  = mean(T , na.rm = TRUE)), .(year2)]
vars <- colnames(d_stat)[-1]
d_stat[, (sprintf("%s_flag", vars)) := lapply(.SD, stat_mete), .SDcols = vars]; d_stat



dp_diff <- d_diff[site == sitename & meth == "ELMORE", .SD, .SDcols = c(1:8)[-7]] %>% spread(index, value)
dp_diff %<>% plyr::mutate(year2 = as.numeric(substr(flag, 1, 4)))

# merge(dp_diff, d_stat, by = "year2")[stat != "normal"]

a <- merge(dp_diff, d_stat, by = "year2") %>% melt_metrics()
a$async <- cut(a$value, c(-Inf, -8, 0, 8, Inf),
               labels = c("A", "-", "+", "D"))

library(klaR)
z <- partimat(async ~ Prcp + Rs + T, a[phase == "autumn"])
plot(z)

ggplot(a, aes(phase, value, color = Prcp_flag)) + geom_boxplot()




dp_doy <- df_p[site == sitename & meth == "ELMORE", 1:9] %>% spread(index, value)
dp_doy %<>% plyr::mutate(year2 = as.numeric(substr(flag, 1, 4)))

merge(dp_doy, d_stat, by = "year2")[stat != "normal"][order(type, year2)] # stat,


vars_com <- c("site", "type", "flag", "origin", "meth", "IGBP", "year2", "Prcp", "stat")



