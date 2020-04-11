source("test/main_pkgs.R")

sp <- st_166[site %in% sites] %>% df2sp()
writeOGR(sp, "3 flux in TP.kml", "station", driver = "KML")
## update 20200322 -------------------------------------------------------------
varname = "GPP_NT"
version = glue("({varname}) v0.2.6.9000") # test version

file_pheno_full = glue("INPUT/pheno_flux166_full {version}.rda")
file_pheno_prim = glue("INPUT/pheno_flux95_prim ({version}).rda")
file_brks = glue("INPUT/pheno_gpp_st109 {version}.rda")

# metrics_all = c("Greenup", "TRS1.sos", "UD", "TRS2.sos", "DER.sos", "TRS5.sos", "TRS6.sos", "TRS8.sos", "SD", "Maturity",
#                 "Senescence", "DD", "TRS8.eos", "TRS6.eos", "TRS5.eos", "DER.eos", "TRS2.eos", "RD", "TRS1.eos", "Dormancy")
metrics_all = metrics_select
indexNames  = c("RMSE", "MAE", "Bias")

## load phenology data
# 1. 准备输入数据
{
    load(file_pheno_prim); rm(st)
    x = melt(df_VI_prim, c("sate", "type_VI", "group", "site", "flag", "origin", "meth"),
             value.name = "y_sim")
    y = melt(df_gpp_prim, c("site", "flag", "origin", "meth"), value.name = "y_obs")
    # x$flag %<>% as.character()
    # y$flag %<>% as.character()
    df = merge(x, y, by = c("site", "flag", "origin", "meth", "variable")
               # all.x = TRUE
    )[, diff := y_sim - y_obs]
    df$type_period = "others"
    df[variable %in% metric_spring, type_period := "Green-up season"]
    df[variable %in% metric_autumn, type_period := "Withering season"]

    products     = c("Terra_EVI", "Aqua_EVI", "combined_EVI", "Terra_NDVI", "Aqua_NDVI", "combined_NDVI", "combined_LAI")
    products_fix = c("Terra_EVI", "Aqua_EVI", "Combined_EVI", "Terra_NDVI", "Aqua_NDVI", "Combined_NDVI", "Combined_LAI")
    df[, product := sprintf("%s_%s", gsub("df_", "", sate), type_VI)]
    df$product %<>% factor(products, products_fix)
}

metrics <- colnames(df_gpp_prim)[-(1:4)]
d_gpp <- df_gpp_prim[meth %in% c("Beck", "Elmore") & site %in% sites,lapply(.SD, mean), .(site, flag), .SDcols = metrics]
d_vi <- df_VI_prim[meth %in% c("Beck", "Elmore") &site %in% sites,lapply(.SD, mean), .(site, flag, type_VI), .SDcols = metrics] %>%
    {merge(. , d_gpp[, 1:2])[order(type_VI, site, flag)]}

d_gpp[, .(site, flag)]

df_gsl = merge(
    get_gsl(df_VI_prim, value.name = "y_sim"),
    get_gsl(df_gpp_prim, value.name = "y_obs")
) %>% plyr::mutate(diff = y_sim - y_obs)
df_gsl$type_VI %<>% factor(names_VI)

per_bad = sum(abs(df$diff) >= 60, na.rm = TRUE)/nrow(df)
per_bad

# metric_spring <- contain(df_gpp, "sos|UD|SD|Greenup|Maturity")
# metric_autumn <- contain(df_gpp, "eos|DD|RD|Senescence|Dormancy")

# about 15% has a MAE  > 60
# 2.
# Even trough we have dealed with growing season dividing very carefully, there are still 5.5% phenological metrics has a absolute error higher than 90d. If absolute difference of $y_{sim}$ and $y_{obs}$ is high that 90d (about 3 month), it might be introduced by the error of growing season dividing. Hence, those phenological metrics are excluded when calculating the goodness performance.

## 2.1 不同VI, 不同curve fitting methods的表现 （数据准备）---------------------

include.r = FALSE
# d = df[abs(diff) < 60, as.list(GOF(y_obs,y_sim, include.r = include.r)),
#        .(sate, type_VI, meth, site, variable, type_period)]
d = df[abs(diff) < 60, as.list(GOF(y_obs,y_sim, include.r = include.r)),
       .(product, meth, site, variable, type_period)]

# 不同植被指数
d_vi = df[abs(diff) < 60, as.list(GOF(y_obs,y_sim, include.r = include.r)), .(product, type_period, site)]
# 不同Curve fitting methods
d_meth = df[abs(diff) < 60, as.list(GOF(y_obs,y_sim, include.r = include.r)), .(meth, type_period, site)]
names(d_vi)[1] = "x"
names(d_meth)[1] = "x"

d_comp <- list("Curve fitting methods" = d_meth,
               "Remote sensing vegetation indices" = d_vi[!(x %in% c("Combined_LAI")) & !is.na(x), ]) %>%
    melt_list("type_comp")

sites <- c("CN-Dan", "CN-Ha2", "CN-HaM")


## Figure 3. methods and products ----------------------------------------------
## 2.2 不同VI, 不同curve fitting methods的表现 （数据准备）---------------------
{
    source("test/main_vis.R")
    d_fig1 = d_comp %>% melt(c("x", "type_period", "type_comp", "site"), variable.name = "index")
    d_gof  = d_fig1[type_period != "others", as.list(stat_sd(value)),
                    .(x, type_period, type_comp, index)][, label := sprintf("%.1f±%3.1f", y, sd)]
    d_lab = expand.grid(type_comp = unique(d_gof$type_comp), type_period = unique(d_gof$type_period))
    d_lab$label = sprintf("(%s)", letters[1:nrow(d_lab)])

    temp <- foreach(indexName = indexNames, i = icount()) %do% {
        runningId(i)
        d_i     = d_fig1[type_period != "others" & index == indexName]
        d_gof_i = d_gof[type_period != "others" & index == indexName]
        {
            p <- ggplot(d_i, aes(x, value))
            p <- p +
                stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.5) +
                geom_boxplot2(aes(fill = x), notch = TRUE, outlier.shape = NA, coef = 0, width = 0.8,
                              show.legend = FALSE) +
                geom_blank(data = d_gof_i, aes(x, y = ymax + 2.4)) +
                geom_text(data = d_gof_i, # [type_comp == "Curve fitting methods"]
                          aes(x, ymax, label = label), vjust = -0.5, size = 3.5) +
                geom_text(data = d_lab, aes(-Inf, Inf, label = label), hjust = -0.5, vjust = 1.2,
                          size = 6, fontface = 2, family = "Times") +
                theme(panel.grid.major = element_blank(),
                      axis.text.x = element_text(angle = 30, hjust = 1),
                      strip.background = element_rect(colour = "black", size = 0.1),
                      panel.background = element_rect(fill = "white", colour = "grey", size = 0.1)) +
                facet_grid(type_period~type_comp, scales = "free_x") +
                labs(x = NULL, y = glue("{indexName} (days)"))
            if (indexName == "Bias")
                p <- p + geom_hline(yintercept =  0, color = "blue", linetype = 2)
            else
                p <- p + geom_hline(yintercept = 20, color = "red", linetype = 2)
            g = ggplot_gtable(ggplot_build(p))
            g$widths[5] %<>% multiply_by(4/6) # 5/7
            # grid.draw(g)

            outfile = glue("Figure3_methods_products {indexName} {version}.pdf") #pdf
            write_fig(g, outfile, 10, 6)
            # p
        }
    }
}

## Figure S4 -------------------------------------------------------------------
{
    library(scales)
    d = df[abs(diff) < 60, .(DOY = mean(y_obs, na.rm = TRUE)), .(type_period, variable, site)]
    colors_period <- hue_pal()(2) %>% rev()

    d$variable %<>% factor(metrics_all)
    d <- d[!is.na(variable), ]

    stat_hline <- function(x) {
        x <- stats::na.omit(x)
        c(yintercept = median(x))
    }

    p <- ggplot(d, aes(variable, DOY, fill = type_period)) +
        stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.5) +
        geom_boxplot2(notch = FALSE, outlier.shape = NA, coef = 0, width = 0.8) +
        theme(legend.position = c(0.02, 0.998),
              legend.justification = c(0, 1),
              panel.grid.major.x = element_line(size = 0.2),
              panel.grid.major.y = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        geom_vline(xintercept = c(2, 5, 7, 9, 12) + 0.5, linetype = 2, size = 0.1) +
        scale_fill_manual(values = colors_period) +
        labs(x = NULL, y = "Day of year (DOY)")
    outfile = glue("FigureS4_time distribution of phenological metrics {version}.pdf")
    write_fig(p, outfile, 9, 5)
    # write_fig(p, gsub(".pdf", ".tif", outfile), 9, 5)
}

