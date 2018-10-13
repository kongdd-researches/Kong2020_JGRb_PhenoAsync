library(grid)
library(gridExtra)
library(ggpmisc)
# source('test/phenology_async/R/s1_materials/5.2 GPP_Elasticity.R')
source('test/phenology_async/R/main_async.R')
predictors <- c("EVI", "NDVI", "Rs", "T", "Prcp", "VPD", "APAR",
                "Wscalar", "Tscalar", "epsilon_eco", "epsilon_chl")#[-6]#[-c(1, 2)]

sitename <- "AT-Neu"#"CH-Oe2" #"AT-Neu"
d <- d_mod09a1 # all sites' data
x <- d[site == sitename] #  & SummaryQA <= 1, .SD, .SDcol = c("site", "date", "ydn", varnames)

## 2. filter sites with more than 5 year data
info <- d[, .(n = length(unique(year))), .(site)][n >= 5][order(-n)]
sites_long <- st[site %in% info$site]$site

#
info_async <- read.xlsx("table1.over_decouple.xlsx") %>% data.table()
info_async_long <- info_async[site %in% sites_long]

d_long <- d[site %in% sites_long]


# parameter for loess
smooth_formula <- y~poly(x, 2)
span <- 1

#' global variables:
#' smooth_formula, span
ggplot_1var <- function(x, varname = "APAR", color = "red"){
    p <- ggplot(x, aes_string("dn", varname, color = "year")) +
        # geom_point(color = "transparent") +
        geom_smooth(method = "loess", formula = smooth_formula, span = span,
                    color = color)+ #fill = color,
        scale_y_continuous(position = "right") +
        theme(panel.background = element_rect(fill = "transparent"),
              axis.ticks.y.right = element_line(color = color),
              axis.text.y.right = element_text(color = color),
              axis.title.y.right = element_text(color = color),
              panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank()) # get rid of minor grid
    p
}


# check elastic -----------------------------------------------------------


CairoPDF("Figures2_check_elastic_v4.pdf", 8.5, 12)

for (i in seq_along(sites_long)){
# for (i in 1:10){
    runningId(i)
    sitename <- sites_long[i]

    x <- d[site == sitename, ]
    x[, `:=`(epsilon_eco = GPP / (Rs*0.45),
             epsilon_chl = GPP / (Rs*0.45*1.25*(EVI-0.1)))]
    check_sensitivity(x, predictors)
}

dev.off()


d_temp <- info_async_long %>% melt(measure.vars = c("spring", "autumn"), variable.name = "phase")
d_temp$IGBP %<>% factor(IGBPnames_006)
d_temp$phase %<>% factor(c("spring", "autumn"))



    lwd <- 0.8
    colors <- scales::hue_pal()(2) %>% rev


    # geom_jitter(width = 0.2)

ggplot(d_temp, aes(IGBP, value, color = phase)) + geom_boxplot() +
        geom_point(position = position_jitterdodge(jitter.width = 0.3), show.legend = F) +
        ylab("Phenological metrics of EVI - GPP") +
        # scale_x_discrete(breaks = xlab$IGBP, labels = xlab$label) +
        geom_hline(yintercept = 0, color = "blue", linetype = 2, size = lwd) +
        geom_hline(yintercept = c(-15, 15), color = "red", linetype = 2, size = lwd) +
        theme_light(base_size = fontsize, base_family = "Arial") +
        theme(legend.position = c(1-0.01, 0.01), legend.justification = c(1, 0),
              panel.grid.major = element_line(linetype = 2),
              panel.grid.minor = element_blank(),
              legend.title=element_blank(),
              axis.text = element_text(color = "black")) +
        scale_color_manual(values = colors)
