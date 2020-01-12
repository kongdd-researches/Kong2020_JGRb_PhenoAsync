
#' global variables:
#' smooth_formula, span
#' @import grid
#' @importFrom ggpmisc stat_poly_eq
#'
#' @examples
#' ggplot_1var
ggplot_1var <- function(x, varname = "APAR",
    color = "red", alpha = 0.3, span = 0.7,
    ylab = NULL, labels = waiver())
{
    p <- ggplot(x, aes_string("dn", varname)) + #, color = "year"
        # geom_point(alpha = 0.2, color = color) +
        geom_smooth(method = "loess",
          # formula = smooth_formula,
                    span = span,
                    alpha = alpha, fill = color,
                    color = color) + #
        scale_y_continuous(position = "right", labels = labels) +
        labs(x = NULL) +
        theme(panel.background = element_rect(fill = "transparent"),
            plot.margin = margin(0, r = -1, 0, 0),
              axis.ticks.y.right = element_line(color = color),
              axis.text.y.right = element_text(color = color),
              axis.title.y.right = element_text(color = color),
              panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank()) # get rid of minor grid
    if (!is.null((ylab))) p <- p + labs(y = ylab)
    p
    # , margin = margin(l = 2, r = 4)
}

#' ggplot_multiAxis
#' @param p1,p2 should have axis.tick.y.right and axis.title.y.left
#'
#' @importFrom grid addGrob
#' @export
ggplot_multiAxis <- function(p1, p2, show = TRUE){
    # intersperse a copy of the bottom axes
    g1 <- p1
    g2 <- p2

    if (!("gtable" %in% class(p1)))  g1 <- ggplotGrob(p1)
    if (!("gtable" %in% class(p2)))  g2 <- ggplotGrob(p2)

    g1_grob_names <- map_chr(g1$grob, "name")
    g2_grob_names <- map_chr(g2$grob, "name")

    I_panel1 <- g1$grobs %>%  {grep("panel", g1_grob_names)}
    panel2   <- g2$grobs %>%  {.[[grep("panel", g2_grob_names)]]}
    g1$grobs[[I_panel1]] %<>% addGrob(panel2)

    ## 2. find ylab-r position
    I_yr1  <- g1$layout %$% {r[grep("ylab-r", name)]} %>% unique()
    I_yr2  <- g2$layout %$% {r[grep("axis-r|ylab-r", name)]} %>% unique() # get `axis-r` and `ylab-r`

    g = g2[, I_yr2]
    # grid.newpage()
    col = g$grobs[[4]]$children[[1]]$gp$col
    x   = -0.05
    tck = 2
    lwd = 5
    gp = gpar(col = col, lwd = lwd)
    axis_y        = segmentsGrob(x, 0, x    , 1, gp = gp)
    axis_y_top    = segmentsGrob(x, 1, x+tck, 1, gp = gp)
    axis_y_bottom = segmentsGrob(x, 0, x+tck, 0, gp = gp)
    axis_y = gtable_add_grob(g, list(axis_y, axis_y_top, axis_y_bottom), t = 7, l = 1) #%>% grid.draw()
    # browser()

    all <- gtable:::cbind.gtable(
        g1[, seq(max(I_yr1))],
        # g2[, I_yr2],
        axis_y,
        # rect,
        # g1[, seq(max(I_yr1)+1, ncol(g1))],
        size = "first")

    if (show){
        grid.newpage()
        grid.draw(all)
    }
    all
    # layout   <- g$layout %>% mutate(vp = sprintf("%s.%d-%d-%d-%d", name, t, r, b, l)) %>% data.table()
    # vp_panel <- layout[name == "panel", vp]
    # ## 2. overlap the new panel
    # downViewport(vp_panel)
    # grid.draw(panel)
}

label_left <- function(len = 4, digit = 1) {
    fmt1 = sprintf("%%%ds", len)
    fmt2 = sprintf("%%.%df", digit)
    function(x) {
        # browser()
        sprintf(fmt1, sprintf(fmt2, x))
    }
}

label_right <- function(len = 4, digit = 1) {
    fmt1 = sprintf("%%-%ds", len)
    fmt2 = sprintf("%%.%df", digit)
    function(x) { sprintf(fmt1, sprintf(fmt2, x)) }
}

add_label <- function(p, label) {
    p + geom_text(data = data.table(x = -Inf, y = Inf), aes(x, y),
        label = label, hjust = -0.1, vjust = 2, size = 5)
    # p + geom_text(
    #     data = data.table(x = 13, y = -Inf), aes(x, y),
    #     label = label, hjust = 0, vjust = -1.3, size = 5
    # )
}
plot_LUE_multiAxis <- function(x, label, span = 0.75) {
    # , color = year
    # span = 0.7
    p <- ggplot(x, aes(dn, GPP)) +
        # geom_point() +
        geom_smooth(method = "loess",
            span = span, 
            # formula = smooth_formula, span = span,
            color = "black") +
        theme(panel.background = element_rect(size = 1)) +
        scale_y_continuous(labels = label_left()) +
    labs(y = expression(GPP[obs] * " (gC " * m^-2 * d^-1 * ")"), x = NULL)

    x[, VPD_sqrt := sqrt(VPD) ]
    # p_GPPsim  <- ggplot_1var(x, "GPP_sim" , "grey60",
    #     ylab = expression(bold(GPP[sim])),
    #     labels = label_right())
    p_GPPvpm  <- ggplot_1var(x, "GPP_vpm" , "blue", ylab = expression(bold(GPP[vpm])),
        labels = label_right(), span = span) + 
        labs(y = expression(GPP[vpm] * " (gC " * m^-2 * d^-1 * ")"))
    p_GPPpml  <- ggplot_1var(x, "GPP_pml" , "red", ylab = expression(bold(GPP[pml])),
        labels = label_right(), span = span)
        

# browser()
    # GPP_max = x[, pmax(GPP, GPP_vpm, GPP_pml, na.rm = TRUE)] %>% max()
    # GPP_min = x[, pmin(GPP, GPP_vpm, GPP_pml, na.rm = TRUE)] %>% min()
    # d_blank = data.table(dn = 1:2, GPP = c(GPP_max, GPP_max))
    # p        <- p + geom_blank(data = d_blank, aes(dn, GPP))
    # p_GPPpml <- p_GPPpml + geom_blank(data = d_blank, aes(dn, GPP))
    # p_GPPvpm <- p_GPPvpm + geom_blank(data = d_blank, aes(dn, GPP))

    # 增加调整axis ylim 的选项
    # x[, pmax()]
    alpha = 0.2
    p_EVI  <- ggplot_1var(x, "EVI", "darkgreen", labels = label_right(digit = 2), alpha = alpha, span = span)
    p_NDVI <- ggplot_1var(x, "NDVI", "green", labels = label_right(digit = 1), alpha = alpha, span = span)
    p_LAI  <- ggplot_1var(x, "LAI" , "darkviolet", labels = label_right(digit = 1), alpha = alpha, span = span)
    p_VI2  <- ggplot_1var(x, "VI_dhour", "red", labels = label_right(digit = 2), alpha = alpha, span = span) + 
        labs(y = "EVI*PC")

    p_APAR <- ggplot_1var(x, "APAR", "red", span = span)
    p_Rs   <- ggplot_1var(x, "PAR"  , "purple", span = span) + 
        labs(y = expression("PAR (W " * m^-2 * d^-1 * ")"))
    p_dhour <- ggplot_1var(x, "dhour2"  , "blue", span = span) + 
        labs(y = "dayl (hour)")
    p_TS   <- ggplot_1var(x, "TS"  , "darkgoldenrod2", span = span) + # soil
        labs(y = "TS (°C)")
    p_Tair_day <- ggplot_1var(x, "Tair_day"  , "yellow4", span = span) + # soil
        labs(y = expression(Tair[day] * " (°C)"))
    p_VPD  <- ggplot_1var(x, "VPD_sqrt" , "darkorange1", span = span) + 
        labs(y = expression(sqrt(VPD) * " (" * kPa ^ 0.5 * ")"))
    p_prcp <- ggplot_1var(x, "Prcp", "skyblue", labels = label_right(len = 3), span = span) + 
        labs(y = expression(Prcp * " ("* mm^-1 * ")"))
    p_epsilon_eco <- ggplot_1var(x, "epsilon_eco", "darkorange1", span = span)
    p_epsilon_chl <- ggplot_1var(x, "epsilon_chl", "yellow", span = span)

    p_Wscalar <- ggplot_1var(x, "Wscalar", "blue", span = span)
    p_Tscalar <- ggplot_1var(x, "Tscalar", "yellow4", span = span)
    # p_epsilon_eco, p_epsilon_chl
    lst_1 = list(p, p_GPPvpm, p_VI2) # , p_GPPvpm, , p_EVI, p_NDVI, p_LAI, , p_GPPpml
    # browser()
    lst_1[[length(lst_1)]] %<>% add_label(label)
    p1 <- reduce(lst_1, ggplot_multiAxis, show = F)

    p <- p + labs(y = NULL)
    p2_0 <- reduce(list(p, p_EVI, p_LAI), ggplot_multiAxis, show = F) # p_NDVI, , p_VI2
    p2_1 <- reduce(list(p, p_Rs, p_dhour), ggplot_multiAxis, show = F) # , p_APAR
    p2 <- reduce(list(p, p_TS, p_Tair_day), ggplot_multiAxis, show = F) # p_Tscalar
    p3 <- reduce(list(p, p_prcp, p_VPD), ggplot_multiAxis, show = F) # p_Wscalar

    p = arrangeGrob(grobs = list(p1, p2_0, p2_1, p2, p3), nrow = 1, widths = c(9, 9, 9, 9, 9))
    # listk(p1, p2)
    p
}
