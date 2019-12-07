
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
