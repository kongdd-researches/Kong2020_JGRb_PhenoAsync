anno_text <- function(label, x = 0.1, y = 0.9) {
    panel <- function(data, coords) {
        textGrob(label, x, y,
            just = c(0, 1),
            gp = gpar(fontfamily = "Times")
        )
    }
    grid_panel(panel)
}

