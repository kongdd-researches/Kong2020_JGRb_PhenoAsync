# source('F:/Github/PML_v2/fluxsites_tidy/R/pkg/pkg_vis.R')
windowsFonts(Times = windowsFont("Times New Roman"),
             Arial = windowsFont("Arial"))
fontsize <- 15

# theme_grey, theme_gray, theme_light
mytheme  <- theme_grey(base_size = fontsize, base_family = "Arial") +
    theme(
        # legend.position = c(0.02, 0.02), legend.justification = c(0, 0), 
        # legend.position="bottom", 
        text = element_text(colour = "black", size = fontsize), 
        # axis.title.x = element_blank(),
        # plot.title = element_text(hjust = 0),
        axis.title = element_text(face = "bold", size = fontsize+1), 
        axis.text  = element_text(colour = "black", size = fontsize), 
        strip.text = element_text(colour = "black", size = fontsize, face = "bold"),
        # axis.text.x = element_text(angle = 0, size = fontsize), 
        # legend.margin = unit(0.2, "cm"), 
        legend.text  = element_text(size = fontsize, face = "bold"), 
        legend.title = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        # panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(size = 0.2),
        plot.margin = unit(c(1,3,1,1)*0.2, "cm"))
theme_set(mytheme)
