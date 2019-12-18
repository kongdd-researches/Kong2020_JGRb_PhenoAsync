## code to prepare `DATASET` dataset goes here

d_ATNew = fread("data-raw/fluxnet_AT-New_8day.csv")
usethis::use_data(d_ATNew, overwrite = TRUE)

p  <- ggplot(d_ATNew, aes(dn, GPP)) + geom_smooth()
p1 <- ggplot_1var(d_ATNew, "GPP")
p2 <- ggplot_1var(d_ATNew, "EVI", color = "blue")

# ggplot_multiAxis(p, p1)

p_final <- reduce(list(p, p1, p2), ggplot_multiAxis, show = T)

write_fig(expression({
    check_sensitivity_async(d_ATNew, predictors)
}), "async.pdf", 8, 10)
