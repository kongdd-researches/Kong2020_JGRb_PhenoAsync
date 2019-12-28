df_obs = merge(df_final, st[, .(site, LC)], by = "site")
df_obs[, `:=`(
  Wscalar = clamp(Wscalar, c(0, 1)),
  Tscalar = LUE_Tscalar(TS, IGBP)
)]
df_obs[, `:=`(
  PAR  = Rs*0.45,
  APAR = Rs*0.45*1.25*(EVI.whit), 
  epsilon_eco = GPP / (Rs*0.45), 
  epsilon_chl = GPP / (Rs*0.45*1.25*(EVI - 0.1))
)]
d = df_obs[site == "AT-Neu"]

vars_comm <- c("site", "date", "IGBP", "year", "year2", "ydn", "lat", "dn", "t", "LC", "QC_flag", "group")
vars_aggr <- colnames(df_obs) %>% setdiff(vars_comm)

df_d8 = df_obs[, lapply(.SD, mean, na.rm = TRUE), .(site, dn, LC), .SDcols = vars_aggr]
# d <- df_d8[LC == "Grassland"]

{
  # load_all()
  lcs = st$LC %>% levels()
  gs = foreach(lc = lcs, i = icount()) %do% {
    d = df_d8[LC == lc]
    label = sprintf("(%s) %s", letters[i], lc)
    g = plot_LUE_multiAxis(d, label = label)
    # write_fig(g, "temp.pdf", 15, 3)
    g
  }
  fontsize <- 16
  left   <- textGrob(expression(GPP[obs]), gp=gpar(fontsize=fontsize, fontface = "bold"), rot = 90)
  bottom <- textGrob(expression(bold("Time (the i-th 8-day)")), gp=gpar(fontsize=fontsize, fontface = "bold", fontfamily = "Times"))
  p = arrangeGrob(grobs = gs, left=NULL, bottom=bottom, ncol=1)
  write_fig(p, "p1.pdf", 18, 10)
}

# Wscalar not used, because it is overestimated in winter, which will emplify 
# amplitude of `Wscalar` and will lead to a smaller `Wscalar` in the growing 
# season.

write_fig(g, "temp.pdf", 15, 4)
ggplot(df_d8, aes(dn, GPP)) + facet_wrap(~LC) + 
  # geom_point() + 
  stat_summary(fun.data = stat_sd, geom = "ribbon", alpha = 0.5) +
  stat_summary(fun.data = stat_sd, geom = "line", alpha = 0.5)
  # geom_smooth()

write_fig(expression({
  check_sensitivity(d, predictors)
}), "async.pdf", 8, 10)


ggplot_1var(d)
