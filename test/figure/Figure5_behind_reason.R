source("test/main_pkgs.R")
source("test/main_vis.R")

# also need merge LAI into MOD09A1
{
  df_final = readRDS("./INPUT/flux8d_assembly_st166.RDS") # nrow = 440725
  # df_final[, `:=`(GPP_sim  = pmax(0, 0.078*1.25*(EVI.whit-0.1)*Rs*Tscalar*Wscalar))]

  # Merge PMLV2 GPP
  d_PML = fread("INPUT/fluxnet/st212_PMLV2_v015_8day_0m_buffer.csv") %>%
    .[, .(site, group, date = ymd(date), GPP_pml = GPP/1E2)]
  df_final <- merge(df_final, d_PML, by = .(site, group, date) %>% names(), all.x = TRUE)

  # PML is overestimated and VPM is underestimated
  df_final[, as.list(GOF(GPP, GPP_vpm))]
  df_final[, as.list(GOF(GPP, GPP_pml))]

  # 2. dhour2, linear比较合适
  # df_final[, dhour3 := solartime::computeDayLength(date, lat)]
  df_final[, dhour_norm := (dhour2/max(dhour2))^1, .(site)]
  df_final[, VI_dhour := dhour_norm * EVI.whit, .(site)] # Bauerlea

  df_final$PAR_TOA = df_final[, .(lat, doy = yday(date))][, MJ_2W(cal_Ra(lat, doy))*0.4 ]
}

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
# add simulated VI
d = df_obs[site == "AT-Neu"]

vars_comm <- c("site", "date", "IGBP", "year", "year2", "nydn", "lat", "dn", "t", "LC", "QC_flag", "group")
vars_aggr <- colnames(df_obs) %>% setdiff(vars_comm)
df_d8 = df_obs[, lapply(.SD, mean, na.rm = TRUE), .(site, dn, LC), .SDcols = vars_aggr] %>%
  plyr::mutate(dn = 8*dn + 1)
# d <- df_d8[LC == "Grassland"]
{
  # devtools::load_all()
  fontsize = 14
  varname = "GPP_NT"
  # varname = "GPP_DT"
  # load_all()

  lcs = st$LC %>% levels()
  gs = foreach(lc = lcs, i = icount()) %do% {
    d = df_d8[LC == lc]
    label = sprintf("(%s) %s", letters[i], lc)
    NO_begin = (i-1)*5 + 1
    g = plot_LUE_multiAxis(d, NO_begin, lc, span = 0.65, varname)
    # write_fig(g, "temp.pdf", 15, 3)
    g
  }
  fontsize <- 16
  left   <- textGrob(expression(GPP[obs]), gp=gpar(fontsize=fontsize, fontface = "bold"), rot = 90)
  # bottom <- textGrob(expression(bold("Time (the" ~ i^{th} ~ "8-day)")), gp=gpar(fontsize=fontsize, fontface = "bold", fontfamily = "Times"))
  bottom <- textGrob(expression(bold("DOY (day of year)")), gp=gpar(fontsize=fontsize, fontface = "bold", fontfamily = "Times"))
  p = arrangeGrob(grobs = gs, left=NULL, bottom=bottom, ncol=1)
  outfile = glue("Figure7_behind_reason_dhour2_linear-{varname}.pdf")
  scale = 0.95
  write_fig(p, outfile, 18*scale, 9*scale, show = TRUE)
  # pdf_SumatraPDF(outfile)
}

# d = df_final[site == "CA-Man" & group == 1, ]
# d_pheno <- pheno_T[site == "CA-Man" & group == 1, ]
# ggplot(d, aes(date, LSWI)) + geom_line() +
#   geom_vline(xintercept = d_pheno$sos_date, color = "blue") +
#   geom_vline(xintercept = d_pheno$eos_date, color = "red")
# ggplot(d, aes(date, Wscalar)) + geom_line() +
#   geom_vline(xintercept = d_pheno$sos_date, color = "blue") +
#   geom_vline(xintercept = d_pheno$eos_date, color = "red")
#   # geom_point(data = d_pheno, aes(y = NULL, xintercept = eos_date), color = "red")


# Wscalar not used, because it is overestimated in winter, which will emplify
# amplitude of `Wscalar` and will lead to a smaller `Wscalar` in the growing
# season.

# write_fig(g, "temp.pdf", 15, 4)
# ggplot(df_d8, aes(dn, GPP)) + facet_wrap(~LC) +
#   # geom_point() +
#   stat_summary(fun.data = stat_sd, geom = "ribbon", alpha = 0.5) +
#   stat_summary(fun.data = stat_sd, geom = "line", alpha = 0.5)
#   # geom_smooth()
#
# write_fig(expression({
#   check_sensitivity(d, predictors)
# }), "async.pdf", 8, 10)
#
# ggplot_1var(d)
