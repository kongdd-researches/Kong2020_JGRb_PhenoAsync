source("test/main_pkgs.R")

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

# 测试光周期对ET和GPP影响
{
  d <- df_final[group == 5,
                .(site, date, ydn, dn,
                  GPP, GPP_vpm, EVI.whit, VPD,
                    LE_CORR, len = dhour2, intensity = Rs*24/dhour,
                    volume = Rs*24, Rs,
                    TA, TS, Tair_day, Prcp,
                    PAR_TOA,
                    dhour = dhour2)] %>%
    unique() %>% merge(st_212[, .(site, IGBP, LC)], .)
  #%>% na.omit()
  x = d[site %in% st$site,
        lapply(.SD, mean, na.rm = TRUE), .(LC, dn = get_dn(date)),
        .SDcols = colnames(d)[-(1:6)]] %>%
    .[order(dn), ]
  d[, dhour_norm := dhour/max(dhour), .(site)]

  lst_vars <- list(
    .(GPP    , len, intensity, volume),
    .(LE_CORR, len, intensity, volume),
    .(GPP    , dhour_norm, intensity, volume),
    .(LE_CORR, dhour_norm, intensity, volume),
    .(GPP    , dhour_norm, intensity),
    .(LE_CORR, dhour_norm, intensity),
    .(GPP    , dhour_norm, PAR_TOA),
    .(LE_CORR, dhour_norm, PAR_TOA)
  ) %>% map(names)

  temp = foreach(vars = lst_vars, i = icount()) %do% {
    d_i = d[, ..vars] %>% na.omit()
    ans = pcor(d_i)$estimate; print(ans)
    ans
  }
}

{
  l = d[, .(LE_CORR, PAR_TOA, dhour_norm)] %>% na.omit()
  m <- plsreg1(l[, .(PAR_TOA, dhour_norm)], l[,.(LE_CORR)])
  m[c("VIP", "std.coefs")] %>% print()

  l = d[, .(GPP, PAR_TOA, dhour_norm)] %>% na.omit()
  m <- plsreg1(l[, .(PAR_TOA, dhour_norm)], l[,.(GPP)])
  m[c("VIP", "std.coefs")] %>% print()
}

# end of photoperiod
# > x[, as.list(sapply(.SD, which.max)), .(LC), .SDcols = c("GPP", "EVI.whit", "volume", "dhour", "TS", "TA", "Prcp")]
# LC GPP EVI.whit volume dhour TS TA Prcp
# 1: Grassland  23       25     23    23 26 26   24
# 2:    Forest  23       25     23    23 27 27   35
# 3:  Cropland  23       25     23    23 26 26   28
# 4:       ENF  23       26     23    23 27 25   28
# 5: Shrubland  23       27     23    23 25 26   22

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
df_d8 = df_obs[, lapply(.SD, mean, na.rm = TRUE), .(site, dn, LC), .SDcols = vars_aggr]
# d <- df_d8[LC == "Grassland"]

library(grid)
library(gridExtra)
library(gtable)
{
  # load_all()
  lcs = st$LC %>% levels()
  gs = foreach(lc = lcs, i = icount()) %do% {
    d = df_d8[LC == lc]
    label = sprintf("(%s) %s", letters[i], lc)
    g = plot_LUE_multiAxis(d, label = label, span = 0.65)
    # write_fig(g, "temp.pdf", 15, 3)
    g
  }
  fontsize <- 16
  left   <- textGrob(expression(GPP[obs]), gp=gpar(fontsize=fontsize, fontface = "bold"), rot = 90)
  bottom <- textGrob(expression(bold("Time (the i-th 8-day)")), gp=gpar(fontsize=fontsize, fontface = "bold", fontfamily = "Times"))
  p = arrangeGrob(grobs = gs, left=NULL, bottom=bottom, ncol=1)
  outfile = "Figure5_behind_reason_dhour2_linear.pdf"
  write_fig(p, outfile, 18, 10, show = TRUE)
  pdf_SumatraPDF(outfile)
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
