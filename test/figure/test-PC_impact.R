# after "test/figure/Figure5_behind_reason.R"

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
