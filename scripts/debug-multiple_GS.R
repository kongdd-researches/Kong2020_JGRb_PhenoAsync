# after source: scripts/Figure8_JGRB_multiple_GSs_GPP.R
to_date <- function(d, t) d %>% dplyr::mutate(across(1:3, .fns = ~ t[.x]))

brks <- lst_brks.multi$`IT-Noe`$brks
l <- brks$fit.raw$`2007`
dt <- brks$dt
dt <- find_season.default(l$ziter1, l$t)
ggplot(l, aes(t, ziter1)) +
    geom_line() +
    geom_point(data = dt, aes(beg, y_beg)) +
    geom_point(data = dt, aes(end, y_end))

# {
#     load_all("I:/Research/phenology/phenofit.R/")
#     r <- find_season.default(l$ziter1, l$t, nptperyear = 365, opt)
#     r$dt0 %>%
#         mutate(across(pos:right, .fns = ~ l$t[.x])) %>%
#         arrange(pos)
# }

d <- df_part[site == "IT-Noe", .(date, GPP_DT, x = seq_along(date))]
t <- d$date
y <- d$GPP_DT

{
    dat <- lst_brks.multi$`IT-Noe`$brks$fit
    t <- dat$t
    z <- dat$ziter1
    info <- findpeaks_season_jl(z,
        r_max = 0.1, r_min = 0.05,
        minpeakdistance = nptperyear / 6
    )
    dt <- info$max %>% subset(val > -9000) %>%
        to_date(t) %>%
        data.table()

    # julia_source("N:/Research/GEE_repos/GEE-latest/phenofit.jl/src/phenofit.jl")
    # julia_call("phenofit.check_season!", dt)
    # # julia_call("phenofit.filter_seasons", dt)
    # d_season =
    #     rbind(cbind(info$max, type = 1),
    #           cbind(info$min, type = -1)) %>% data.table() %>% .[order(idx), ]
    # d_season2 = d_season %>% to_date(t)
    
    plot(t, z, type = "l")
    dt2 <- dt[status == "", ]
    ggplot() +
        geom_line(data = data.table(t, z), aes(t, z), color = "black") +
        geom_point(data = dt2, aes(start, val_start), color = "blue") +
        geom_point(data = dt2, aes(stop, val_stop), color = "blue") +
        geom_point(data = dt2, aes(idx, val), color = "red")
    
    # cripple_01 = data.table(t, z)
    # attr(cripple_01, "description") = "left cripple, FLUXNET2015 IT-Noe"
    # use_data(cripple_01, overwrite = TRUE)
    write_fig(last_plot(), "season_left_cripple.pdf", 6, 2)
}

# julia_init()
# if calendarYear then single
# lst_brks.single <- main_divide_season(df_part, info,
#     sites_multi = sites2, sites_single,
#     calendarYear = TRUE, varname = varname,
#     outfile = glue("Figure/phenofit_seasons_single_{version}.pdf")
# )

## 3. curve fitting and get phenology metrics
# lst_brks <- c(lst_brks.single, lst_brks.multi)
# TRSs <- c(0.1, 0.2, 0.5, 0.6, 0.8, 0.9)
#
# InitCluster(10, kill = FALSE)
# lst_pheno <- main_phenofit(lst_brks,
#     TRS = TRSs,
#     show = FALSE, outfile = glue("Figure/gpp_phenofit_pheno_{version}.pdf")
# )
# # lst_pheno2 = main_phenofit(lst_brks[86:length(lst_brks)], TRS = TRSs,
# #     show = TRUE, outfile = glue("Figure/gpp_phenofit_pheno_{version}2.pdf"))
#
# outfile <- glue("INPUT/pheno_gpp_st109 {version}.rda")
# save(lst_brks.single, lst_brks.multi, lst_brks, lst_pheno,
#     sites_multi, sites_single,
#     file = outfile
# )

# {
#     load_all("I:/Research/phenology/phenofit.R/")
#     # l_season = div_season(ypred, nptperyear = 365)
#     # d_seaosn = l_season$dt
#     l <- lst_brks.multi$`CZ-BK2`
#     r = season_mov(l$INPUT, .lambda_vcurve = TRUE)
# }

## visualization
# write_fig(g, "Figure1_phenofit_curve_fitting.pdf", 10, 6)
# ratio = 1.15
# file <- "Figure5_Phenology_Extraction_temp.pdf"
# cairo_pdf(file, 8*ratio, 6*ratio)
# temp <- get_pheno(fit$fits$ELMORE[2:6], IsPlot = T)
# dev.off()
# file.show(file)

# {
#     t = r$fit$t
#     ypred = r$fit %>% select(starts_with("ziter")) %>% last()
#     d_season = r$dt %>% rename_season()
#     pch = 16
#     plot(t, ypred, type = "l")
#     with(d_season, points(time_start, val_start, col = "blue", pch = pch))
#     with(d_season, points(time_end, val_end, col = "blue", pch = pch))
#     with(d_season, points(time_peak, val_peak, col = "red", pch = pch))
# }
