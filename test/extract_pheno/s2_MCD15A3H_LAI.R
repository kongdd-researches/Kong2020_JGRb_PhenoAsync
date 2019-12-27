source("test/main_pkgs.R")

file = "INPUT/fluxnet/st212_MCD15A3H_0m_buffer.csv"
df = fread(file, drop = 1:2) %>%
    .[date <= "2015-12-31", .(site, group, t = date, y = Lai/10, QC = FparExtra_QC, FparLai_QC)] # , FparExtra_QC
df[, c("QC_flag", "w") := qc_FparLai(QC)]
df[, w2 := qc_5l(FparLai_QC)]
df$t %<>% ymd()
# d = df[site == "AR-SLu" & group == 5,]

load(file_brks)
# all 166 sites
sites = st_166$site %>% set_names(., .)
grps_sites = sites %>% set_names(seq_along(.), .)

# sites      = names(lst_brks)[1:95] # sites_rm not included
# grps_sites = seq_along(sites) %>% set_names(sites)
# sites = c(sites_single, sites_multi)
# grps_sites = 66
# grps_sites = c(10, 29, 30, 34, 43, 54)

InitCluster(12)
lst_LAI = foreach(i = grps_sites, icount()) %dopar% {
    runningId(i)
    sitename = sites[i]
    brks = lst_brks[[sitename]]$brks

    grps = 1:9 %>% set_names(., .)
    l_site = foreach(j = grps) %do% {
        title = sprintf("[%02d] %s_%d", i, sitename, j)
        # wmin  = 0.5
        d = df[site == sitename & group == j, ]
        wmid = ifelse(sitename %in% c("DE-Obe", "US-Me2"), 0.1, 0.5)
        d[, c("QC_flag", "w") := qc_FparLai(QC, wmin = 0.1, wmid = wmid)]
        # d[QC_flag == "aerosol", w := wmin]
        if (all(is.na(d$y))) return()

        # Init parameters for phenofit_site ------------------------------------
        south = st_212[site == sitename, lat] < 0

        minExtendMonth = 0.5
        maxExtendMonth = 1
        if (sitename %in% c("US-Prr", "FI-Sod", "CA-NS2")) maxExtendMonth = 5 # long winter
        lambda <- tryCatch({
            l_lambda = v_curve(d[!is.na(y)], lg_lambdas = seq(1, 3, 0.1), IsPlot = FALSE)
            ans = l_lambda$lambda
            if (ans > 500) ans <- ans*2/3
            ans
        }, error = function(e) lambda0)
        # return(lambda)
        if (lambda < 150) {
            message(sprintf("%s: lambda = %f", sitename, lambda))
            maxExtendMonth = 0.5
            minExtendMonth = 0
        } #else lambda = lambda0
        # end parameter --------------------------------------------------------

        tryCatch({
            l = phenofit_site(d$y, d$t, d$w, d$QC_flag, nptperyear = 92,
                          brks = NULL,
                          .check_season = TRUE,
                          rm.closed = TRUE,
                          lambda = lambda,
                          maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                          south = south,
                          ymin = 0.1, wmin = 0.1, wsnow = 0.8,
                          verbose = FALSE,
                          titlestr = title, show = FALSE)
            l
        }, error = function(e){
            message(sprintf("[02%d]: %s", i, e$message))
        })
    }
    # write_fig(expression({
    #     l = phenofit_site(d$y, d$t, d$w, d$QC_flag, nptperyear = 92,
    #                       titlestr = "site", show = TRUE)
    # }), "a.pdf", 10, 4)
}

{
    version = 0.3
    outfile = glue("phenofit_LAI_st95_{version}.pdf")
    merge_pdf(outfile, pattern = "\\[", del = TRUE)
    # pdf_acrobat(outfile)
}
save(lst_LAI, file = "pheno_MCD15A3H_LAI_st166.rda")

# merge_pdf("LAI_phenofit_gpp_brks_v01_st95.pdf", pattern = "\\[", del = TRUE)
# dt = l$dt %>% do.call(rbind, .)
# df_gpp$date %<>% ymd()
# d_gpp = df_gpp[site == sitename, .(site, t = date, GPP_DT, GPP_NT)]

{
    j = 5
    sitename = "CZ-BK2"
    sitename = "CA-NS5"
    sitename = "CH-Dav"
    sitename = "US-KS2"
    d = df[site == sitename & group == j, ]
    # d = d[t >= "2014-01-01" & t <= "2015-01-01"]
    nptperyear = 92
    INPUT <- check_input(d$t, d$y, d$w, QC_flag = d$QC_flag,
                         nptperyear = nptperyear, south = FALSE,
                         maxgap = nptperyear/4, alpha = 0.02, wmin = 0.2)
    # plot_input(INPUT, show.y0 = TRUE)
    lg_lambdas = seq(1, 4, 0.1)
    lambda <- v_curve(INPUT, lg_lambdas)$lambda

    brks <- season_mov(INPUT,
                       FUN = smooth_wWHIT, wFUN = wTSM,
                       maxExtendMonth = 3,
                       lambda = 1e3,
                       r_min = 0.03,
                       .check_season = FALSE,
                       # years.run = 2004,
                       IsPlot = FALSE, IsPlot.OnlyBad = FALSE, print = FALSE)

    write_fig(expression(plot_season(INPUT, brks)), "check_season.pdf", 10, 4)
}

{
    d$y[1:200] %>% plot(type = "b")
    halfwin = ceiling(nptperyear/36)
    movmean(d$y, halfwin) %>% lines(col = "blue", type = "b")
}
