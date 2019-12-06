source("test/main_pkgs.R")

file_gof <- "gof_EVI_st95_phenofit.csv"
file     =  "INPUT/fluxnet/st212_MOD13A1_0m_buffer.csv"
df = fread(file, drop = 1:2) %>%
    .[date <= "2015-12-31", .(site, group, t = ymd(date),
                              NDVI = NDVI/1e4, EVI = EVI/1e4,
                              QC = SummaryQA)] # , FparExtra_QC
df[, c("QC_flag", "w") := qc_summary(QC)]


load(file_brks)
sites      = names(lst_brks)[1:95] # sites_rm not included
grps_sites = seq_along(sites) %>% set_names(sites)
# sites = c(sites_single, sites_multi)
# grps_sites = 66
# grps_sites = c(10, 29, 30, 34, 43, 54)

# df_gof = fread(file_gof)
# grps_sites <- df_gof[NSE  < 0.7, set_names(ID, site)]
# d_match    <- match2(c("IT-La2", "SD-Dem", "CZ-BK2", "DE-Gri", "CA-Qfo", "CH-Dav", "DE-Obe"), names(grps_sites))
# grps_sites <- grps_sites[-d_match$I_y]
# # grps_sites <- df_gof[NSE >= 0.7, set_names(ID, site)]

# list_files("Figure/20190403/", "*.pdf")
InitCluster(12)
lst_EVI = foreach(i = grps_sites, icount()) %dopar% {
    runningId(i)
    sitename = sites[i]
    brks = lst_brks[[sitename]]$brks

    grps = 1:9 %>% set_names(., .)
    l_site = foreach(j = grps, icount()) %do% {
        title = sprintf("[%02d] %s_%d", i, sitename, j)
        # wmin  = 0.5
        d = df[site == sitename & group == j, .(t, y = EVI, QC, QC_flag, w)]
        # wmid = ifelse(sitename %in% c("DE-Obe", "US-Me2"), 0.1, 0.5)
        # d[, c("QC_flag", "w") := qc_summary(QC, wmin = 0.1, wmid = wmid)]
        if (sitename == "US-Me2") {
            d[QC_flag == "marginal", w := 0.2]
        }
        # if (all(is.na(d$y))) return()
        minExtendMonth = 0.5
        maxExtendMonth = 2
        if (sitename %in% c("US-Prr", "FI-Sod", "CA-NS2")) maxExtendMonth = 5 # long winter
        # l_lambda = v_curve(d, lg_lambdas = seq(-1, 2, 0.1), IsPlot = FALSE)
        # lambda <- l_lambda$lambda
        lambda = 5
        if (sitename %in% c("DE-Kli")) {
            lambda = 1
            maxExtendMonth = 0.5
            minExtendMonth = 0
        }

        tryCatch({
            l = phenofit_site(d$y, d$t, d$w, d$QC_flag, nptperyear = 23,
                            brks = NULL,
                            wFUN = wTSM,
                          .check_season = TRUE,
                          lambda = lambda,
                          maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                          verbose = FALSE,
                          ymin = 0.1, wmin = 0.1, wsnow = 0.8,
                          write.fig = TRUE,
                          titlestr = title, show = TRUE)
            l

        }, error = function(e){
            message(sprintf("[02%d]: %s", i, e$message))
        })
    }
}

{
    df_gof = map(lst_EVI %>% rm_empty, function(l){
        if (!is.null(l$`1`)) {
            l$`1`$fit[, GOF(y, ziter2, include.r = TRUE)]
        } else NULL
    }) %>% do.call(rbind, .) %>%
        {cbind(site = rownames(.), ID = match(rownames(.), sites), data.table(.))}
    fwrite(df_gof, file_gof)
    # df_gof = fread(file_gof)
    df_bad <- df_gof[NSE < 0.7, ]
}

{
    version = 0.3
    outfile = glue("phenofit_EVI_{version}.pdf")
    merge_pdf(outfile, pattern = "\\[", del = TRUE)
    # pdf_acrobat(outfile)
}
save(lst_EVI, file = "pheno_EVI_st95.rda")

# merge_pdf("EVI_phenofit_test.pdf", pattern = , del = TRUE)
# merge_pdf("EVI_phenofit_v10_st95.pdf", pattern = "\\[", del = TRUE)
# merge_pdf("LAI_phenofit_gpp_brks_v01_st95.pdf", pattern = "\\[", del = TRUE)
# dt = l$dt %>% do.call(rbind, .)
# df_gpp$date %<>% ymd()
# d_gpp = df_gpp[site == sitename, .(site, t = date, GPP_DT, GPP_NT)]
# Sys.setenv(PATH = paste0("/opt/bin:/opt/anaconda3/bin:", Sys.getenv("PATH")))

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
