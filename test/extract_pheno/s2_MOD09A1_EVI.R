source("test/main_pkgs.R")

# FOR MOD09A1
# // 620-670nm, RED, sur_refl_b01
# // 841-876nm, NIR, sur_refl_b02
# // 459-479nm, BLUE, sur_refl_b03
# // 1628-1652nm, SWIR, sur_refl_b06
satellite = "MOD09A1"
df = tidy_modis_09A1(satellite)

## 2. phenofit -----------------------------------------------------------------

sites = st_166$site %>% set_names(., .)
grps_sites = sites %>% set_names(seq_along(.), .)
grps_sites = df_bad[, set_names(ID, site)]

lambda0    = 15
nptperyear = 46

InitCluster(12)
lst_EVI = foreach(i = grps_sites, icount()) %dopar% {
    runningId(i)
    sitename = sites[i]
    # brks = lst_brks[[sitename]]$brks

    grps = 5 %>% set_names(., .)
    l_site = foreach(j = grps, icount()) %do% {
        title = sprintf("[%02d] %s_%d", i, sitename, j)
        # wmin  = 0.5
        d = df[site == sitename & group == j, .(t, y = EVI, QC, QC_flag, w)]
        south = st_212[site == sitename, lat] < 0
        # wmid = ifelse(sitename %in% c("DE-Obe", "US-Me2"), 0.1, 0.5)
        # d[, c("QC_flag", "w") := qc_summary(QC, wmin = 0.1, wmid = wmid)]
        if (sitename == "US-Me2") {
            d[QC_flag == "marginal", w := 0.2]
        }
        # if (all(is.na(d$y))) return()
        minExtendMonth = 0.5
        maxExtendMonth = 1
        if (sitename %in% c("US-Prr", "FI-Sod", "CA-NS2")) maxExtendMonth = 5 # long winter
        # only optim lambda when lambda_opt < 2
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
        # lambda = NULL
        tryCatch({
            l = phenofit_site(d$y, d$t, d$w, d$QC_flag, nptperyear = nptperyear,
                              brks = NULL,
                              wFUN = wTSM,
                              .check_season = TRUE,
                              rm.closed = TRUE,
                              lambda = lambda,
                              maxExtendMonth = maxExtendMonth, minExtendMonth = minExtendMonth,
                              south = south,
                              verbose = FALSE,
                              ymin = 0.1, wmin = 0.1, wsnow = 0.8,
                              write.fig = TRUE,
                              use.y0 = TRUE, ylab = "NDVI",
                              titlestr = title, show = FALSE)
            l
        }, error = function(e){
            message(sprintf("[02%d]: %s", i, e$message))
        })
    }
}

# 133: NO-Blv, SNO
# 168: US-Me1
{
    file_gof = "INPUT/gof_MOD09A1_EVI_st166_phenofit.csv"
    df_gof = map(lst_EVI %>% rm_empty, function(l){
        if (!is.null(l$`1`)) {
            l$`1`$fit[, GOF(y, ziter2, include.r = TRUE)]
        } else NULL
    }) %>% do.call(rbind, .) %>%
        {cbind(site = rownames(.), ID = match(rownames(.), sites), data.table(.))}
    fwrite(df_gof, file_gof)
    df_gof = fread(file_gof)
    df_bad <- df_gof[NSE < 0.5, ]
}

{
    version = 0.5
    outfile = glue("phenofit_MOD09A1_EVI_{version}.pdf")
    merge_pdf(outfile, pattern = "\\[", del = FALSE)
    pdf_acrobat(outfile)
}
save(lst_EVI, file = "pheno_MOD09A1_EVI_st166.rda")

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
