
devtools::load_all("../phenofit.R")
devtools::load_all("../rTIMESAT.R")

## multiple seasons
source("test/main_pkgs.R")
source("test/data-prepare/s1_dat0_divide_growing_season.R")
library(ggnewscale)
library(patchwork)
library(metR)

df_part$date %<>% as_date()
calendarYear = TRUE

df = readRDS("../rfluxnet.R/OUTPUT/fluxsites166_official_SUBSET_DD_raw.RDS")

dat = df[site == "IT-CA1"]
sitename = "IT-CA1"
d_DT = df[site == sitename, .(date, GPP_DT, GPP_DT_QC, RECO_DT, RECO_DT_QC)]
d_NT = df[site == sitename, .(date, GPP_NT, GPP_NT_QC, RECO_NT, RECO_NT_QC)]
brks_qc = c(-Inf, 0.3, 0.5, 0.7, Inf)
d = list(DT = d_DT, NT = d_NT) %>%
    map(~set_names(.x, c("date", "GPP", "GPP_QC", "RECO", "RECO_QC"))) %>%
    melt_list("type") %>%
    mutate(qc_lev = cut(GPP_QC, brks_qc)) %>%
    .[type == "NT"]
dat_gpp = d[date >= "2012-01-01", .(t = date, y = GPP, w = GPP_QC)]
ggplot(dat_gpp[w >= 0, ], aes(t, y)) + geom_line()


half = 350; stl(y2, half*2 + 1) %>% plot()
y2 = ts(dat_gpp$y, start = c(2012, 1), frequency = 365)
y = zoo(dat_gpp$y, dat_gpp$t)

{
    # load_all("../phenofit.R")
    # with(dat_gpp, lambda_vcurve_jl(y, w, lg_lambda_max = 5))
    # with(dat_gpp, lambda_cv_jl(y, w, lg_lambda_max = 5))
    wBisquare_julia <- function(y, yfit, w, ..., wmin = 0.2,
                                trs_high = 0.3,
                                trs_low  = trs_high,
                                trs_bg = 0.2,
                                .toUpper = TRUE)
    {
        julia_init()
        if (missing(w)) w  <- rep(1, length(y))
        wnew = JuliaCall::julia_call("wBisquare", y, yfit, w,
                                     wmin = wmin,
                                     trs_high = trs_high,
                                     trs_low  = trs_low,
                                     trs_bg = trs_bg)
        return(wnew)
    }

    r_kong = with(dat_gpp, process_phenofit(y, t, w = rep(1, length(y)),
                                            nptperyear = 365,
        rFUN = "smooth_wSG", frame = 5,
        wFUN = wBisquare_julia,
        lambda = 5000,
        .v_curve = FALSE,
        methods = c("AG", "Zhang", "Beck", "Elmore", "Gu"), #, "AG2"
        iters = 3,
        maxExtendMonth = 2,
        minExtendMonth = 5/30,        outfile = "a.pdf", overwrite = TRUE,
        run.curvefit = F))
}

d = data.table::fread("I:/Research/phenology/PhenoAsync/a.csv")
y = d$y
w = d$w
w[w < 1e-4] = 1e-4
t = system.time(for (i in 1:1e3){
    r = rcpp_wSG(y, 30L, 2L, w)
})
