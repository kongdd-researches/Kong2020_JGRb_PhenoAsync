source("test/main_pkgs.R")
load_all("../phenofit")

df_north = readRDS(file_GPP_north)

info = df_north[, .N, .(site)] %>% merge(st_166, by = "site") %>% {
    . = .[order(IGBP), ]
    .[, ID := 1:.N]
}
sites = info$site %>% unique()
sites_no_continue <- c("BE-Bra", "BE-Vie", "CA-Man", "CH-Cha", "CH-Dav",
    "CH-Fru", "CN-Din", "DE-Akm", "DE-Kli", "FR-Gri", "FR-LBr", "IT-BCi", "IT-CA1",
    "IT-Col", "IT-Cpz", "IT-Noe", "IT-Ren", "IT-Ro1", "IT-Ro2", "IT-SRo", "JP-SMF",
    "NL-Hor", "NL-Loo", "RU-Fyo", "US-ARM", "US-Ha1", "US-Los", "US-MMS", "US-Me2",
    "US-Me6", "US-PFa", "US-Syv", "US-WCr") %>% set_names(., .)

date_start = make_date(1999,  1,  1)
date_end   = make_date(2018, 12, 31)

{
    file = "phenofit_fluxnet116_season_divides.pdf"
    Cairo::CairoPDF(file, 10, 8)
    par(mfrow = c(5, 1))

    # DT: daytime partition
    lst = foreach(sitename = sites, i = icount()) %do% {
        runningId(i)
        sp = info[site == sitename, ]
        d = df_north[site == sitename & date >= date_start & date <= date_end,
                     .(site, t = date, y = GPP_DT, w = 1 - is.na(GPP_DT))] # GPP_NT,
        tryCatch({
            l = divide_seasons(d, sp, 365, lambda = 100)
            titlestr = with(sp[1, ], sprintf("%dth %s, %s, [%.2f, %.2f]", ID, site, IGBP, lon, lat))
            with(l, plot_season(INPUT, brks, title = titlestr, show.legend = FALSE))
            l
            # write_fig(expression({with(l, plot_season(INPUT, brks, title = sitename))}), "a.pdf", 10, 4)
        }, error = function(e){
            message(sprintf("[e] %d %s: %s", i, sitename, e$message))
        })
    }
    dev.off()
    SumatraPDF(file)
}

# 采用单growing season的IGBP: MF, WET, SAV, SNO()

# IGBP (n): multiple growing season sites
   WSA (1,  2): US-SRM
   OSH (1,  4): US-Whs
   GRA (3, 23): US-SRG, US-Wkg, CZ-BK2, CN-SW2 (集中度)
   ENF ( , 29): FR-LBr (检查)
   EBF (, 5)     : IT-Cp2, 几乎所有的站点都为考虑

   CRO: 1:5, 7, 8-11,
   CSH: 17, IT-Noe
   DBF: 21, IT-CA1, IT-Ro1, IT-Ro2,

# DE-Akm IT-CA1 US-Me6
# 8     13     30

# Warning messages:
# 1: In max(y_good) : max里所有的参数都不存在；回覆-Inf
# 2: In max(y_good) : max里所有的参数都不存在；回覆-Inf
# 3: In (function (INPUT, rFUN = wWHIT, wFUN = wTSM, iters = 2, wmin = 0.1,  :
#                      Can't find a complete growing season before trim!
# 4: In (function (INPUT, rFUN = wWHIT, wFUN = wTSM, iters = 2, wmin = 0.1,  :
#   Can't find a complete growing season before trim!
# 5: In max(y_good) : max里所有的参数都不存在；回覆-Inf

## check about head and tail in d


## divide into two situations: one growing season and multiple growing season
{
    y = d$GPP_NT
    r = wSG(y, frame = 91, nptperyear = 365, iters = 3)
    write_fig(expression({
        plot(y, type = "l")
        movmean(d$GPP_NT, 20) %>% lines(col = "red", lwd = 1)
        lines(r$zs %>% last(), col = "blue", lwd = 1)
        grid()
    }), "movmean.pdf", 10, 4)
}
# `GPP_DT`: (daytime partition) much better, no negative value
# `GPP_NT`: (nighttime partition), has negative value
