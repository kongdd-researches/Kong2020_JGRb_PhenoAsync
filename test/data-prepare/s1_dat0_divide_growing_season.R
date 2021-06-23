source("test/main_pkgs.R")

outfile = "INPUT/df_north_109st.rda"

if (!file.exists(outfile)) {
    df_north = readRDS(file_GPP_north)
    ## 建议删除站点：
    sites_rm <- c("CN-Sw2", "US-Blo",
                  "FR-LBr", "IT-CA1", "IT-CA2", # ENF, 拖尾分布
                  "IT-BCi", # 2012年以后多于三生长季
                  "IT-Noe", # 生长季不明显
                  "CH-Oe1", # cut 4-times
                  "US-ARM", # 三个生长季
                  "US-Me6", # 多种植被类型，明显的拖尾分布
                  "US-SRG", "US-Wkg", # 多种植被类型，明显的拖尾分布
                  "US-Whs", # OSH (1,  4):
                  "US-SRM"
                  ) # GRA, ENF
    info_full = df_north[, .N, .(site)] %>%
        merge(st_166[IGBP != "EBF", ], by = "site") %>%
        {
            . = .[order(IGBP), ]
            .[, ID := 1:.N]
        } # 107 sites left

    info_sub = df_north[, .N, .(site)] %>%
        merge(st_166[IGBP != "EBF" & !(site %in% sites_rm), ], by = "site") %>% {
        . = .[order(IGBP), ]
        .[, ID := 1:.N]
    } # 107 sites left
    sites = info_sub$site %>% unique()
    # multiple growing season sites
    sites_multi <- c(
        "BE-Lon", "CH-Oe2", "DE-Geb", "DE-Kli", "DE-RuS", "FI-Jok", "FR-Gri", "IT-BCi", "IT-CA2", "US-ARM", # CRO
        "IT-Noe", # CSH
        "IT-CA1", "IT-Ro1", "IT-Ro2", # DBF: 21, IT-CA3,
        "FR-LBr", "US-Me6", # ENF, look normal, except 2005
        "CH-Oe1", "CZ-BK2", "US-SRG", "US-Wkg", # GRA (3, 23)
        "US-Whs", # OSH (1,  4):
        "US-SRM" # WSA (1,  2):
        # "IT-Cp2" # EBF (, 5)     : , 几乎所有的站点都为考虑
        ) %>% setdiff(sites_rm)
    # sites with mixed of vegetation types are not inclued .

    sites_single <- setdiff(sites, sites_multi)
    # sites <- c(sites_single, sites_multi, sites_rm)
    # sites_no_continue <- c("BE-Bra", "BE-Vie", "CA-Man", "CH-Cha", "CH-Dav",
    #     "CH-Fru", "CN-Din", "DE-Akm", "DE-Kli", "FR-Gri", "FR-LBr", "IT-BCi", "IT-CA1",
    #     "IT-Col", "IT-Cpz", "IT-Noe", "IT-Ren", "IT-Ro1", "IT-Ro2", "IT-SRo", "JP-SMF",
    #     "NL-Hor", "NL-Loo", "RU-Fyo", "US-ARM", "US-Ha1", "US-Los", "US-MMS", "US-Me2",
    #     "US-Me6", "US-PFa", "US-Syv", "US-WCr") %>% set_names(., .)
    date_start = make_date(1999,  1,  1)
    date_end   = make_date(2018, 12, 31)

    df_part = df_north[date >= date_start & date <= date_end, ] %>% setkeyv(c("site"))

    save(df_part, date_start, date_end, 
      sites_multi, sites_single, sites, sites_rm, 
      info_full, info_sub, file = outfile)
} else {
   load(outfile)
}

# {
#     file = "phenofit_fluxnet86_single_season.pdf"
#     Cairo::CairoPDF(file, 10, 8)
#     par(mfrow = c(5, 1))

#     # DT: daytime partition
#     lst = foreach(sitename = sites_single, i = icount(1)) %do% {
#         runningId(i)
#         sp = info[site == sitename, ]
#         d = df_north[site == sitename & date >= date_start & date <= date_end,
#                      .(site, t = date, y = GPP_DT, w = 1 - is.na(GPP_DT))] # GPP_NT,
#         tryCatch({
#             l = divide_seasons(d, sp, 365, lambda = 100)
#             titlestr = with(sp[1, ], sprintf("%dth %s, %s, [%.2f, %.2f]", ID, site, IGBP, lon, lat))
#             with(l, plot_season(INPUT, brks, title = titlestr, show.legend = FALSE))
#             l
#             # write_fig(expression({with(l, plot_season(INPUT, brks, title = sitename))}), "a.pdf", 10, 4)
#         }, error = function(e){
#             message(sprintf("[e] %d %s: %s", i, sitename, e$message))
#         })
#     }
#     dev.off()
#     SumatraPDF(file)
# }

# ## multiple seasons
# {
#     sites = sites_multi
#     sites = sites_rm
#     info = info_full

#     file = "phenofit_fluxnet10_multi_seasons_v3.pdf"
#     Cairo::CairoPDF(file, 10, 8)
#     par(mfrow = c(5, 1))
#     nptperyear = 365
#     # DT: daytime partition
#     lst = foreach(sitename = sites, i = icount()) %do% {
#         runningId(i)
#         # sitename = "IT-Ro1"
#         # sitename = "DE-Kli"
#         sp = info[site == sitename, ]
#         d = df_north[site == sitename & date >= date_start & date <= date_end,
#                      .(site, t = date, y = GPP_DT, w = 1 - is.na(GPP_DT))] # GPP_NT,
#         tryCatch({
#             l = divide_seasons(d, sp, 365, lambda = 100,
#                                # .movmean = TRUE,
#                                iters = 1,
#                                # wFUN = wBisquare,
#                                .v_curve = TRUE,
#                                r_min = 0.1, r_max = 0.2,
#                                rm.closed = FALSE, is.continuous = FALSE)
#             l$titlestr = with(sp[1, ], sprintf("%dth %s, %s, [%.2f, %.2f] lambda = %.1f", ID, site, IGBP, lon, lat, l$lambda))
#             # with(l, plot_season(INPUT, brks, title = titlestr, show.legend = FALSE))
#             l
#             # write_fig(expression({with(l, plot_season(INPUT, brks, title = sitename))}), "a.pdf", 10, 4)
#         }, error = function(e){
#             message(sprintf("[e] %d %s: %s", i, sitename, e$message))
#         })
#     }
#     dev.off()
#     SumatraPDF(file)
# }

# {
#     st_info = read_json("st_109.json")
#     st_info = map(st_info, function(l){
#         l$doi %<>% unlist()
#         l$refer %<>% map(unlist)
#         l
#     })
# }
# write_json(st_info[sites_multi], "st_22_multi.json", pretty = TRUE)
# # 采用单growing season的IGBP: MF, WET, SAV, SNO()
# # IGBP (n): multiple growing season sites

# # DE-Akm IT-CA1 US-Me6
# # 8     13     30

# # Warning messages:
# # 1: In max(y_good) : max里所有的参数都不存在；回覆-Inf
# # 2: In max(y_good) : max里所有的参数都不存在；回覆-Inf
# # 3: In (function (INPUT, rFUN = wWHIT, wFUN = wTSM, iters = 2, wmin = 0.1,  :
# #                      Can't find a complete growing season before trim!
# # 4: In (function (INPUT, rFUN = wWHIT, wFUN = wTSM, iters = 2, wmin = 0.1,  :
# #   Can't find a complete growing season before trim!
# # 5: In max(y_good) : max里所有的参数都不存在；回覆-Inf

# ## check about head and tail in d

# {
#     par(mfrow = c(2, 1))
#     # y = d$y
#     y = l$brks$whit$ziter3
#     t = l$brks$whit$t
#     ylim = c(min(diff(y), na.rm = TRUE), max(y, na.rm = TRUE))
#     plot(t, y, type = "l", ylim = ylim); grid()
#     par(new = TRUE)
#     plot(t, c(0, diff(y)), type = "l", col = "red");
#     abline(a = 0, b = 0, col = "blue")
# }

# ## divide into two situations: one growing season and multiple growing season
# {
#     y = d$GPP_NT
#     r = wSG(y, frame = 91, nptperyear = 365, iters = 3)
#     write_fig(expression({
#         plot(y, type = "l")
#         movmean(d$GPP_NT, 20) %>% lines(col = "red", lwd = 1)
#         lines(r$zs %>% last(), col = "blue", lwd = 1)
#         grid()
#     }), "movmean.pdf", 10, 4)
# }
