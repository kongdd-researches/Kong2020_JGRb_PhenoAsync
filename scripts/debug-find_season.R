opt = get_options("season")
opt$rm.closed = TRUE

brks <- lst_brks.multi$`IT-Noe`$brks
l <- brks$fit.raw$`2007`
dt = brks$dt

{
    load_all("I:/Research/phenology/phenofit.R/")
    dt = find_season.default(l$ziter1, l$t, options = opt)
    dt

    size = 2
    ggplot(l, aes(t, ziter1)) +
        geom_line() +
        geom_point(data = dt, aes(peak, y_peak), color = "red", size = size) +
        geom_point(data = dt, aes(beg, y_beg), color = "blue", size = size) +
        geom_point(data = dt, aes(end, y_end), color = "blue", size = size)
    write_fig(last_plot(), "brks_IT-NOE.pdf")
}


