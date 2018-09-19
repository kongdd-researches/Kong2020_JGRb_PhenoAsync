d <- df_lst[c(2, 4)] %>% melt_list("model")

d <- melt(d, id.vars = c("model", "site", "meth","flag", "origin"), variable.name = "index") %>%
    dcast(meth+site+flag+origin+index~model)


pattern_sos <- "sos|UD|SD|Greenup|Maturity"
pattern_eos <- "eos|DD|RD|Senescence|Dormancy"

d[grep(pattern_sos, index), period := "sos"]
d[grep(pattern_eos, index), period := "eos"]
d[abs(`GPP[obs]` - MOD13A1_EVI) >= 300, outlier := 1]
d[abs(`GPP[obs]` - MOD13A1_EVI) <  300, outlier := 0]

ggplot(d[period == "sos"], aes(`GPP[obs]`, MOD13A1_EVI, color = index)) +
    geom_point() + facet_wrap(~meth) +
    geom_abline(slope = 1, color = "red", size = 1) +
    geom_smooth(method = "lm") +
    geom_text(data = d[outlier == 1], aes(label = site), alpha = 1, show.legend = F)

ggplot(d[period == "eos"], aes(`GPP[obs]`, MOD13A1_EVI)) + geom_point() + facet_wrap(~meth) +
    geom_abline(slope = 1, color = "red", size = 1) +
    geom_smooth(method = "lm")
