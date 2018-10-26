library(plsdepot) # install_github('kongdd/plsdepot')

## Global variables
varnames <- c("EVI", "NDVI", "T", "Prcp", "Rs", "VPD", "GPP", paste0("GPP_t", 1:3))[1:7]
# formula  <- varnames %>% paste(collapse = "+") %>% {as.formula(paste("GPP~", .))}

# ============================ GPP_vpm theory ==================================
# In the order of IGBPname_006
# https://github.com/zhangyaonju/Global_GPP_VPM_NCEP_C3C4/blob/master/GPP_C6_for_cattle.py
epsilon_C3 <- 0.078 # g C m-2 day -1 /W m-2
epsilon_C4 <- c(rep(0, 8), rep(epsilon_C3*1.5, 4), 0, epsilon_C3*1.5)

cal_Tscalar <- function(T, IGBP){
    IGBPname <- c("ENF", "EBF", "DNF", "DBF", "MF" , "CSH",
                "OSH", "WSA", "SAV", "GRA", "WET", "CRO",
                "URB", "CNV")
    Tmin <- c(-1, -2, -1, -1, -1, -1, 1, -1, 1, 0, -1, -1, 0, 0)
    Tmax <- c(40, 48, 40, 40, 48, 48, 48, 48, 48, 48, 40, 48, 48, 48)
    Topt <- c(20, 28, 20, 20, 19, 25, 31, 24, 30, 27, 20, 30, 27, 27)

    I <- match(IGBP[1], IGBPname)
    Tscalar <- (T-Tmax[I])*(T-Tmin[I]) / ( (T-Tmax[I])*(T-Tmin[I]) - (T - Topt[I])^2 )
    Tscalar
}
# param <- data.table(IGBPname, Tmin, Tmax, Topt)
# Tscalar <- (T-Tmax)*(T-Tmin) / ( (T-Tmax)*(T-Tmin) - (T - Topt)^2 )
# Wscaler <- (1 + LSWI) / (1 + LSWI_max)
# ==============================================================================

getRange <- function(d, variable, by = .(IGBP)){
    if (is.quoted(by)) by <- names(by)

    eval(parse(text = sprintf('d[, .(min = min(%s, na.rm = T), max = max(%s, na.rm = T)), by]',
                              variable, variable)))
}

# only for site data
scaleVar_byGPP <- function(d, variable = "EVI"){
    range_org <- getRange(d, variable)[,.(min, max)] %>% as.numeric()
    range_new <- getRange(d, "GPP")[,.(min, max)] %>% as.numeric()

    coef <- lm(range_new~range_org) %>% coef()
    eval(parse(text = sprintf("d[, %s_z := %s*%f+%f]", variable, variable, coef[2], coef[1])))
    d
}
#' scaleGPP_ByCoef
#' scaled GPP into same range of coef, by IGBP
#'
#' @param d A data.frame with the columns of 'min' and 'max' and 'GPP'
scaleGPP_ByCoef <- function(d){
    d <- data.table(d) # The stupid ddply

    range_new  <- d[1, .(min, max)] %>% as.numeric()
    range_org  <- d[, .(min = min(GPP), max = max(GPP))] %>% as.numeric()

    coef <- lm(range_new~range_org) %>% coef()
    d[, GPP_z := (GPP*coef[2]+coef[1])]
    d[, .(min = min(GPP), max = max(GPP))]
    d
}

lm_coef <- function(x) {
    l <- lm(formula, x)
    # glance(l_lm)
    coefs0 <- tidy(l) %$% set_names(estimate, term)
    coefs0 <- coefs0[-1] # omit intercept

    ## reorder coef
    coefs <- rep(NA, length(varnames)) %>% set_names(varnames)
    coefs[match(names(coefs), varnames)] <- coefs0
    c(coefs, n = nrow(x))
}

#' reorder variables
match_varnames <- function(coefs0, varnames){
    ## reorder coef
    coefs <- rep(NA, length(varnames)) %>% set_names(varnames)
    coefs[match(names(coefs0), varnames)] <- coefs0
    coefs
}

pls_coef <- function(x, predictors_var = varnames, response_var = "GPP_DT"){
    if (is.data.table(x)){
        predictors <- x[, .SD, .SDcols = predictors_var]
    } else {
        predictors <- x[, predictors_var]
    }
    predictors %<>% as.matrix()

    # rm ALL is.na variable
    I_col      <- apply(predictors, 2, function(x) !all(is.na(x))) %>% which()
    predictors <- predictors[, I_col]

    response  <- x[[response_var]] %>% as.matrix(ncol = 1)
    # ERROR when comps = 1, 20180926
    l_pls     <- plsreg1(predictors, response, comps = 2, crosval = TRUE)
    coefs     <-  l_pls$std.coefs %>% match_varnames(predictors_var)

    # c(coefs, n = nrow(x))
    VIP <- l_pls$VIP %>% .[nrow(.), ] %>% match_varnames(predictors_var)
    n = nrow(x)
    c(coef = coefs, VIP = VIP, n = n)
    # list(coef = c(coefs, n = n),
    #     VIP = c(VIP, n = n))
}

#' autocorrelation coefficientn
get_acf <- function(x){
    acf(x, lag.max = 10, plot = F, na.action = na.pass)$acf[,,1][-1]
}

#' add previous time step GPP as a new variable
#' @param x Data.table with the column of ydn and GPP
addPredictor_tn <- function(x){
    I0 <- x$ydn
    I_1  <- match(I0 - 1, I0)
    I_2  <- match(I0 - 2, I0)
    I_3  <- match(I0 - 3, I0)

    x$GPP_t1 <- x$GPP[I_1]
    x$GPP_t2 <- x$GPP[I_2]
    x$GPP_t3 <- x$GPP[I_3]
    x
}

## 2. Test the difference of `pls` and 'lm'
test <- function(){
    par(mfrow = c(2, 1))
    # yhat <- predict(pls1_one, predictors)
    plot(response[[1]], l_pls$y.pred);abline(a = 0, b = 1, col = "red"); grid()

    yhat_lm <- predict(l_lm, x)
    plot(response[[1]], yhat_lm);abline(a = 0, b = 1, col = "red"); grid()


    fit <- data.table(yobs = response[[1]],
                      y_pls = l_pls$y.pred,
                      y_lm = yhat_lm)

    with(na.omit(fit),
         list(pls = GOF(yobs, y_pls) %>% as.data.frame.list(),
              lm  = GOF(yobs, y_lm ) %>% as.data.frame.list()) %>% melt_list("meth"))
    with(fit,
         list(pls = GOF(yobs, y_pls) %>% as.data.frame.list(),
              lm  = GOF(yobs, y_lm ) %>% as.data.frame.list()) %>% melt_list("meth"))
}


## visualization

#' @param p2 should have axis.tick.y.right and axis.title.y.left
ggplot_multiAxis <- function(p1, p2, show = TRUE){
    # intersperse a copy of the bottom axes
    g1 <- p1
    g2 <- p2
    if (!("gtable" %in% class(p1)))  g1 <- ggplotGrob(p1)
    if (!("gtable" %in% class(p2)))  g2 <- ggplotGrob(p2)

    g1_grob_names <- map_chr(g1$grob, "name")
    g2_grob_names <- map_chr(g2$grob, "name")

    I_panel1 <- g1$grobs %>%  {grep("panel", g1_grob_names)}
    panel2   <- g2$grobs %>%  {.[[grep("panel", g2_grob_names)]]}
    g1$grobs[[I_panel1]] %<>% addGrob(panel2)

    ## 2. find ylab-r position
    I_yr1  <- g1$layout %$% {r[grep("ylab-r", name)]} %>% unique()
    I_yr2  <- g2$layout %$% {r[grep("axis-r|ylab-r", name)]} %>% unique() # get `axis-r` and `ylab-r`

    all <- gtable:::cbind.gtable(
        g1[, seq(max(I_yr1))],
        g2[, I_yr2],
        g1[, seq(max(I_yr1)+1, ncol(g1))],
        size = "first")

    if (show){
        grid.newpage()
        grid.draw(all)
    }
    all

    # layout   <- g$layout %>% mutate(vp = sprintf("%s.%d-%d-%d-%d", name, t, r, b, l)) %>% data.table()
    # vp_panel <- layout[name == "panel", vp]

    # ## 2. overlap the new panel
    # downViewport(vp_panel)
    # grid.draw(panel)
}


GPP_D1 <- function(x, predictors){
    varnames <- c(predictors, "GPP")
    ## 1. dx cal should be by site
    x <- data.table(x)
    headvars <- c("site", "date", "year", "ydn", "dn")
    I_t0 <- x$ydn
    I_t1 <- match(I_t0 - 1, I_t0) # previous time step

    # back forward derivate
    x_t1 <- x[I_t1]
    dx   <- x[, ..varnames] - x_t1[, ..varnames] # first order derivate

    # dx divide x_bar, absolute change become relative change
    mean_dx <- dx[, ..varnames] %>% colMeans(na.rm = T)
    mean_x  <- x[, ..varnames] %>% colMeans(na.rm = T)

    # standardized by mean. `_z` means standardized
    mean_x_inv <- rep(1/mean_x, nrow(dx)) %>% matrix(byrow = T, nrow = nrow(dx))

    dx_z <- dx[, ..varnames] * mean_x_inv
    dx_z <- cbind(x[, ..headvars], dx_z)

    dx_z
}

########################### ELASTICITY FUNCTIONS ###############################
figureNo <- 0

# parameter for loess
smooth_formula <- y~poly(x, 2)
span <- 1

ggplot_1var <- function(x, varname = "APAR", color = "red"){
    p <- ggplot(x, aes_string("dn", varname, color = "year")) +
        # geom_point(color = "transparent") +
        geom_smooth(method = "loess", formula = smooth_formula, span = span,
                    color = color)+ #fill = color,
        scale_y_continuous(position = "right") +
        theme(panel.background = element_rect(fill = "transparent"),
              axis.ticks.y.right = element_line(color = color),
              axis.text.y.right = element_text(color = color),
              axis.title.y.right = element_text(color = color),
              panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank()) # get rid of minor grid
    p
}

# global variables:
# st, info_async
check_sensitivity <- function(x, predictors, nptperyear = 46,
    grp1 = c("EVI", "APAR", "Rs", "epsilon_eco", "epsilon_chl", "GPPsim"),
    col1 = c("green", "red", "purple", "darkorange1", "yellow2", "darkgreen"),
    grp2 = c("EVI", "T", "VPD", "VPD_trans", "Prcp", "Wscalar", "Tscalar"),
    col2 = c("green", "yellow2", "darkorange1", "purple", "skyblue", "blue", "yellow4")
    )
{
    ## 0. prepare plot data
    dx_z <- GPP_D1(x, predictors) # only suit for by site

    p <- ggplot(x, aes(dn, GPP, color = year)) +
        # geom_point() +
        geom_smooth(method = "loess", formula = smooth_formula, span = span, color = "black") %>% list(.)

    FUN_plot <- function(varname, color) list(ggplot_1var(x, varname, color))
    ps_1 <- mapply(FUN_plot, grp1, col1) %>% c(list(p), .)
    ps_2 <- mapply(FUN_plot, grp2, col2) %>% c(list(p), .)

    # p_all <- ggplot_multiAxis(p, p_apar)
    p1 <- reduce(ps_1, ggplot_multiAxis, show = F)
    p2 <- reduce(ps_2, ggplot_multiAxis, show = F)

    fontsize <- 14
    titlestr <- st[site == sitename, ] %$%
        sprintf("[%03d,%s] %s, lat=%.2f, nyear=%.1f", ID, IGBP, site, lat, nrow(x)/nptperyear)
    biasstr  <- with(info_async[site == sitename], sprintf("bias: sos=%.1f,eos=%.1f", spring, autumn))
    titlestr <- paste(titlestr, biasstr, sep = "  ")

    title <- textGrob(titlestr, gp=gpar(fontsize=fontsize, fontface = "bold"))

    p_series <- arrangeGrob(p1, p2, ncol = 1, top = title)
    # grid.newpage();grid.draw(p_series)
    # ggplot_build(p)$layout$panel_scales_y[[1]]$range$range

    ## 2. mete forcing
    delta <- dx_z %>% melt(c("site", "date", "year", "ydn", "dn", "GPP"))

    formula <- y ~ x
    p_mete <- ggplot(delta[dn > 10], aes(value, GPP)) +
        geom_point() +
        facet_wrap(~variable, scales = "free", ncol = 2) +
        geom_smooth(method = "lm", se=T, formula = formula) +
        stat_poly_eq(formula = formula,
                    eq.with.lhs = "italic(hat(y))~`=`~",
                    rr.digits = 2,
                    aes(label = paste(..eq.label.., ..rr.label.., sep = "~"), color = "red"),
                    parse = TRUE) +
        ggtitle("dGPP ~ dx")
    # print(p_mete)

    # avoid empty figure in first page
    if (!(exists("figureNo") & figureNo == 0)){
        grid.newpage()
    }
    if (exists("figureNo")) figureNo <<- figureNo + 1
    arrangeGrob(p_series, p_mete) %>% grid.draw()
}

# plot(pls1_one, "observations")
# l <- lm(GPP ~ Rn + VPD + Prcp + T + EVI, x) #%>% plot()



