library(plsdepot) # install_github('kongdd/plsdepot')

# x <- na.omit(x)
# x <- d[IGBP == "GRA" & d16 == 10]

## Global variables
varnames <- c("EVI", "NDVI", "T", "Prcp", "Rs", "VPD", paste0("GPP_t", 1:3))[1:6]
formula  <- varnames %>% paste(collapse = "+") %>% {as.formula(paste("GPP~", .))}

fix_neg <- function(x){
    x[x < 0] <- 0
    x
}

getRange <- function(d, variable, by = .(IGBP)){
    if (is.quoted(by)) by <- names(by)

    eval(parse(text = sprintf('d[, .(min = min(%s, na.rm = T), max = max(%s, na.rm = T)), by]',
                              variable, variable)))
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
addPredictor_tn <- function(x){
    I0 <- x$yd16
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




# plot(pls1_one, "observations")
# l <- lm(GPP ~ Rn + VPD + Prcp + T + EVI, x) #%>% plot()
