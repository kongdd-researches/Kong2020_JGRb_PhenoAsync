# test/async_tries/5.2 GPP_Elasticity.R
#
#' Get 1th back forward derivate
#' @param x A data.table, at least with the column of varnames
Elasticity_GPP <- function(x, predictors){
    formula <- paste(predictors, collapse = "+") %>% {as.formula(paste("GPP~", ., "-1"))}
    # print("formula:"); print(formula)
    dx_z      <- GPP_D1(x, predictors)
    mean_dx_z <- colMeans(dx_z[, ..predictors], na.rm = T) # mean of delta(x)/x_bar

    get_lmcoef <- function(dx_z, formula){
        if (sum(!is.na(dx_z$GPP)) <= 3) return(NULL)
        ## 1. linear regression
        l_lm <- lm(formula, dx_z) # `-1`: rm interception

        # gpp_mean <- x_mean[['GPP']]
        coef   <- coef(l_lm)
        pvalue <- tidy(l_lm) %$% {
            I <- match(names(coef), term)
            set_names(p.value[I], names(coef))
        }

        delta <- mean_dx_z*coef # delta
        perc  <-  delta %>% {./sum(., na.rm = T)}         # contribution portions to GPP changes
        perc_abs <-  abs(delta) %>% {./sum(., na.rm = T)} # contribution portions to GPP changes

        # ## 2. PLSR
        # # pls can't achieve a better performance than lm in training data,
        # # Because, pls only use part information.
        # l_pls <- mvr(GPP~EVI+Rs+T+Prcp+VPD, data = dx)

        df_fit <- data.table(yobs = dx_z$GPP, ypred = predict(l_lm, dx_z))
        # plot(ypred~yobs, df_fit); grid(); abline(a = 0, b = 1, col = "red")
        gof <- with(df_fit, GOF(yobs, ypred))
        res <- listk(coef, pvalue, perc_abs, perc, gof)
        res
    }

    res <- dlply(dx_z, .(dn), get_lmcoef, formula)
    dn  <- as.numeric(names(res))
    ans <- rm_empty(res) %>% purrr::transpose() %>% map(~do.call(rbind, .x) %>% cbind(dn, .) %>% data.table)
    ans
    # res$gof %>% colMeans()
    # pdat <- melt_list(res[1:3], "type") %>%
    #     melt(id.vars = c("dn", "type")) %>%
    #     dcast(dn+variable~type, value.var = "value")
    #
    # ggplot(pdat, aes(dn, perc_abs)) +
    #     geom_point(aes(color = coef >= 0)) +
    #     geom_line() +
    #     facet_wrap(~variable)
}
