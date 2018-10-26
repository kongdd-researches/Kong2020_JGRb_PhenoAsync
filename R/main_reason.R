#' define_period
#' Define growing periods according to history phenology metrics.
#' 
define_period <- function(his_phenology, delta = 8){
    periods <- c(
        "TRS1.sos ~ TRS5.sos",
        "TRS5.sos ~ pop",
        "pop ~ TRS5.eos",
        "TRS5.eos ~ TRS1.eos",
        "TRS5.sos ~ TRS5.eos",
        "UD ~ SD",
        "SD ~ DD",
        "DD ~ RD",
        "SD ~ pop",
        "pop ~ DD"
    )

    info <- with(his_phenology, list(
        # TRS duration
        c(TRS1.sos, TRS5.sos),
        c(TRS5.sos, DER.pop),
        c(DER.pop , TRS5.eos),
        c(TRS5.eos , TRS1.eos),
        c(TRS5.sos, TRS5.eos),

        # Gu period
        c(GU.UD, GU.SD),
        c(GU.SD, GU.DD),
        c(GU.DD, GU.RD),
        c(GU.SD, DER.pop),
        c(DER.pop, GU.DD)
    )) %>% do.call(rbind, .) %>% as.data.table() %>% 
        set_colnames(c("begin", "end")) %>% cbind(period = periods, .)
    info[, `:=`(dn_begin = floor(begin/delta), 
                dn_end = ceiling(end/delta))]
    info
}

stat_mete <- function(x){
    levels <- c("small", "normal", "high")

    qval <- quantile(x,  probs = seq(0.25, 0.75, 0.25), na.rm = T)
    flag <- rep("normal", length(x))

    flag[x <= qval[1]] <- levels[1]
    flag[x >= qval[3]] <- levels[3]
    factor(flag, levels)
}


melt_metrics <- function(d){
    vars <- intersect(metrics, colnames(d))
    res <- melt(d, measure.vars = vars, variable.name = "index")
    res$phase <- metric_phase(res$index)
    res
}


aov_IndexDiff <- function(df){
    ddply(df, .(index), function(d) TukeyHSD(aov(value~type, d))$type)
}

aov_MeanDiff <- function(dpi, level = 0.05){
    # 1. ANOVA analysis to check the diff of EVI and GPP
    info_metric <- aov_IndexDiff(dpi)

    # spread into matrix, construct period index
    dpi_matrix <- dpi %>% dplyr::select(-matches("mean|anorm|phase")) %>% spread(index, value)
    dpi_duration <- dpi_matrix[, .(s1 = GU.SD - GU.UD,
           s2 = GU.DD - GU.SD,
           s3 = GU.RD - GU.DD,
           los_GU   = GU.RD - GU.UD,
           los_trs1 = TRS1.eos - TRS1.sos,
           los_trs5 = TRS5.eos - TRS5.sos
    ), .(flag, type)]

    info_duration <- dpi_duration %>% melt(c("flag", "type"), variable.name = "index") %>% aov_IndexDiff()
    info <- list(metric = info_metric, duration = info_duration)
    list(info=info, info.sign = map(info, ~.[`p adj` <= level]))
}

VIP_pls <- function(object) {
  # if (object$method != "oscorespls")
  #     stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  # if (nrow(object$Yloadings) > 1)
  #     stop("Only implemented for single-response models")
  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  Wnorm2 <- colSums(object$loading.weights^2)
  SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}
