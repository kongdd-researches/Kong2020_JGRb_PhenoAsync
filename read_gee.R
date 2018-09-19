files <- dir("F:/Github/data/flux/fluxnet212", pattern = "phenoflux.*", full.names = T) %>%
    set_names(str_extract(basename(.), "(?<=_).*(?=_)"))
lst_new <- llply(files, fread, .progress = "text")
list2env(lst_new, .GlobalEnv)


vars_all <- c("site", "date", "scale", "NDVI", "EVI", "SummaryQA",
              "Lai", "Fpar", "FparLai_QC", "Gpp", "Psn_QC", "ET", "ET_QC")

select_vars <- . %>% {names(.)[4:ncol(.)]}

# main script -------------------------------------------------------------

vars_com <- c("site", "date", "scale")

vars <- colnames(df) %>% intersect(vars_all, .)
var_qc <- grep("QC|QA", vars) %>% vars[.]
vars <- setdiff(vars, c(var_qc, vars_com))


d <- df[, lapply(.SD, median, na.rm = T), .(site, date, scale), .SDcols = vars]

vars2 <- c(vars_com, var_qc)
d_qc  <- df[scale == scale0, ..vars2]

b <- dcast(a, site+date~scale, value.var = vars)



grep("QC|QA", vars_all) %>% vars_all[.]
