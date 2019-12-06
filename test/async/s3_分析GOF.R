source("test/main_pkgs.R")

load("INPUT/pheno_EVI_st95.rda")
load("INPUT/pheno_NDVI_st95.rda")
load("INPUT/pheno_LAI_st95.rda")
load("INPUT/pheno_gpp_st109.rda")

sites = names(lst_LAI)


sitename = sites[1]
d_gpp = lst_pheno[[sitename]]$doy %>% melt_list("meth")
lst_VI = list(EVI = lst_EVI, NDVI = lst_NDVI, LAI = lst_LAI)

# r <- lst_VI %>% map_depth(3, "pheno") %>% melt_tree(c("type_VI", "site", "group"))
r <- lst_NDVI %>% map_depth(2, "pheno") %>% melt_tree(c("site", "group"))
r <- lst_EVI %>% map_depth(2, "pheno") %>% melt_tree(c("site", "group"))
r <- lst_LAI %>% map_depth(2, "pheno") %>% melt_tree(c("site", "group"))

melt_pheno <- function(lst) {
    res <- foreach(l = lst, i = icount()) %do% {
        # print(i)
        temp <- map(l, "pheno") %>% rm_empty()
        if (!is_empty(temp)) {
            melt_list(temp, "group")
        } else NULL
    } %>% rm_empty()
    melt_list(res, "site")
}
# map(lst_VI, melt_pheno)
# d_LAI = lst_VI$EVI$[[sitename]] %>% map_depth(1, "pheno") %>% melt_list("group")
# d_LAI = lst_LAI[[sitename]] %>% map_depth(1, "pheno") %>% melt_list("group")
# d_LAI = lst_LAI[[sitename]] %>% map_depth(1, "pheno") %>% melt_list("group")

