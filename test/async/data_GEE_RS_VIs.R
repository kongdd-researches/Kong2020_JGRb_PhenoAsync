source("test/main_pkgs.R")


l <- read_json(path.mnt("/mnt/g/CSIRO/ET&GPP/fluxnet212/raw-json/phenoflux212_MCD15A3H_006_463m_buffer.geojson"))$features
x <- l[[1]]

ncluster = 8
InitCluster(ncluster)
# load_pkgs(ncluster, )
df = llply(l, gee_var_len, .parallel = TRUE) %>% # .progress = "text",
    do.call(rbind, .)

lst = llply(l, tidy_gee_json, .progress = "text")

# 检查buffer格点数的连续型
res = dlply(df, .(site), function(d){
    c(var_table(d$Lai), var_table(d$Fpar), var_table(d$FparLai_QC))
})

var_table <- function(x) {
    r = table(x)
    r[names(r) != 0]
}
