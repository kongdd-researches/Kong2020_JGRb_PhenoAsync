# works for multiple projects
load_all <- function (path = ".", reset = TRUE, recompile = FALSE, 
    export_all = TRUE, helpers = TRUE, quiet = FALSE, ...) 
{
    if (inherits(path, "package")) {
        path <- path$path
    }
    save_all()
    check_dots_used()

    pkgload::load_all(path = ".", reset = reset, recompile = recompile, 
        export_all = export_all, helpers = helpers, quiet = quiet, ...)
    pkgload::load_all(path = "../phenofit", reset = reset, recompile = recompile, 
        export_all = export_all, helpers = helpers, quiet = quiet, ...)
}

suppressWarnings({
    environment(load_all) <- environment(devtools::build)
    assignInNamespace("load_all", load_all, ns="devtools")  
})

