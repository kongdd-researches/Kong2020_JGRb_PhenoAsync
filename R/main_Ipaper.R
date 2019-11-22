# FUNCTIONS moved to `Ipaper` in the future.

#' check_file
#' 
#' @param file 
#' 
#' @return A integer indicating file status
#' - `0`  : file  already exist
#' - `-1` : file isn't exist, and can't open it 
#' - `1`  : ok
#' 
#' @keywords internal
#' @export
check_file <- function(file, outdir){
    outfile <- paste0(outdir, basename(file))

    tag <- 1
    if (!file.exists(file)){
        message(sprintf("[%s] is not exist. can't open it!", file))
        tag <- -1
    }
    if (file.exists(outfile)){
        message(sprintf("[%s]: already exist!", outfile))
        tag  <- 0
    }
    if (tag < 1) outfile <- ""
    return(outfile)
}

SumatraPDF = Ipaper:::cmd_func("SumatraPDF.exe")
