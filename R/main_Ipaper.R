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

# works
merge_pdf <- function(outfile = "RPlot.pdf", indir = 'Figure', pattern = "*.pdf", del = FALSE){
    files <- dir(indir, pattern, full.names = TRUE) #%>% gsub("/", "\\\\", .)
    order <- str_extract(basename(files), "(?<=\\[)\\d*(?=.*\\])") %>% as.numeric() %>% order()
    if (all(is.finite(order))) files = files[order]

    str_files = paste(files, collapse = "' '") %>% paste0("'", ., "'")
    # str_files = paste(files, collapse = " ")
    cmd <- sprintf("pdfmerge -o %s %s", outfile, str_files)
    # print(cmd)

    app = ifelse(.Platform$OS.type == "windows", "powershell", "")
    if (.Platform$OS.type == "windows") {
        status = Ipaper:::shell(cmd, shell = app, wait = TRUE, ignore.stderr = FALSE)
    } else {
        status = system(cmd, wait = TRUE, ignore.stderr = FALSE)
    }
    if (status != 0) {
        print(status)           
    }
    if (del) file.remove(files)
}


pdf_SumatraPDF <- SumatraPDF

pdf_acrobat <- function(file) {
    cmd = sprintf('acrobat /A "zoom=100" "%s"', file)
    Ipaper:::shell(cmd)
}

list_files <- function(indir, pattern, del = FALSE) {
    files = dir(indir, pattern, full.names = TRUE)
    if(del) file.remove(files) else files
}
