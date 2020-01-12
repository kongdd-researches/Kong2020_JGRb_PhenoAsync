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

pdf_acrobat <- function(file) {
    cmd = sprintf('acrobat /A "zoom=100" "%s"', file)
    Ipaper:::shell(cmd)
}
pdf_SumatraPDF <- Ipaper::SumatraPDF

list_files <- function(indir, pattern, del = FALSE) {
    files = dir(indir, pattern, full.names = TRUE)
    if(del) file.remove(files) else files
}

transpose <- purrr::transpose
mutate <- plyr::mutate


stat_sd <- function(x, ...){
    x <- x[!is.na(x)]
    y  <- mean(x)
    y2 <- median(x)
    sd <- sd(x)
    c(y = y, y2 = y2, ymin = y-sd, ymax = y+sd, sd = sd)
}

stat_sd_label <- function(x) {
    x <- x[!is.na(x)]
    y  <- mean(x)
    sd <- sd(x)
    label <- sprintf("%.1f±%3.1f", y, sd)
    y2 <- median(x)
    # browser()
    data.frame(y = y2, label = label)
}


which.null <- function(l) {
    which(sapply(l, is.null))
}

replace_null <- function(l, unlist = TRUE) {
    I_null <- map_lgl(l, is.null) %>% which()
    if (length(I_null) > 0) l[I_null] <- NA
    if (unlist) unlist(l) else l
}

# fluxnet function -------------------------------------------------------------
select_reference <- function(x) {
    if (is.null(x)) {
        return( data.table(type = NA_character_, doi = NA_character_, refer = NA_character_))
    }

    types = map(x, "REFERENCE_USAGE") %>% replace_null() 
    I_best = which(types == "Primary_Citation")

    ans = if (length(I_best) > 0) {
        temp = x[[I_best[1]]]
        list(type = "primary"  , doi = temp$REFERENCE_DOI, refer = temp$REFERENCE_PAPER)
    } else {
        temp = x[[1]]
        list(type = "reference", doi = temp$REFERENCE_DOI, refer = temp$REFERENCE_PAPER)
    }
    replace_null(ans, unlist = FALSE) %>% as.data.table()
}
