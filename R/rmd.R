#' RMarkdown to R Script
#'
#' This function will read a standard R markdown source file and convert it to
#' an R script to allow the code to be run using the "source" function.
#'
#' The function is quite simplisting in that it reads a .Rmd file and adds
#' comments to non-r code sections, while leaving R code without comments
#' so that the interpreter can run the commands.
#'
#' @param infile The path to the .Rmd file
#' @export
rmd2rscript <- function(infile){
  flIn <- readLines(infile)
  cdStrt <- which(grepl(flIn, pattern = "```{r*", perl = TRUE))
  cdEnd <- sapply(cdStrt, function(x){
    preidx <- which(grepl(flIn[-(1:x)], pattern = "```", perl = TRUE))[1]
    return(preidx + x)
  })
  flIn[c(cdStrt, cdEnd)] <- ""
  expFun <- function(strt, End){
    strt <- strt+1
    End <- End-1
    return(strt:End)
  }
  idx <- unlist(mapply(FUN = expFun, strt = cdStrt, End = cdEnd, SIMPLIFY = FALSE))
  comIdx <- 1:length(flIn)
  comIdx <- comIdx[-idx]
  for(i in comIdx){
    flIn[i] <- paste("#' ", flIn[i], sep = "")
  }
  nm <- strsplit(infile, split = "\\.")[[1]][1]
  flOut <- file(paste(nm, "[rmd2r].R", sep = ""), "w")
  for(i in 1:length(flIn)){
    cat(flIn[i], "\n", file = flOut, sep = "\t")
  }
  close(flOut)
}
