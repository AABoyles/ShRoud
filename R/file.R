#' Get Extension
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
get_extension <- function(filename) {
    if (!is.character(filename)) {
        filename <- toString(filename)
    }
    parsed <- strsplit(filename, ".", TRUE)[[1]]
    num <- length(parsed)
    return(tolower(parsed[num]))
}

#' Read File
#'
#' @description This insane thing is designed to reduce the number of
#' dependencies while maintaining a workflow that 'just works'. The tradeoff is
#' that it may behave differently on systems with different sets of packages.
#'
#' TODO: think through how to do this properly, if possible.
#'
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_file <- function(path, ...) {
    fail <- function() {
        stop("This R session doesn't have a parser for this filetype.")
    }
    extension <- get_extension(path)
    if (extension == "arff") {
        return(foreign::read.arff(path, ...))
    }
    if (extension == "csv") {
        if ("readr" %in% .packages(all = TRUE)) {
            load_package("readr")
        } else {
            read_csv <- partial_apply(read.csv, stringsAsFactors = FALSE)
        }
        return(read_csv(path, ...))
    }
    if (extension %in% c("dat", "fwf", "txt")) {
        if ("readr" %in% .packages(all = TRUE)) {
            load_package("readr")
        } else {
            read_table <- partial_apply(read.table, stringsAsFactors = FALSE)
        }
        return(read_table(path, ...))
    }
    if (extension == "dbf") {
        return(foreign::read.dbf(path, ...))
    }
    if (extension == "dta") {
        if ("haven" %in% .packages(all = TRUE)) {
            load_package("haven")
        } else {
            read_dta <- foreign::read.dta
        }
        read_dta(path, ...)
    }
    if (extension == "sasb7dat") {
        load_package("haven")
        return(read_sas(path, ...))
    }
    if (extension == "por") {
        if ("haven" %in% .packages(all = TRUE)) {
            load_package("haven")
        } else {
            read_por <- foreign::read.spss
        }
        return(read_por(path, ...))
    }
    if (extension == "sav") {
        if ("haven" %in% .packages(all = TRUE)) {
            load_package("haven")
        } else {
            read_sav <- foreign::read.spss
        }
        return(read_sav(path, ...))
    }
    if (extension == "spss") {
        if ("haven" %in% .packages(all = TRUE)) {
            load_package("haven")
        } else {
            read_spss <- foreign::read.spss
        }
        return(read_spss(path, ...))
    }
    if (extension == "tsv") {
        if ("readr" %in% .packages(all = TRUE)) {
            load_package("readr")
        } else {
            read_tsv <- partial_apply(read.table, stringsAsFactors = FALSE, sep = "\t")
        }
        return(read_tsv(path, ...))
    }
    if (extension %in% c("xls", "xlsx")) {
        load_package("readxl")
        return(read_excel(path, ...))
    }
    fail()
}
