#' FixNAs
#'
#' @param data a Dataframe with some NAs to replace
#'
#' @return A dataframe without any NAs
#' @export
#'
#' @examples
fixNAs <- function(data){
  for(column in colnames(data)){
    replacement <- "Unknown"
    if(is.integer(data[[column]])){
      replacement <- as.integer(mean(data[[column]], na.rm=TRUE))
    } else if(is.numeric(data[[column]])){
      replacement <- mean(data[[column]], na.rm=TRUE)
    }
    if(length(data[[column]][is.na(data[[column]])]) == nrow(data)){
      data[[column]] <- NULL
    } else {
      data[[column]][is.na(data[[column]])] <- replacement
    }
  }
  data
}

#' Factorize
#'
#' @param data A Dataframe
#'
#' @return A Dataframe with all the character columns replaced by factor columns
#' @export
#'
#' @examples
factorize <- function(data){
  for(column in colnames(data)){
    if(is.character(data[[column]][1])){
      data[[column]] <- as.factor(data[[column]])
    }
  }
  data
}
