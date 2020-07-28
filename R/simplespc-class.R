#' Main call to apply SPC to some data
#'
#' This function is the main entry point for the package. It provides
#' a simple way to apply different rules for different types of data.
#' It is possible to use the function in training mode so that rather
#' than returning the anomalies to some given input data, it only
#' calculates the information necessary (an instance of
#' \code{SpcData}) that can be used to evaluate other input data.
#' 
#' @param data Either a numeric vector/matrix or an instance of
#'     \code{SpcData}
#' @param chart A string with the name of a chart. The chart is
#'     expected to exist as a class inheriting from \code{SpcChart}.
#'     See \code{\link{spcchart}}.
#' @param ruleset A string with the name of a given ruleset. Currently
#'     only "WE" (for "Western Electric") is implemented.
#' @param rules An integer vector with the rules to be calculated. It
#'     defaults to all rules for Western Electric.
#' @return An instance of \code{SimpleSpc} that contains both the
#'     input data as well as a matrix of booleans indicating the
#'     anomalies corresponding to each rule.
#' 
#' @export
simplespc <- function(data,
                      chart,
                      ruleset="WE",
                      rules=1:4) {
    if (is.null(ruleset) || ruleset == "") {
        zones <- build_zones(new(chart, data=data))
        return(zones)
    } else {
        if (!inherits(data, "SpcData")) {
            data <- build_zones(new(chart, data=data))
        } else if (!is.null(chart) || chart != "") {
            warning("chart will be ignored")
        }
        if (ruleset == "WE") {
            res <- western_electric(data)
        }
    }
    return(new("SimpleSpc", data=data, anomalies=res))
}
    

#' The \code{SimpleSpc} class
#'
#' @slot data An instance of \code{SpcData}
#' @slot anomalies A data.frame of booleans
#' @export
setClass("SimpleSpc",
         representation=list(data="SpcData",
                             anomalies="data.frame"))


#' Validates a SimpleSpc instance
setValidity("SimpleSpc",
            function(object) {
                errors <- character()
                if (length(object@data@yhat) != ncol(object@anomalies)) {
                    msg <- "Something went wrong"
                    msg <- c(errors, msg)
                }
                if (!is.logical(object@anomalies)) {
                    msg <- "Anomalies should be a boolean data.frame"
                    msg <- c(errors, msg)
                }
                if (length(errors) == 0) TRUE else errors
            })


#' Show method for SimpleSpc
setMethod("show",
          signature="SimpleSpc",
          function(object) cat(sprintf("Detected %s anomalies",
                                   sum(rowSums(object@anomalies))), "\n"))
