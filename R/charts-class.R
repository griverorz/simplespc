# Union between numeric and matrix
setClassUnion("NumericOrMatrix", c("numeric", "matrix"))


#' A base class for SPC models
#'
#' All charts are expected to inherit from this class. The class
#' defines a single argument \code{data} that can be either a numeric
#' vector or a matrix. The class does not impose any restrictions on
#' the matrix but it is intended to be used for charts that require
#' (observations, sample sizes)
#' 
#' @slot data An instance of numeric or matrix
#' @rdname spcchart
#' @include zone-class.R
setClass(Class="SpcChart",
         slots=list(data="NumericOrMatrix"),
         contains="VIRTUAL")


#' The class for x-bar charts
#'
#' @rdname spcchart
setClass(Class="xbarchart",
         slots=list(data="numeric"),
         contains="SpcChart")


#' The class for c charts
#' 
#' @rdname spcchart
setClass(Class="cchart",
         slots=list(data="numeric"),
         contains="SpcChart")


#' The class for p charts
#' 
#' @rdname spcchart
setClass(Class="pchart",
         slots=list(data="matrix"),
         contains="SpcChart")


# The input data for p charts is validated to ensure that the input
# matrix contains two columns and that the second column can
# reasonably represent the sample sizes from which the counts in the
# first column are extracted
setValidity("pchart",
            function(object) {
                errors <- character()
                if (ncol(object@data) != 2) {
                    msg <- "Input matrix must have two columns"
                    msg <- c(errors, msg)
                }
                if (any(object@data[, 1] < object@data[, 2])) {
                    msg <- "Sizes are larger than the observations"
                    msg <- c(errors, msg)                        
                }
                if (length(errors) == 0) TRUE else errors                
            }) 


#' An S4 class to represent SPC Data
#'
#' This class is used to host the necessary data to apply a ruleset to
#' given input data. The class facilitates building and testing the
#' validity of the different zones that define statistical process
#' control. 
#' 
#' @slot data A numeric vector or a matrix. This slot should contain
#'     the raw data that was passed to the chart.
#' @slot yhat A numeric representation of the \code{data} slot that is
#'     used as centerline.
#' @slot statistics A numeric vector with the calculated location.
#' @slot deviation A numeric vector with the standard deviation used
#'     for the computation of the difference zones. It can have length
#'     > 1 if the chart has variable limits.
#' @slot zones A \code{zone} object with the definition of the three
#'     zones ("0", "A", "B", "C") that are used to evaluate the input
#'     data.
#' @rdname spcdata
setClass("SpcData",
         representation=list("data"="NumericOrMatrix",
                             "yhat"="numeric",
                             "statistics"="numeric",
                             "deviation"="numeric",
                             "zones"="Zone"))


# The implementation of SPC needs to impose some constraints on the
# data. Currently, only the fact that the data does not contain
# missing values is verified.
setValidity("SpcData",
            function(object) {
                errors <- character()
                if (any(is.na(object@data))) {
                    msg <- "Missing values not allowed in data"
                    errors <- c(errors, msg)
                }
                if (length(errors) == 0) TRUE else errors                
            })

