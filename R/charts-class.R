#' Union between numeric and matrix
setClassUnion("NumericOrMatrix", c("numeric", "matrix"))


#' A base class for SPC models
#' 
#' @slot data An instance of numeric or matrix
#' @rdname spcchart
setClass(Class="SpcChart",
         slots=list(data="NumericOrMatrix"),
         contains="VIRTUAL")


#' @rdname spcchart
setClass(Class="xbarchart",
         slots=list(data="numeric"),
         contains="SpcChart")


#' @rdname spcchart
setClass(Class="cchart",
         slots=list(data="numeric"),
         contains="SpcChart")


#' @rdname spcchart
setClass(Class="pchart",
         slots=list(data="matrix"),
         contains="SpcChart")


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
#' @slot data A numeric vector or a matrix
#' @slot stats A numeric vector
#' @slot sd A numeric vector
#' @slot zones A \code{zone} object
setClass("SpcData",
         representation=list("data"="NumericOrMatrix",
                             "yhat"="numeric",
                             "stats"="numeric",
                             "sd"="numeric",
                             "zones"="Zone"))


setValidity("SpcData",
            function(object) {
                errors <- character()
                if (any(is.na(object@data))) {
                    msg <- "Missing values not allowed in data"
                    errors <- c(errors, msg)
                }
                if (length(errors) == 0) TRUE else errors                
            })

