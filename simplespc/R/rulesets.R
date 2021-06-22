#' The generic for the Western Electric ruleset
#'
#' This generic applies the Western Electric rules to \code{SpcData}
#'
#' @docType methods
#' @rdname western_electric-methods
setGeneric("western_electric", function(object, rule=1:4, ...)
    standardGeneric("western_electric"))


#' Methods for Western Electric ruleset
#' 
#' @param object An instance of \code{SpcData}
#' @param rule A vector of integers indicating the rule to be applied
#'     to the data. It defaults to rules 1 to 4.
#' @param ... Additional parameters. Currently not implemented
#' @return A data.frame of booleans with as many rows as data points
#'     in \code{data} and as many columns as rules specified in
#'     \code{rule}.
#' 
#' @export
#' @rdname western_electric-methods
#' @aliases western_electric,ANY,ANY-method
setMethod("western_electric",
          signature="SpcData",
          function(object, rule, ...) {
              res <- list()
              for (i in rule) {
                  name <- sprintf("Rule%s", i)
                  res[[name]] <- switch(as.character(i),
                                        "1"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         1,
                                                                         zone(object@zones, "A")),
                                                     1),
                                        "2"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         2,
                                                                         zone(object@zones, "B")),
                                                     3),
                                        "3"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         4,
                                                                         zone(object@zones, "C")),
                                                     5),
                                        "4"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         8,
                                                                         zone(object@zones, "0")),
                                                     8))
              }
              res <- as.data.frame(do.call(cbind, res))
              return(res)
          })


#' The generic for the Nelson ruleset
#'
#' This generic applies the Nelson rules to \code{SpcData}
#'
#' @docType methods
#' @rdname nelson-methods
setGeneric("nelson", function(object, rule=1:8, ...)
    standardGeneric("nelson"))


#' Methods for Western Electric ruleset
#' 
#' @param object An instance of \code{SpcData}
#' @param rule A vector of integers indicating the rule to be applied
#'     to the data. It defaults to rules 1 to 8.
#' @param ... Additional parameters. Currently not implemented
#' @return A data.frame of booleans with as many rows as data points
#'     in \code{data} and as many columns as rules specified in
#'     \code{rule}.
#' 
#' @export
#' @rdname nelson-methods
#' @aliases nelson,ANY,ANY-method
setMethod("nelson",
          signature="SpcData", 
          function(object, rule, ...) {
              res <- list()
              for (i in rule) {
                  name <- sprintf("Rule%s", i)
                  res[[name]] <- switch(as.character(i),
                                        "1"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         1,
                                                                         zone(object@zones, "A")),
                                                     1),
                                        "2"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         9,
                                                                         zone(object@zones, "0")),
                                                     9),
                                        "3"=chunkify(object@yhat,
                                                     monotone,
                                                     6),
                                        "4"=chunkify(object@yhat,
                                                     zigzag,
                                                     14),
                                        "5"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         2,
                                                                         zone(object@zones, "B")),
                                                     3),
                                        "6"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         4,
                                                                         zone(object@zones, "C")),
                                                     5),
                                        "7"=chunkify(object@yhat,
                                                     function(x) inside(x,
                                                                        15,
                                                                        zone(object@zones, "C")),
                                                     15),
                                        "8"=chunkify(object@yhat,
                                                     function(x) outside(x,
                                                                         8,
                                                                         zone(object@zones, "B"),
                                                                         mixing=TRUE), 
                                                     8))
              }
              res <- as.data.frame(do.call(cbind, res))
              return(res)
          })
