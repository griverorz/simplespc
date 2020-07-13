#' The generic for the Western Electric ruleset
#'
#' This generic applies the Western Electric rules to \code{SpcData}
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
#' @docType methods
#' @rdname build_zones-methods
setGeneric("western_electric", function(object, rule=1:4, ...)
    standardGeneric("western_electric"))


#' @rdname western_electric-methods
#' @aliases western_electric,ANY,ANY-method
setMethod("western_electric",
          signature="SpcData", 
          function(object, rule, ...) {
              res <- list()
              for (i in rule) {
                  name <- sprintf("Rule%s", i)
                  res[[name]] <- switch(i,
                                        runner(object@yhat, 1, 1,
                                               zone(object@zones, "A")),
                                        runner(object@yhat, 2, 3,
                                               zone(object@zones, "B")),
                                        runner(object@yhat, 4, 5,
                                               zone(object@zones, "C")),
                                        runner(object@yhat, 8, 8,
                                               zone(object@zones, "0")))
              }
              res <- as.data.frame(do.call(cbind, res))
              return(res)
          })

