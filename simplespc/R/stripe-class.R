#' An S4 class to represent an stripe
#'
#' @slot lower A numeric vector representing the lower bound of the stripe
#' @slot upper A numeric vector representing the upper bound of the stripe
#' @rdname stripe
setClass("Stripe",
         slots=list(lower="numeric", upper="numeric"))


# Validator for the class Stripe
setValidity("Stripe",
            function(object) {
                errors <- character()
                if (length(object@lower) != length(object@upper)) {
                    msg <- "Lower and upper bounds have different lengths"
                    errors <- c(errors, msg)
                }
                if (any(is.na(object@lower) | is.na(object@upper))) {
                    msg <- "Stripes cannot include missing values"
                    errors <- c(errors, msg)
                }
                if (any(object@lower > object@upper)) {
                    msg <- "The lower bound is larger than the upper bound"
                    errors <- c(errors, msg)
                }
                if (length(errors) == 0) TRUE else errors
            })


#' Calculate the length of a stripe
#'
#' @param x A stripe
#' @return An integer
#' 
#' @rdname stripe-methods
#' @aliases length,ANY,ANY-method
#' 
#' @export
#' @examples
#' st <- stripe(c(1, 1), c(2, 2))
#' length(st)
setMethod("length", signature="Stripe", function(x) length(x@upper))


#' Extend a stripe a number of times
#'
#' @param x A stripe
#' @param times An integer giving the non-negative number of times to
#'     extend the stripe
#' @return A stripe extended \code{times} times
#'
#' @rdname stripe-methods
#' @aliases rep,ANY,ANY-method

#' @export
#' @examples
#' st <- stripe(c(1, 1), c(2, 2))
#' rep(st, 2)
setMethod("rep",
          signature="Stripe",
          function(x, times) {
              stripe(rep(x@lower, times),
                       rep(x@upper, times))
          })


#' Instantiate an \code{Stripe} object
#'
#' This function creates a new instance of \code{Stripe} which is
#' defined by lower and upper bounds
#'
#' @param lower A numeric vector 
#' @param upper A numeric vector
#' 
#' @export
#' @rdname stripe
#' 
#' @examples
#' stripe(c(1, 1), c(2, 2)) 
stripe <- function(lower=-Inf, upper=Inf) {
    return(new("Stripe", lower=lower, upper=upper))
}


#' Check if one object belongs to an stripe
#'
#' @param x A numeric vector or a stripe
#' @param y A stripe
#' @param strict A boolean vector of length one or two. Value
#'     \code{TRUE} indicates strict inequality. Value \code{FALSE}
#'     indicates weak inequality. If a vector of length two is passed,
#'     the first element will be interpreted as the comparison to be
#'     applied to the lower bound.
#' @return A boolean vector with value TRUE is
#' @export
#' 
#' @rdname belongs-methods
#' @aliases belongs,ANY,ANY-method
setGeneric("belongs", function(x, y, ...) standardGeneric("belongs"))


#' @rdname belongs-methods
#' @aliases belongs,ANY,ANY-method
setMethod("belongs",
          signature=c("numeric", "Stripe"),
          function(x, y, strict=FALSE) {
              if (!is.logical(strict) | length(strict) > 2) {
                  stop(sprintf("Argument %s must have length <= 2", 
                               shQuote(strict)))
              }
              if (length(x) != length(y)) {
                  stop("The two arguments must have equal length")
              }
              if (unique(strict) == TRUE) {
                  res <- x > y@lower & x < y@upper
              } else if (unique(strict) == FALSE) {
                  res <- x >= y@lower & x <= y@upper
              } else if (strict == c(TRUE, FALSE)) {
                  res <- x > y@lower & x <= y@upper
              } else {
                  res <- x >= y@lower & x < y@upper
              }
              return(res)
          })


#' @rdname belongs-methods
#' @aliases belongs,ANY,ANY-method
setMethod("belongs",
          signature=c("Stripe", "Stripe"),
          function(x, y, strict=TRUE) {
              return(belongs(x@upper, y) & belongs(x@lower, y))
          })
