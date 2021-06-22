#' Test if a number of observations fall outside an interval
#'
#' @param x A numeric vector
#' @param howmany An integer with how many observations in x need to
#'     be outside an interval
#' @param interval A stripe
#' @param mixing A boolean. If TRUE, outside observations need to be
#'     on different sides of the interval. If TRUE, outside
#'     observations need to be on the same side of the interval.
#'     Default is FALSE
#' @return A boolean with value TRUE if \code{howmany} observations in
#'     \code{x} fall outside of \code{interval}
#' @export
#' 
#' @examples
#' outside(c(10, 10, 10, 10), 3, stripe(-1, 1))
#' outside(c(10, 10, 10, 10), 3, stripe(-1, 1), mixing=TRUE)
outside <- function(x, howmany, interval, mixing=FALSE) {
    res <- FALSE
    ucl <- interval@upper
    lcl <- interval@lower
    if (mixing) {
        above <- sum(x > ucl)
        below <- sum(x < lcl)        
        if (above != 0 & below != 0 & above + below >= howmany)
            res <- TRUE
    } else {
        if (sum(x > ucl) >= howmany) {
            res <- TRUE
        }
        if (sum(x < lcl) >= howmany) {
            res <- TRUE
        }        
    }    
    return(res)
}


#' Tests if a number of observations fall inside an interval
#'
#' @param x A numeric vector
#' @param howmany An integer with how many observations in x need to
#'     be inside an interval
#' @param interval A stripe
#' @return A boolean with value TRUE if \code{howmany} observations in
#'     \code{x} fall inside of \code{interval}
#' @export
#' 
#' @examples
#' inside(c(10, 10, 10, 10), 3, stripe(-100, 100))
inside <- function(x, howmany, interval) {
    res <- FALSE
    ucl <- interval@upper
    lcl <- interval@lower
    if (sum(x > lcl & x < ucl) >= howmany) {
        res <- TRUE
    }
   return(res)
}


#' Tests if a vector has an oscillating pattern
#'
#' @param x A numeric vector
#' @return A boolean with value TRUE if x contains observations that
#'     oscillate
#' @export
#'
#' @examples
#' zigzag(c(1, 0, 1, 0))
zigzag <- function(x) {
    if (any(is.na(x))) {
        stop("Input data should not contain missing values")
    }
    res <- FALSE
    test <- sign(diff(x))
    odds <- test[1:length(test) %% 2 == 0]
    evens <- test[1:length(test) %% 2 == 1]
    if  (all.equal(length(unique(odds)),
                   length(unique(evens)),
                   1) & all(test != 0)) {
        if (!identical(odds, evens)) {
            res <- TRUE
        }
    }
    return(res)
}


#' Tests if a vector shows a monotonically increasing or decreasing pattern
#'
#' @param x A numeric vector
#' @return A boolean with value TRUE if x contains a monotonically increasing or decreasing
#' @export
#'
#' @examples
#' monotone(c(1, 2, 3))
#' monotone(c(3, 2, 1))
monotone <- function(x) {
    if (any(is.na(x))) {
        stop("Input data should not contain missing values")
    }
    res <- FALSE
    test <- sign(diff(x))
    if (all(test > 0) | all(test < 0)) {
        res <- TRUE
    }
    return(res)
}


#' Applies a runner function to chunks of a given size
#'
#' @param x A numeric vector
#' @param runner A function that returns a boolean
#' @param size The chunk size from \code{x} to be passed to \code{runner}
#' @export
#'
#' @examples
#' chunkify(c(1, 0, 1, 0), zigzag, 4)
chunkify <- function(x, runner, size) {
    res <- rep(FALSE, length(x))
    for (i in seq_along(x)) {
        begin <- 1
        end <- min(i + size - 1, length(x))
        test <- x[begin:end]
        if (runner(test) & length(test) >= size) {
            res[end] <- TRUE
        }
    }
    return(res)
}
