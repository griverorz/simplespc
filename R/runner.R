#' Basic runner
#'
#' @param x A numeric vector
#' @param howmany An integer
#' @param outof An integer
#' @param interval A stripe
#' @return A vector of booleans
#' @export
#' 
#' @examples
#' runner(c(1, 2, 3, 4), 2, 3, rep(stripe(2, 2), 3))
runner <- function(x, howmany, outof, interval) {
    res <- rep(FALSE, length(x))
    for (i in seq_along(x)) {
        begin <- i
        end <- min(i + (outof - 1), length(x))
        test <- x[begin:end]
        ucl <- interval@upper[begin:end]
        lcl <- interval@lower[begin:end]        
        if (length(test) == outof) {
            if (sum(test > ucl) >= howmany & test[1] > ucl[1]) {
                res[end] <- TRUE
            }
            if (sum(test < lcl) >= howmany & test[1] < lcl[1]) {
                res[end] <- TRUE
            }
        }
    }
    return(res)    
}


#' Main entry point
#'
#' @param data A numeric vector or a matrix
#' @param chart A string with the name of a chart
#' @return A data frame of booleans
#' @export
simplespc <- function(data, chart) {
    test <- build_zones(new(chart, data=data))
    res <- data.frame("Rule1"=rule1(test),
                      "Rule2"=rule2(test),
                      "Rule3"=rule3(test),
                      "Rule4"=rule4(test))
    return(res)
}
