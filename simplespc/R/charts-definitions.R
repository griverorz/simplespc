#' The generic for \code{build_zones}
#'
#' @docType methods
#' @rdname build_zones-methods
setGeneric("build_zones", function(object, ...) standardGeneric("build_zones"))


#' Build zones for the x-bar chart
#' 
#' This function allows for the creation of different methods to
#' generate the zones needed for each type of chart. Each chart is
#' expected to provide a \code{build_zones} method that takes the
#' \code{data} slot (see \code{spcchart}) and produces an instance of
#' \code{SpcData}.
#'
#' @param object An instance of any chart inherited from \code{SpcChart}.
#' @param ... Additional arguments for a chart.
#' @return An instance of \code{SpcData}
#' 
#' @export
#' @rdname build_zones-methods
#' @aliases build_zones,ANY,ANY-method
setMethod("build_zones",
          signature="xbarchart",
          function(object) {
              n <- length(object@data)
              mu <- mean(object@data)
              sigma <- stats::sd(object@data)
              
              zones <- new("Zone",
                           zoneA=rep(stripe(mu - 3 * sigma,
                                            mu + 3 * sigma), n),
                           zoneB=rep(stripe(mu - 2 * sigma,
                                            mu + 2 * sigma), n),
                           zoneC=rep(stripe(mu - 1 * sigma,
                                            mu + 1 * sigma), n),
                           centerline=rep(stripe(mu, mu), n))
              
              new("SpcData",
                  data=object@data,
                  yhat=object@data,
                  statistics=mu,
                  deviation=sigma,
                  zones=zones)
          })


#' Build zones for the c chart
#'
#' @param object An instance of any chart inherited from \code{SpcChart}.
#' @param ... Additional arguments for a chart.
#' @return An instance of \code{SpcData}

#' @rdname build_zones-methods
#' @aliases build_zones,ANY,ANY-method
setMethod("build_zones",
          signature="cchart",
          function(object) {
              n <- length(object@data)
              mu <- mean(object@data)
              sigma <- sqrt(mean(object@data))
              
              zones <- new("Zone",
                           zoneA=rep(stripe(mu - 3 * sigma,
                                            mu + 3 * sigma), n),
                           zoneB=rep(stripe(mu - 2 * sigma,
                                            mu + 2 * sigma), n),
                           zoneC=rep(stripe(mu - 1 * sigma,
                                            mu + 1 * sigma), n),
                           centerline=rep(stripe(mu, mu), n))
              
              new("SpcData",
                  data=object@data,
                  yhat=object@data,
                  statistics=mu,
                  deviation=sigma,
                  zones=zones)
          })


#' Build zones for the p chart
#'
#' @param object An instance of any chart inherited from \code{SpcChart}.
#' @param ... Additional arguments for a chart.
#' @return An instance of \code{SpcData}
#' 
#' @rdname build_zones-methods
#' @aliases build_zones,ANY,ANY-method
setMethod("build_zones",
          signature="pchart",
          function(object) {
              n <- nrow(object@data)
              mu <- object@data[, 1]/object@data[, 2]
              pbar <- sum(object@data[, 1])/sum(object@data[, 2])
              sigma <- pbar * (1 - pbar)
              sizes <- object@data[, 2]
              
              zones <- new("Zone",
                           "zoneA"=stripe(mu - 3 * sqrt(sigma * sizes),
                                          mu + 3 * sqrt(sigma * sizes)),
                           "zoneB"=stripe(mu - 2 * sqrt(sigma * sizes),
                                          mu + 2 * sqrt(sigma * sizes)),
                           "zoneC"=stripe(mu - 1 * sqrt(sigma * sizes),
                                          mu + 1 * sqrt(sigma * sizes)),
                           "centerline"=rep(stripe(pbar, pbar), n))
              
              new("SpcData",
                  data=object@data,
                  yhat=mu,
                  statistics=mu,
                  deviation=sigma,
                  zones=zones)
          })
