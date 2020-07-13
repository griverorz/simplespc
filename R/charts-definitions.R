#' Build zones
#' @export
setGeneric("build_zones", function(object, ...) standardGeneric("build_zones"))


#' @rdname build_zones
#' @export
setMethod("build_zones",
          signature="xbarchart",
          function(object) {
              n <- length(object@data)
              mu <- mean(object@data)
              sigma <- sd(object@data)
              
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
                  stats=mu,
                  sd=sigma,
                  zones=zones)
          })


#' @rdname build_zones
#' @export
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
                  stats=mu,
                  sd=sigma,
                  zones=zones)
          })


#' @rdname build_zones
#' @export
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
                  stats=mu,
                  sd=sigma,
                  zones=zones)
          })
