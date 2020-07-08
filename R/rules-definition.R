#' Build zones
setGeneric("build_zones", function(object, ...) standardGeneric("build_zones"))


#' @rdname build_zones
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


setGeneric("rule1", function(object, ...) standardGeneric("rule1"))


setMethod("rule1",
          signature("SpcData"),
          function(object) {
              runner(object@yhat, 1, 1, zone(object@zones, "A"))
          })


setGeneric("rule2", function(object, ...) standardGeneric("rule2"))


setMethod("rule2",
          signature("SpcData"),
          function(object) {
              runner(object@yhat, 2, 3, zone(object@zones, "B"))
          })


setGeneric("rule3", function(object, ...) standardGeneric("rule3"))


setMethod("rule3",
          signature("SpcData"),
          function(object) {
              runner(object@yhat, 4, 5, zone(object@zones, "C"))
          })


setGeneric("rule4", function(object, ...) standardGeneric("rule4"))


setMethod("rule4",
          signature("SpcData"),
          function(object) {
              runner(object@yhat, 8, 8, zone(object@zones, "0"))
          })

