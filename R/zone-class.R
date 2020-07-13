#' An S4 class to represent zones for SPC
#'
#' The core idea behind SPC is the definition of zones/regions around
#' the centerline that are used to decide whether a given point is to
#' be considered an anomaly or not. This class allows the user to
#' manually define each of those zones. The user is also expected to
#' pass a centerline which is defined here as an instance of
#' \code{Stripe} in which upper and lower bounds are equal.
#' 
#' @slot centerline A stripe containing the definition centerline
#'     (lower and upper bounds are equal)
#' @slot zoneA A stripe with the definition of zone A
#' @slot zoneB A stripe with the definition of zone B
#' @slot zoneC A stripe with the definition of zone C
#' 
#' @rdname zone
setClass("Zone",
         slots=list(centerline="Stripe",
                    zoneA="Stripe",
                    zoneB="Stripe",
                    zoneC="Stripe"))


#' Validation for the Zone class
setValidity("Zone",
            function(object) {
                errors <- character()
                if (all(object@centerline@upper != object@centerline@lower)) {
                    msg <- "Centerline should be a stripe equal to the centerline"
                    errors <- c(errors, msg)
                }
                if (!all(belongs(object@zoneC, object@zoneB, strict=FALSE))) {
                    msg <- "Zone C is not contained in zone B"
                    errors <- c(errors, msg)
                }
                if (!all(belongs(object@zoneB, object@zoneA, strict=FALSE))) {
                    msg <- "Zone B is not contained in zone C"
                    errors <- c(errors, msg)
                }
                if (length(errors) == 0) TRUE else errors    
            })



#' Getters for the zone class
#' 
#' @param object A zone
#' @return A stripe
#' @rdname setters-getters
NULL


#' The generic to get each zone from a \code{Zone} object
#'
#' A getter to retrieve the zone from a \code{Zone} object
#'
#' @param object An instance of \code{Zone}
#' @param region One of "0" (for the centerline), "A", "B", "C" to
#'     retrieve the stripe correspoding to that zone
#'
#' @export
#' @docType methods
#' @rdname setters-getters
setGeneric("zone", function(object, region,...) standardGeneric("zone"))


#' @rdname setters-getters
setMethod("zone", signature="Zone",
          function(object, region, ...) {
              region <- ifelse(region == "0", "centerline",  paste0("zone", region))
              slot(object, region)
          })


#' Setters for the zone class
#'
#' @param object A zone
#' @param value A stripe 
#' @return A stripe
#' @name setters-getters
NULL


setGeneric("zone<-", function(object, region, ...) standardGeneric("zone<-"))


#' The generic to set the value of each zone in a \code{Zone} object
#'
#' A setter to change the value of the zone in a \code{Zone} object
#'
#' @param object An instance of \code{Zone}
#' @param region One of "0" (for the centerline), "A", "B", "C" to
#'     retrieve the stripe correspoding to that zone
#' @param value An instance of \code{Stripe}
#'
#' @export
#' @docType methods
#' @rdname setters-getters
setReplaceMethod("zone", signature="Zone", function(object, region, value, ...) {
    region <- ifelse(region == "0", "centerline", paste0("zone", region))
    slot(object, region) <- value
    if (validObject(object)) {
        return(object)
    }
})
