#' @include stripe-class.R
NULL


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
#' @rdname zone
setClass("Zone",
         slots=list(centerline="Stripe",
                    zoneA="Stripe",
                    zoneB="Stripe",
                    zoneC="Stripe"))


# Validation for the Zone class
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
#' @param region One of "0" (for the centerline), "A", "B", "C" to
#'     retrieve the stripe correspoding to that zone
#'
#' @return A stripe
#' @rdname getters
NULL


#' The generic to get each zone from a \code{Zone} object
#'
#' @rdname getters
setGeneric("zone", function(object, region, ...) standardGeneric("zone"))


#' Zone constructor
#'
#' A constructor for the zone class
#'
#' @param object An instance of \code{Zone}
#' @param region One of "0" (for the centerline), "A", "B", "C" to
#'     retrieve the stripe correspoding to that zone
#' @param ... Additional arguments. Not implemented.
#' @return A zone
#' 
#' @export
#' @rdname getters
setMethod("zone", signature="Zone",
          function(object, region, ...) {
              region <- ifelse(region == "0", "centerline",  paste0("zone", region))
              slot(object, region)
          })


#' Setters for the zone class
#'
#' @name setters
NULL

#' Setter for the zone
#'
#' @rdname setters
setGeneric("zone<-", function(object, value) standardGeneric("zone<-"))


#' Set the value of a zone in a \code{Zone} object
#'
#' A setter to change the value of the zone in a \code{Zone} object
#'
#' @param object An instance of \code{Zone}
#' @param value An instance of \code{Zone}
#'
#' @export
#' @rdname setters
setReplaceMethod("zone", signature="Zone", function(object, value) {
    object <- value
    if (validObject(object)) {
        return(object)
    }
})

#' Edits slot in a zone
#'
#' @rdname zone-setters
setGeneric("set_zone", function(object, region, value) standardGeneric("set_zone"))


#' Sets a specific zone in a \code{Zone} object
#'
#' Updates the value of a given zone in a Zone object
#' 
#' @param object A Zone object
#' @param region One of "0" (for the centerline), "A", "B", "C" to
#'     retrieve the stripe correspoding to that zone
#' @param value A \code{Stripe} with the new value for the region
#' 
#' @export
#' @rdname zone-setters
setMethod("set_zone", signature="Zone", function(object, region, value) {
    region <- ifelse(region == "0", "centerline", paste0("zone", region))
    slot(object, region) <- value
    
    if (validObject(object)) {
        return(object)
    }
})
