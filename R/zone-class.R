#' An S4 class to represent zones for SPC
#'
#' @slot centerline A stripe containing the definition centerline
#'     (lower and upper bounds are equal)
#' @slot zoneA A stripe with the definition of zone A
#' @slot zoneB A stripe with the definition of zone B
#' @slot zoneC A stripe with the definition of zone C
setClass("Zone",
         slots=list(centerline="Stripe",
                    zoneA="Stripe",
                    zoneB="Stripe",
                    zoneC="Stripe"))


#' Validator for the class Zone
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
#' @param object An zone
#' @return A stripe
#' @rdname getters
NULL


#' @rdname getters
setGeneric("zone", function(object, ...) standardGeneric("zone"))
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
#' @name setters
NULL


#' @rdname setters
setGeneric("zone<-", function(object, region, ...) standardGeneric("zone<-"))
setReplaceMethod("zone", signature="Zone", function(object, region, value, ...) {
    region <- ifelse(region == "0", "centerline", paste0("zone", region))
    slot(object, region) <- value
    if (validObject(object)) {
        return(object)
    }
})
