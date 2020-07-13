setClass(Class="Rule",
         slots=list(data="SpcData"),
         contains="VIRTUAL")

setClass("Ruleset",
         slots=list(rules="Rule"))

setClass("WesternElectric",
         contains="Ruleset")

setGeneric("rule", function(object, rule,...) standardGeneric("rule"))


#' @export
setMethod("rule",
          signature="WesternElectric",
          function(object, rule) {
              switch(rule,
                     "1"=runner(object@yhat, 1, 1, zone(object@zones, "A")),
                     "2"=runner(object@yhat, 2, 3, zone(object@zones, "B")),
                     "3"=runner(object@yhat, 4, 5, zone(object@zones, "C")),
                     "4"=runner(object@yhat, 8, 8, zone(object@zones, "0")))
          })
