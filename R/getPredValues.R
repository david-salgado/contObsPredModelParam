#' @title Return component \code{PredValues} from the input object
#'
#' @description \code{getPredValues} extracts the predicted values from the slot \code{VarRoles}
#' of the input object.
#'
#' @param object Object of class \linkS4class{contObsPredModelParam}.
#'
#' @return character vector corresponding to the component \code{PredValues} of the input
#' parameter.
#'
#' @examples
#'
#' @export
setGeneric("getPredValues", function(object){standardGeneric("getPredValues")})

#' @rdname getPredValues
#'
#' @export
setMethod(
  f = "getPredValues",
  signature = c("contObsPredModelParam"),
  function(object){object@VarRoles$PredValues}
)
