#' @title Return component \code{getPredErrorSTD} from the input object
#'
#' @description \code{getPredErrorSTD} extracts the standard deviation of the predicted values
#' from the slot \code{VarRoles} of the input object.
#'
#' @param object Object of class \linkS4class{contObsPredModelParam}.
#'
#' @return character vector corresponding to the component \code{getPredErrorSTD} of the input
#' parameter.
#'
#' @examples
#'
#' @export
setGeneric("getPredErrorSTD", function(object){standardGeneric("getPredErrorSTD")})

#' @rdname getPredErrorSTD
#'
#' @export
setMethod(
  f = "getPredErrorSTD",
  signature = c("contObsPredModelParam"),
  function(object){object@VarRoles$PredErrorSTD}
)
