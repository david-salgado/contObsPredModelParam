#' @title Return component \code{ObsErrorSTD} from the input object
#'
#' @description \code{getObsErrorSTD} extracts the component \code{ObsErrorSTD} of the slot
#' \code{VarRoles} of the input object.
#'
#' @param object Object of class \linkS4class{contObsPredModelParam}.
#'
#' @return character vector corresponding to the component \code{ObsErrorSTD} of the input
#' parameter.
#'
#' @examples
#'
#' @export
setGeneric("getObsErrorSTD", function(object){standardGeneric("getObsErrorSTD")})

#' @rdname getObsErrorSTD
#'
#' @export
setMethod(
  f = "getObsErrorSTD",
  signature = c("contObsPredModelParam"),
  function(object){object@VarRoles$ObsErrorSTD}
)
