#' @title Return component \code{ErroProb} from the input object
#'
#' @description \code{getErroProb} extracts the component \code{ErrorProb} of the slot
#' \code{VarRoles} of the input object.
#'
#' @param object Object of class \linkS4class{ErrorProbParam}.
#'
#' @return character vector corresponding to the component \code{ErrorProb} of the input
#' parameter.
#'
#' @examples
#'
#' @export
setGeneric("getErrorProb", function(object){standardGeneric("getErrorProb")})

#' @rdname getErrorProb
#'
#' @export
setMethod(
  f = "getErrorProb",
  signature = c("contObsPredModelParam"),
  function(object){object@VarRoles$ErrorProb}
)
