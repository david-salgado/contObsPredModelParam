#' @title Return slot \code{VarRoles} from the input object
#'
#' @description \code{getVarRoles} extracts the slot \code{VarRoles} of the input object.
#'
#' @param object Object of class \linkS4class{contObsPredModelParam}.
#'
#' @return List with components \code{Units}, \code{Domains}, \code{DesignW},\code{ObjVariables},
#' \code{PredValues}, \code{PredErrorSTD}, \code{ObsErrorSTD},\code{ErrorProb}
#'
#' @examples
#'
#' @export
setGeneric("getVarRoles", function(object){standardGeneric("getVarRoles")})

#' @rdname getVarRoles
#'
#' @export
setMethod(
  f = "getVarRoles",
  signature = c("contObsPredModelParam"),
  function(object){object@VarRoles}
)
