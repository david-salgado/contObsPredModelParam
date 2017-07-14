#' @title Return the component \code{ObjVariables} from the input object
#'
#' @description \code{getObjVariables} extracts the component \code{ObjVariables} of the slot
#' \code{VarRoles} of the input object.
#'
#' @param object Object of class \linkS4class{contObsPredModelParam}.
#'
#' @return character vector corresponding to the component \code{ObjVariables} of the input
#' parameter.
#'
#' @examples
#'
#' @export
setGeneric("getObjVariables", function(object){standardGeneric("getObjVariables")})

#' @rdname getObjVariables
#'
#' @export
setMethod(
  f = "getObjVariables",
  signature = c("contObsPredModelParam"),
  function(object){object@VarRoles$ObjVariables}
)
