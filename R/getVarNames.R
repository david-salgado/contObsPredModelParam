#' @title Return slot \code{VarNames} from the input object
#'
#' @description \code{getVarNames} extracts the slot \code{VarNames} of the input object.
#'
#' In the case of objects of classes \linkS4class{ObsErrorSTDParam} and \linkS4class{ErrorProbParam},
#' it returns the slot \code{VarNames}.
#'
#' In the case of objects of class \linkS4class{PredValueTSParam}, it returns the \code{VarNames}
#' of the slot \code{ImputationParam} which contains the names of the variables to be imputed.
#'
#' @param object Object of class \linkS4class{ObsErrorSTDParam}, \linkS4class{ErrorProbParam} or
#' \linkS4class{PredValueTSParam} .
#'
#' @return Character vector corresponding to the slot \code{VarNames} of the input parameter.
#'
#' @examples
#'
#' @export
setGeneric("getVarNames", function(object){standardGeneric("getVarNames")})

#' @rdname getVarNames
#'
#' @export
setMethod(
  f = "getVarNames",
  signature = c("ObsErrorSTDParam"),
  function(object){object@VarNames}
)
#'
#' @rdname getVarNames
#'
#' @export
setMethod(
  f = "getVarNames",
  signature = c("ErrorProbParam"),
  function(object){object@VarNames}
)
#'
#' @rdname getVarNames
#'
#' @export
setMethod(
  f = "getVarNames",
  signature = c("PredValueTSParam"),
  function(object){object@ImputationParam@VarNames}
)
