#' @title Return slot \code{EdData} from the input object
#'
#' @description \code{getEdData} extracts the slot \code{EdData} of the input object.
#'
#' @param object Object of class \linkS4class{ObsErrorSTDParam} or \linkS4class{ErrorProbParam}.
#'
#' @return Object of class \linkS4class{StQList} corresponding to the slot \code{EdData} of the input
#' parameter.
#'
#' @examples
#'
#' @export
setGeneric("getEdData", function(object){standardGeneric("getEdData")})

#' @rdname getEdData
#'
#' @export
setMethod(
  f = "getEdData",
  signature = c("ObsErrorSTDParam"),
  function(object){object@EdData}
)
#'
#' @rdname getEdData
#'
#' @export
setMethod(
  f = "getEdData",
  signature = c("ErrorProbParam"),
  function(object){object@EdData}
)
