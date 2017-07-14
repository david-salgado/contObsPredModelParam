#' @title Return slot \code{RawData} from the input object
#'
#' @description \code{getRawData} extracts the slot \code{RawData} of the input object.
#'
#' @param object Object of class \linkS4class{ObsErrorSTDParam} or \linkS4class{ErrorProbParam}.
#'
#' @return Object of class \linkS4class{StQList} corresponding to the slot \code{RawData} of the input
#' parameter.
#'
#' @examples
#'
#' @export
setGeneric("getRawData", function(object){standardGeneric("getRawData")})

#' @rdname getRawData
#'
#' @export
setMethod(
  f = "getRawData",
  signature = c("ObsErrorSTDParam"),
  function(object){object@RawData}
)
#'
#' @rdname getRawData
#'
#' @export
setMethod(
  f = "getRawData",
  signature = c("ErrorProbParam"),
  function(object){object@RawData}
)


