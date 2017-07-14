#' @title Return slot \code{PredictionParam} from the input object
#'
#' @description \code{getPredictionParam} extracts the the parameters to make predictions
#' \code{PredictionParam} of the input object.
#'
#' @param object Object of class \linkS4class{PredValueTSParam}.
#'
#' @return  slot of class \linkS4class{PredTSParam} corresponding to the slot \code{PrectionParam}
#' of the input parameter.
#'
#' @examples
#'
#' @export
setGeneric("getPredictionParam", function(object){standardGeneric("getPredictionParam")})

#' @rdname getPredictionParam
#'
#' @export
setMethod(
  f = "getPredictionParam",
  signature = c("PredValueTSParam"),
  function(object){object@PredictionParam}
)
