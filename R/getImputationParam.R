#' @title Return slot \code{ImputationParam} from the input object
#'
#' @description \code{getImputationParam} extracts the slot \code{ImputationParam} of the input
#' object with the parameters for the imputation method of missing values.
#'
#' @param object Object of class \linkS4class{PredValueTSParam}.
#'
#' @return Object of class \linkS4class{ImputationParam} corresponding to the slot
#' \code{ImputationParam} of the input parameter.
#'
#' @examples
#'
#' @export
setGeneric("getImputationParam", function(object){standardGeneric("getImputationParam")})

#' @rdname getImputationParam
#'
#' @export
setMethod(
  f = "getImputationParam",
  signature = c("PredValueTSParam"),
  function(object){object@ImputationParam}
)
