#' @title Return slot \code{Imputation} from the input object
#'
#' @description \code{getImputation} extracts the slot \code{Imputation} of the input object, which
#' contains the parameters to impute missing values.
#'
#' @param object Object of class \linkS4class{ObsErrorSTDParam} or \linkS4class{ErrorProbParam}.
#'
#' @return Object of class \linkS4class{ImputationParam} corresponding to the slot \code{Imputation}
#' of the input parameter.
#'
#' @examples
#'
#' @export
setGeneric("getImputation", function(object){standardGeneric("getImputation")})

#' @rdname getImputation
#'
#' @export
setMethod(
  f = "getImputation",
  signature = c("ObsErrorSTDParam"),
  function(object){object@Imputation}
)
#'
#' @rdname getImputation
#'
#' @export
setMethod(
  f = "getImputation",
  signature = c("ErrorProbParam"),
  function(object){object@Imputation}
)

