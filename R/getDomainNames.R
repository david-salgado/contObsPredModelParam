#' @title Return slot \code{DomainNames} from the input object.
#'
#' @description \code{getDomainNames} extracts the slot \code{DomainNames}, the names of the
#' variables determining the domains over which to compute the imputation method of missing values,
#' of the input object.
#'
#' In the case of objects of classes \linkS4class{ObsErrorSTDParam} and \linkS4class{ErrorProbParam},
#' it returns the \code{DomainNames} of the slot \code{Imputation}.
#'
#' In the case of objects of class \linkS4class{PredValueTSParam}, it returns the \code{DomainNames}
#' of the slot \code{ImputationParam}.
#'
#' @param object Object of class \linkS4class{ObsErrorSTDParam}, \linkS4class{ErrorProbParam} or
#' \linkS4class{PredValueTSParam}.
#'
#' @return character vector corresponding to the slot \code{DomainNames} of the input parameter.
#'
#' @examples
#'
#' @export
setGeneric("getDomainNames", function(object){standardGeneric("getDomainNames")})

#' @rdname getDomainNames
#'
#'
#' @export
setMethod(
  f = "getDomainNames",
  signature = c("ObsErrorSTDParam"),
  function(object){object@Imputation@DomainNames}
  )
#'
#' @rdname getDomainNames
#'
#'
#' @export
setMethod(
  f = "getDomainNames",
  signature = c("ErrorProbParam"),
  function(object){object@Imputation@DomainNames}
)
#'
#' @rdname getDomainNames
#'
#'
#' @export
setMethod(
  f = "getDomainNames",
  signature = c("PredValueTSParam"),
  function(object){object@ImputationParam@DomainNames}
)
