#' @title Set value of component \code{ObsErrSTD} of an object \linkS4class{contObsPredModelParam}
#'
#' @description \code{setObsErrorSTD} assigns a character value to the component \code{ObsErrSTD}
#' of the slot \code{VarRoles} of the input object \linkS4class{contObsPredModelParam}.
#'
#' @param object Object \linkS4class{contObsPredModelParam} whose component \code{ObsErrSTD} is to
#' be assigned.
#'
#' @param value character vector to be assigned to the component \code{ObsErrSTD}.
#'
#' @return Object \linkS4class{contObsPredModelParam} with the component \code{ObsErrSTD} updated.
#'
#' @export
setGeneric("setObsErrorSTD<-", function(object, value){standardGeneric("setObsErrorSTD<-")})
#'
#' @rdname setObsErrorSTD
#'
#' @export
setReplaceMethod(
  f = "setObsErrorSTD",
  signature = c("contObsPredModelParam","character"),
  function(object, value){
    object@VarRoles$ObsErrSTD <- value
    return(object)
  }
)

