#' @title Set value of component \code{ObsErrorSTD} of an object \linkS4class{contObsPredModelParam}
#'
#' @description \code{setObsErrorSTD} assigns a character value to the component \code{ObsErrorSTD}
#' of the slot \code{VarRoles} of the input object \linkS4class{contObsPredModelParam}.
#'
#' @param object Object \linkS4class{contObsPredModelParam} whose component \code{ObsErrorSTD}
#' is to be assigned.
#'
#' @param value character vector to be assigned to the component \code{ObsErrorSTD}.
#'
#' @return Object \linkS4class{contObsPredModelParam} with the component \code{ObsErrorSTD} updated.
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
    object@VarRoles$obsErrorSTD <- value
    return(object)
  }
)

