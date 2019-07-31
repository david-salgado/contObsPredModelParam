#' @title Set value of component \code{PredErrSTD} of an object \linkS4class{contObsPredModelParam}
#'
#' @description \code{setPredErrorSTD} assigns a character value to the component \code{PredErrSTD}
#' of the slot \code{VarRoles} of the input object \linkS4class{contObsPredModelParam}.
#'
#' @param object Object \linkS4class{contObsPredModelParam} whose component \code{PredErrSTD} is to
#' be assigned.
#'
#' @param value character vector to be assigned to the component \code{PredErrorSTD}.
#'
#' @return Object \linkS4class{contObsPredModelParam} with the component \code{PredErrSTD} updated.
#'
#'@export
setGeneric("setPredErrorSTD<-", function(object, value){standardGeneric("setPredErrorSTD<-")})
#'
#' @rdname setPredErrorSTD
#'
#' @export
setReplaceMethod(
  f = "setPredErrorSTD",
  signature = c("contObsPredModelParam","character"),
  function(object, value){
    object@VarRoles$PredErrSTD <- value
    return(object)
  }
)
