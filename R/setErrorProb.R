#' @title Set value of component \code{ErrorProb} of an object \linkS4class{contObsPredModelParam}
#'
#' @description \code{setErrorProb} assigns a character value to the component \code{ErrorProb} of
#' the slot \code{VarRoles} of the input object \linkS4class{contObsPredModelParam}.
#'
#' @param object Object \linkS4class{contObsPredModelParam} whose component \code{ErrorProb}
#' is to be assigned.
#'
#' @param value character vector to be assigned to the component \code{ErrorProb}.
#'
#' @return Object \linkS4class{contObsPredModelParam} with the component \code{ErrorProb} updated.
#'
#' @export
setGeneric("setErrorProb<-", function(object, value){standardGeneric("setErrorProb<-")})
#'
#' @rdname setErrorProb
#'
#' @export
setReplaceMethod(
  f = "setErrorProb",
  signature = c("contObsPredModelParam","character"),
  function(object, value){
    object@VarRoles$ErrorProb <- value
    return(object)
  }
)
