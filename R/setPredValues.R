#' @title Set value of component \code{PredValues} of an object \linkS4class{contObsPredModelParam}
#'
#' @description \code{setPredValues} assigns a character value to the component \code{PredValues} of
#' the slot \code{VarRoles} of the input object \linkS4class{contObsPredModelParam}.
#'
#' @param object Object \linkS4class{contObsPredModelParam} whose component \code{PredValues}
#' is to be assigned.
#'
#' @param value character vector to be assigned to the component \code{PredValues}.
#'
#' @return Object \linkS4class{contObsPredModelParam} with the component \code{PredValues} updated.
#'
#' @export
setGeneric("setPredValues<-", function(object, value){standardGeneric("setPredValues<-")})
#'
#' @rdname setPredValues
#'
#' @export
setReplaceMethod(
  f = "setPredValues",
  signature = c("contObsPredModelParam","character"),
  function(object, value){
    object@VarRoles$PredValues <- value
    return(object)
  }
)
