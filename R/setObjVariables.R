#' @title Set value of component \code{ObjVariables} of an object \linkS4class{contObsPredModelParam}
#'
#' @description \code{setObjVariables} assigns a character value to the component \code{ObjVariables}
#' of the slot \code{VarRoles} of the input object \linkS4class{contObsPredModelParam}.
#'
#' @param object Object \linkS4class{contObsPredModelParam} whose component \code{ObjVariables}
#' is to be assigned.
#'
#' @param value character vector to be assigned to the component \code{ObjVariables}.
#'
#' @return Object \linkS4class{contObsPredModelParam} with the component \code{ObjVariables} updated.
#'
#'@export
setGeneric("setObjVariables<-", function(object, value){standardGeneric("setObjVariables<-")})

#' @rdname setObjVariables
#'
#' @export
setReplaceMethod(
  f = "setObjVariables",
  signature = c("contObsPredModelParam","character"),
  function(object, value){
    object@VarRoles[['ObjVariables']] <-  value
    return(object)
   }
)

