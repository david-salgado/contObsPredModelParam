#' @title Set value of slot \code{Data} of an object \linkS4class{contObsPredModelParam}
#'
#' @description \code{setData} assigns a \linkS4class{StQ} to the slot \code{Data} of the input
#' object \linkS4class{contObsPredModelParam}
#'
#' @param object Object whose slot \code{Data} is to be assigned.
#'
#' @param value \linkS4class{StQ} to be assigned to the slot \code{Data}.
#'
#' @return Object \linkS4class{contObsPredModelParam} with slot \code{Data} updated.
#'
#' @import StQ
#'
#' @rdname setData
#'
#' @export
setReplaceMethod(
  f = "setData",
  signature = c("contObsPredModelParam","StQ"),
  function(object, value){
    object@Data <- value
    return(object)
  }
)
