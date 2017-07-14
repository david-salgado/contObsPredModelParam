#' @title Return slot \code{Data} from the input object
#'
#' @description \code{getData} extracts the slot \code{Data} which contains the parameters of a
#' continuous observation-prediction model or all data necessary to compute them of the input object.
#'
#' @param object Object of class \linkS4class{contObsPredModelParam}.
#'
#' @return Object of class \linkS4class{StQ} corresponding to the slot \code{Data} of the input
#' parameter.
#'
#' @examples
#'
#' @import StQ
#'
#' @rdname getData
#'
#' @export
setMethod(
  f = "getData",
  signature = c("contObsPredModelParam"),
  function(object){object@Data}
)
