#' \code{ComputeErrorProb} computes the probability of measurement error for each statistical unit
#'
#'
#' @param object Object of class \linkS4class{contObsPredModelParam} containing the statistical
#' units whose probability of measurement error for each variable is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{ErrorProbParam} with the parameters determining
#' the method of computation of the probability of measurement error of each statistical unit.
#'
#' @return Object of class \linkS4class{contObsPredModelParam} with the measurement error
#' probabilities computed for each variable and each statistical unit.
#'
#'
#' @examples
#' \dontrun{}
setGeneric("ComputeErrorProb", function(object, Param) {standardGeneric("ComputeErrorProb")})

#' @rdname ComputeErrorProb
#'
#' @include conObsPredModelParam-class.R ErrorProbParam-class.R
#'
#' @export
setMethod(f = "ComputeErrorProb",
          signature = c("contObsPredModelParam", "ErrorProbParam"),
          function(object, Param){


          }
)
