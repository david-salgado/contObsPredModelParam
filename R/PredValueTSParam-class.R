#' @title S4 class for the parameters to compute the predicted values and the prediction error std
#' for each variable specified in the parameter for prediction
#'
#' @description Definition of the S4 class named \code{PredValueTSParam} for the parameters to
#' compute the predicted values and the prediction error std.
#'
#'
#' @slot PredictionParam \linkS4class{PredTSParam} object with the parameters to compute the
#' predicted values and their standard deviations.
#'
#' @slot ImputationParam \linkS4class{ImputationParam} object with the parameters to impute missing
#' values during the computation of the predicted values and their standard deviations.
#'
#' @examples
#' # An empty contObsPredModelParam object:
#' new(Class = 'PredValueTSParam')
#'
#' @import data.table StQ RepoTime BestTSPred StQImputation
#'
#' @export
setClass(Class = "PredValueTSParam",
         slots = c(PredictionParam = 'PredTSParam',
                   ImputationParam = 'ImputationParam'),
         prototype = list(PredictionParam = new(Class = 'PredTSParam'),
                          ImputationParam = new(Class = 'MeanImputationParam')),
         validity = function(object){

           return(TRUE)
         }
)
