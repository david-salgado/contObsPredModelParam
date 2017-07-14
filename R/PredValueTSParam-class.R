#' @title S4 class for the parameters of time series prediction in an observation-prediction model.
#'
#' @description Definition of the S4 class named \code{PredValueTSParam} for the parameters of
#' a continuous observation-prediction model in the optimization approach to selective editing.
#'
#'
#' @slot RawData \linkS4class{StQList} object with the raw data.
#'
#' @slot EdData \linkS4class{StQList} object with the edited data.
#'
#' @slot VarNames Character vector with the names of the variables whose probability errors are to
#' be computed.
#'
#' @slot Imputation \linkS4class{ImputationParam} object with the parameters to imputed missing
#' values during the computation of the error probabilities.
#'
#' @examples
#' # An empty contObsPredModelParam object:
#' new(Class = 'ErrorProbMLEParam')
#'
#' \dontrun{
#' ImpParam <- new(Class = 'PredValueTSParam',
#'                 PredParam = ,
#'                 ImputationParam =  )
#' ErrorProbMLEParam <- new(Class = 'ErrorProbMLEParam',
#'                          RawData = FD.StQList,
#'                          EdData = FF.StQList,
#'                          VarNames = 'CifraNeg_13.___',
#'                          Imputation = ImpParam)
#'
#'
#' }
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
