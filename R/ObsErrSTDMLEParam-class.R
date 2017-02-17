#' @title S4 class for the parameters of the ML estimation of the obervation error variance
#'
#' @description Definition of the S4 class named \code{ObsErrSTDMLEParam} for the parameters of
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
#' # An empty ObsErrSTDMLEParam object:
#' new(Class = 'ObsErrSTDMLEParam')
#'
#' \dontrun{
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = 'CifraNeg_13.___',
#'                 DomainNames =  'Tame_05._4.')
#' ObsErrSTDMLEParam <- new(Class = 'ObsErrSTDMLEParam',
#'                          RawData = FD.StQList,
#'                          EdData = FG.StQList,
#'                          VarNames = 'CifraNeg_13.___',
#'                          Imputation = ImpParam)
#'
#'
#' }
#'
#' @import data.table StQ RepoTime StQImputation
#'
#' @export
setClass(Class = "ObsErrSTDMLEParam",
         slots = c(RawData = 'StQList',
                   EdData = 'StQList',
                   VarNames = 'character',
                   Imputation = 'ImputationParam'),
         prototype = list(RawData = new(Class = 'StQList'),
                          EdData = new(Class = 'StQList'),
                          VarNames = character(0),
                          Imputation = new(Class = 'MeanImputationParam')),
         validity = function(object){

           if (!all(object@VarNames == object@Imputation@VarNames)) {

             stop('[StQImputation:: validity ErrorProbMLEParam] The slots VarNames in the object and in the slot Imputation must be the same.\n')

           }

           return(TRUE)
         }
)
