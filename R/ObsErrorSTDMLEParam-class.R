#' @title S4 class for the parameters of the ML estimation of the obervation error variance
#'
#' @description Definition of the S4 class named \code{ObsErrorSTDMLEParam} for the parameters of
#' the ML estimation of the obervation error variance.
#'
#'
#' @slot RawData \linkS4class{StQList} object with the raw data.
#'
#' @slot EdData \linkS4class{StQList} object with the edited data.
#'
#' @slot VarNames Character vector with the names of the variables whose probability of measurement
#' error are to be computed.
#'
#' @slot Imputation \linkS4class{ImputationParam} object with the parameters to imputed missing
#' values during the computation of the measurement error probabilities.
#'
#' @examples
#' # An empty ObsErrorSTDMLEParam object:
#' new(Class = 'ObsErrorSTDMLEParam')
#'
#' \dontrun{
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = 'CifraNeg_13.___',
#'                 DomainNames =  'Tame_05._4.')
#' ObsErrorSTDMLEParam <- new(Class = 'ObsErrorSTDMLEParam',
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
setClass(Class = "ObsErrorSTDMLEParam",
         slots = c(RawData = 'StQList',
                   EdData = 'StQList',
                   VarNames = 'character',
                   Imputation = 'ImputationParam'),
         prototype = list(RawData = StQ::StQList(),
                          EdData = StQ::StQList(),
                          VarNames = character(0),
                          Imputation = new(Class = 'MeanImputationParam')),
         validity = function(object){

           if (!all(object@VarNames == object@Imputation@VarNames)) {

             stop('[StQImputation:: validity ErrorProbMLEParam] The slots VarNames in the object and in the slot Imputation must be the same.\n')

           }

           return(TRUE)
         }
)
