#' @title S4 class for the parameters of the ML estimation of the probability of measurement error
#'
#' @description Definition of the S4 class named \code{ErrorProbMLEParam} for the parameters of
#' a continuous observation-prediction model in the optimization approach to selective editing.
#'
#'
#' @slot Data \linkS4class{data.table} with the parameters or all data necessary to compute them.
#'
#' @slot VarRoles List with components \code{Units}, \code}Domains}, \code{DesignW},
#'       \code{PredValues}, \code{PredSTD}, \code{ObsSTD}, \code{ErrorProb} being character vectors
#'       containing the column names according to their respective role in the model.
#'
#' @examples
#' # An empty contObsPredModelParam object:
#' new(Class = 'contObsPredModelParam')
#'
#' @import data.table StQ RepoTime ImputationParam
#'
#' @export
setClass(Class = "ErrorProbMLEParam",
         slots = c(RawData = 'StQList',
                   EdData = 'StQList',
                   VarNames = 'character',
                   Imputation = 'ImputationParam'),
         prototype = list(RawData = new(Class = 'StQList'),
                          EdData = new(Class = 'StQList'),
                          VarNames = character(0),
                          ImputationParam = 'MeanImputationParam'),
         validity = function(object){



           return(TRUE)
         }
)
