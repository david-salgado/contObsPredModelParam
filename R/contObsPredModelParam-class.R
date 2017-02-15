#' @title S4 class for the parameters of a continuous observation-prediction model
#'
#' @description Definition of the S4 class named \code{contObsPredModelParam} for the parameters of
#' a continuous observation-prediction model in the optimization approach to selective editing.
#'
#'
#' @slot Data \linkS4class{data.table} with the parameters or all data necessary to compute them.
#'
#' @slot VarRoles List with components \code{Units}, \code{Domains}, \code{DesignW},
#'       \code{PredValues}, \code{PredSTD}, \code{ObsSTD}, \code{ErrorProb} being character vectors
#'       containing the column names according to their respective role in the model.
#'
#'
#' @examples
#' # An empty contObsPredModelParam object:
#' new(Class = 'contObsPredModelParam')
#'
#' \dontrun{
#'
#' ObsPredPar <- new(Class = 'contObsPredModelParam',
#'                   Data = Data,
#'                   VarRoles = list(Units = 'NOrden', Domains = 'Tame_05._2.')
#'                    )
#'
#' }
#'
#' @import data.table StQ
#'
#' @export
setClass(Class = "contObsPredModelParam",
         slots = c(Data = 'StQ',
                   VarRoles = 'list'),
         prototype = list(Data = new(Class = 'StQ'),
                          VarRoles = list(Units = character(0),
                                          Domains = character(0),
                                          DesignW = character(0),
                                          PredValues = character(0),
                                          PredSTD = character(0),
                                          ObsSTD= character(0),
                                          ErrorProb = character(0))),
         validity = function(object){

           VarRoles <- slot(object, 'VarRoles')
           if (!all(names(VarRoles) %in% c('Units', 'Domains', 'DesignW',
                                           'PredValues', 'PredSTD', 'ObsSTD', 'ErrorProb'))){

             stop('[contObsPredModelParam: validity] All components of VarRoles must be one of these: Units, Domains, DesignW, PredValues, PredSTD, ObsSTD, ErrorProb.')

           }
           Variables <- unlist(VarRoles)

           slotData <- slot(object, 'Data')
           Data <- dcast_StQ(slotData, setdiff(ExtractNames(Variables), getIDQual(slotData)))
           DataColNames <- names(Data)

           VarNotinData <- setdiff(Variables, DataColNames)
           if (length(VarNotinData) != 0) {

             stop(paste0('[contObsPredModelParam: validity] The following variables in VarRoles are not in the slot Data:\n',
                          paste0(VarNotinData, collapse = ', '), '.\n'))
           }

           return(TRUE)
         }
)
