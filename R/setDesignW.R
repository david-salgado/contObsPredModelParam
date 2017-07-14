#' \code{setDesignW} sets the design weight(s) for each variable and for each statistical unit
#'
#'
#' @param object Object of class \linkS4class{contObsPredModelParam} containing the statistical
#' units whose design weights are to be computed.
#'
#' @param Param \linkS4class{data.table} with the design weigths for each variable and each
#' statistical unit.
#'
#' @return Object of class \linkS4class{contObsPredModelParam} with the design weights.
#'
#'
#' @examples
#' \dontrun{
#'
#' load('C:/FDStQList.RData')
#' load('C:/FGStQList.RData')
#' FD <- FD.StQList[['MM112016']]
#' FDList <- FD.StQList[-11]
#' ObsPredPar <- new(Class = 'contObsPredModelParam',
#'                   Data = FG,
#'                   VarRoles = list(Units = 'NOrden', Domains = 'Tame_05._4.'))
#' Weights <- StQ::dcast_StQ(ObsPredPar@Data)[, 'NOrden', with = FALSE][, Personal_07.__2.__ := 1][, CifraNeg_13.___ := 1]
#' setDesignW(ObsPredPar) <- Weights
#'
#' }
#' @rdname setDesignW
#'
#' @export
setGeneric("setDesignW<-", function(object, value) {standardGeneric("setDesignW<-")})

#' @rdname setDesignW
#'
#' @include contObsPredModelParam-class.R
#'
#' @import data.table RepoTime StQ
#'
#' @export
setReplaceMethod(
          f = "setDesignW",
          signature = c("contObsPredModelParam", "data.table"),
          function(object, value){

            Units <- getUnits(object@Data)
            IDQuals <- names(Units)
            if (!all(IDQuals %in% names(value))) {

              stop('[contObsPredModelParam: setDesignW] The input data.table Weights does not have all unit identifiers.')

            }

            Variables <- object@VarRoles[['ObjVariables']]

            DD <- getDD(object@Data)
            VNC <- getVNC(DD)
            for (Var in Variables){

              localVar <- unique(ExtractNames(Var))
              newDDdt <- DD$MicroData[Variable == localVar]
              newDDdt[, Variable := paste0('DesignW', localVar)]

              auxUnitName <- IDDDToUnitNames(Var, DD)
              newVNCdt <- VNC$MicroData[UnitName == auxUnitName | IDQual != '' | NonIDQual != '']
              newVNCdt[UnitName == auxUnitName, IDDD := paste0('DesignW', IDDD)]
              newVNCdt[UnitName == auxUnitName, UnitName := paste0('DesignW', UnitName)]
              newVNC <- BuildVNC(list(MicroData = newVNCdt))

              newDD <- DD(VNC = newVNC, MicroData = newDDdt)
              newDD <- DD + newDD


              newData <- value[, c(IDQuals, Var), with = FALSE]
              setnames(newData, Var, paste0('DesignW', IDDDToUnitNames(Var, DD)))

              newStQ <- melt_StQ(newData, newDD)
              object@Data <- object@Data + newStQ
            }


            object@VarRoles$DesignW <- c(object@VarRoles$DesignW, paste0('DesignW', Variables))

            return(object)

          }
)
