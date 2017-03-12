#' \code{ComputePred} computes the predicted values and the prediction error std for each
#' statistical unit
#'
#'
#' @param object Object of class \linkS4class{contObsPredModelParam} containing the statistical
#' units whose predicted values and prediction error std for each variable is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{PredParam} with the parameters determining
#' the method of computation of the predicted values and prediction error std of each statistical
#' unit.
#'
#' @return Object of class \linkS4class{contObsPredModelParam} with the predicted values and
#' prediction error std computed for each variable and each statistical unit.
#'
#'
#' @examples
#' \dontrun{
#' library(xlsx)
#' library(StQ)
#' library(RepoReadWrite)
#' DD <- RepoXLSToDD('S:/E30183/E30183.NombresVariables_V1.xlsx')
#' FD <- ReadRepoFile('S:/E30183/E30183.FD_V1.MM032016.P_1', DD, perl = TRUE)
#' ObsPredPar <- new(Class = 'contObsPredModelParam',
#'                   Data = FD,
#'                   VarRoles = list(Units = 'NOrden', Domains = c('Tame_05._4.', 'ActivEcono_35._4._2.1.4._0')))
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('PredCifraNeg_13.___', 'PredErrorSTDCifraNeg_13.___',
#'                              'PredPersonal_07.__2.__', 'PredErrorSTDPersonal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.'))
#' PredlmParam <- new(Class = 'PredlmParam',
#'                    EdData = FD,
#'                    VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                    DomainNames = 'Tame_05._2.',
#'                    Imputation = ImpParam)
#' ObsPredPar <- ComputePred(ObsPredPar, PredlmParam)
#'
#' }
setGeneric("ComputePred", function(object, Param) {standardGeneric("ComputePred")})

#' @rdname ComputePred
#'
#' @include contObsPredModelParam-class.R
#'
#' @import data.table RepoTime StQ StQImputation StQPrediction
#'
#' @export
setMethod(f = "ComputePred",
          signature = c("contObsPredModelParam", "PredlmParam"),
          function(object, Param){

            RawData.StQ <- object@Data
            Units <- getUnits(RawData.StQ)
            IDQuals <- names(Units)
            Variables <- Param@VarNames
            DomainNames <- union(Param@DomainNames, Param@Imputation@DomainNames)
            RawData.dm <- dcast_StQ(RawData.StQ, unique(ExtractNames((c(Variables, DomainNames)))))
            RawData.dm <- RawData.dm[, c(IDQuals, Variables, DomainNames), with = FALSE]
            output <- StQPrediction::Predict(RawData.dm, Param)
            output <- Impute(output, Param@Imputation)

            DD <- getDD(object@Data)
            VNC <- getVNC(DD)

            auxVNCIDQual <- VNC$MicroData[IDQual %chin% IDQuals]
            auxVNCNonIDQual <- VNC$MicroData[NonIDQual != '']

            VNCcols <- names(VNC$MicroData)

            for (Var in Variables){

              localVar <- unique(ExtractNames(Var))
              auxDDdt <- DD$MicroData[Variable == localVar]
              for (prefix in c('Pred', 'PredErrorSTD')){

                auxDDdt[, Variable := paste0(prefix, localVar)]
                auxDDdt[, Length := '8']

                auxVNCdt <- VarNamesToDT(Var, DD)
                auxNonIDQuals <- setdiff(names(auxVNCdt), 'IDDD')
                auxVNCdt[, IDDD := paste0(prefix, localVar)]
                auxVNCdt[, UnitName := paste0(prefix, IDDDToUnitNames(Var, DD))]
                auxNonIDQualdt <- auxVNCNonIDQual[, c('NonIDQual', auxNonIDQuals), with = FALSE]
                auxVNCdt <- rbindlist(list(auxVNCdt, auxNonIDQualdt), fill = TRUE)
                auxVNCdt <- rbindlist(list(auxVNCdt, auxVNCIDQual), fill = TRUE)
                for (col in names(auxVNCdt)) { auxVNCdt[is.na(get(col)), (col) := ''] }
                setcolorder(auxVNCdt, VNCcols)
                newVNC <- BuildVNC(list(MicroData = auxVNCdt))

                newDD <- DD(VNC = newVNC, MicroData = auxDDdt)
                newDD <- DD + newDD

                newData <- output[, c(IDQuals, paste0(c('Pred', 'PredErrorSTD'), Var)), with = FALSE]
                #setnames(newData, Var, paste0(prefix, IDDDToUnitNames(Var, DD)))
                newStQ <- melt_StQ(newData, newDD)
                object@Data <- object@Data + newStQ
              }
            }

            object@VarRoles$PredValues <- c(object@VarRoles$PredValues, paste0('Pred', Variables))
            object@VarRoles$PredErrorSTD <- c(object@VarRoles$PredErrorSTD, paste0('PredErrorSTD', Variables))


            return(object)

          }
)
