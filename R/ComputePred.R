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
#' library(StQImputation)
#' library(StQPrediction)
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
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L))
#' VarNames <- c('CifraNeg_13.___', 'Personal_07.__2.__')
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('PredCifraNeg_13.___', 'PredErrorSTDCifraNeg_13.___',
#'                              'PredPersonal_07.__2.__', 'PredErrorSTDPersonal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.'))
#' PredTSParam <- new(Class = 'PredTSParam',
#'                    TS = FF.StQList,
#'                    Param = BestTSPredParam,
#'                    Imputation = ImpParam)
#'
#' ObsPredPar <- ComputePred(ObsPredPar, PredlmParam)
#' ObsPredPar <- ComputePred(ObsPredPar, PredTSParam)
#'
#' }
setGeneric("ComputePred", function(object, Param) {standardGeneric("ComputePred")})

#' @rdname ComputePred
#'
#' @include contObsPredModelParam-class.R PredValueParam-class.R
#'
#' @import data.table RepoTime StQ StQImputation StQPrediction
#'
#' @export
setMethod(f = "ComputePred",
          signature = c("contObsPredModelParam", "PredValueTSParam"),
          function(object, Param){

            RawData.StQ <- getData(object)
            Units <- getUnits(RawData.StQ)
            IDQuals <- names(Units)
            Variables <- getObjVariables(object)
            DomainNames <- getDomainNames(Param)
            RawData.dm <- dcast_StQ(RawData.StQ, unique(ExtractNames((c(Variables, DomainNames)))))
            RawData.dm <- RawData.dm[, c(IDQuals, Variables, DomainNames), with = FALSE]
            PredParam <- getPredictionParam(Param)
            output <- StQPrediction::Predict(RawData.dm, PredParam)
            setnames(output, paste0('STD', Variables), paste0('PredErrorSTD', Variables))
            PredVariables <- paste0('Pred', getVarNames(Param))
            ErrorSTDVariables <- paste0('PredErrorSTD', getVarNames(Param))
            ImpParam <- getImputationParam(Param)
            setVarNames(ImpParam) <- c(PredVariables, ErrorSTDVariables)
            output <- merge(output, RawData.dm[, c(IDQuals, DomainNames), with = FALSE], all.x = TRUE, by = IDQuals)
            output <- Impute(output, ImpParam)

            DD <- getDD(getData(object))
            VNC <- getVNC(DD)
            for (Var in Variables){

              localVar <- unique(ExtractNames(Var))
              newDDdt <- DD$MicroData[Variable == localVar]
              for (prefix in c('Pred', 'PredErrorSTD')){

                newDDdt[, Variable := paste0(prefix, localVar)]

                auxUnitName <- IDDDToUnitNames(Var, DD)
                newVNCdt <- VNC$MicroData[UnitName == auxUnitName | IDQual != '' | NonIDQual != '']
                newVNCdt[UnitName == auxUnitName, IDDD := paste0(prefix, IDDD)]
                newVNCdt[UnitName == auxUnitName, UnitName := paste0(prefix, UnitName)]
                newVNC <- BuildVNC(list(MicroData = newVNCdt))

                newDD <- DD(VNC = newVNC, MicroData = newDDdt)
                newDD <- DD + newDD

                newData <- output[, c(IDQuals, paste0(prefix, Var)), with = FALSE]
                
                newStQ <- melt_StQ(newData, newDD)
                setData(object)<- getData(object) + newStQ
              }
            }

            setPredValues(object) <- c(getPredValues(object), paste0('Pred', Variables))
            setPredErrorSTD(object)<- c(getPredErrorSTD(object), paste0('PredErrorSTD', Variables))


            return(object)

          }
)
