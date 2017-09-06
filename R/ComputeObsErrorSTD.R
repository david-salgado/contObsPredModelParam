#' \code{ComputeObsErrorSTD} computes the observation error variance for each statistical unit
#'
#'
#' @param object Object of class \linkS4class{contObsPredModelParam} containing the statistical
#' units whose probability of measurement error for each variable is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{ObsErrorSTDParam} with the parameters determining
#' the method of computation of the observation error variance of each statistical unit.
#'
#' @return Object of class \linkS4class{ObsErrorSTDParam} with the measurement error
#' probabilities computed for each variable and each statistical unit.
#'
#'
#' @examples
#' \dontrun{
#'
#' ObsPredPar <- new(Class = 'contObsPredModelParam',
#'                   Data = FD,
#'                   VarRoles = list(Units = 'NOrden', Domains = 'Tame_05._2.'))
#'
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.'))
#' ObsErrVarMLEParam <- new(Class = 'ObsErrorSTDMLEParam',
#'                          RawData = FD.StQList,
#'                          EdData = FF.StQList,
#'                          VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                          Imputation = ImpParam)
#' ObsPredPar <- ComputeObsErrorSTD(ObsPredPar, ObsErrVarMLEParam)
#'
#' }
#' @include contObsPredModelParam-class.R ObsErrorSTDParam-class.R
#'
#' @import data.table RepoTime StQ StQImputation

setGeneric("ComputeObsErrorSTD", function(object, Param) {standardGeneric("ComputeObsErrorSTD")})

#' @rdname ComputeObsErrorSTD
#'
#' @export
setMethod(f = "ComputeObsErrorSTD",
          signature = c("contObsPredModelParam", "ObsErrorSTDParam"),
          function(object, Param){

            RawPeriods <- getPeriods(getRawData(Param))
            EdPeriods <- getPeriods(getEdData(Param))
            CommonPeriods <- intersect(EdPeriods, RawPeriods)
            RawData <- subPeriods(getRawData(Param), CommonPeriods)
            EdData <- subPeriods(getEdData(Param), CommonPeriods)
            Units <- getUnits(getData(object))
            IDQuals <- names(Units)
            if (length(CommonPeriods) == 0) {

              stop('[contObsPredModelParam: validity] No common time periods between RawData and EdData.')

            }
            Variables <- getVarNames(Param)
            Variables <- getVarNames(Param)
            PeriodList <- lapply(CommonPeriods, function(Period){

              localVariables <- ExtractNames(Variables)
              localRawData.dm <- dcast_StQ(RawData[[Period]], localVariables)[, c(IDQuals, Variables), with = FALSE]
              localRawData.dm <- merge(Units, localRawData.dm, all.x = TRUE, by = IDQuals)
              localEdData.dm <- dcast_StQ(EdData[[Period]], localVariables)[, c(IDQuals, Variables), with = FALSE]
              localEdData.dm <- merge(Units, localEdData.dm, all.x = TRUE, by = IDQuals)
              localData.dm <- merge(localEdData.dm, localRawData.dm)

              for (Var in Variables){

                localData.dm[, Period := Period]
                localData.dm[, (paste0('ObsError.', Var)) := (-get(paste0(Var, '.x')) + get(paste0(Var, '.y'))) ^ 2 ]
                localData.dm[, (paste0('NumError.', Var)) := (get(paste0(Var, '.x')) != get(paste0(Var, '.y'))) * 1 ]
                localData.dm[, (paste0(Var, '.x')) := NULL]
                localData.dm[, (paste0(Var, '.y')) := NULL]

              }
              return(localData.dm)
            })

            nPeriods <- length(PeriodList)
            ProbList.dt <- rbindlist(PeriodList)

            output <- lapply(Variables, function(Var){

              localOutput <- ProbList.dt[, sum(get(paste0('ObsError.', Var)), na.rm = TRUE) / (sum(get(paste0('NumError.', Var)), na.rm = TRUE) - 1), by = IDQuals]
              setnames(localOutput, 'V1', Var)
              localOutput[is.infinite(get(Var)), (Var) := NA_real_]
              localOutput[is.nan(get(Var)), (Var) := 0]
              localOutput[, (Var) := sqrt(get(Var))]
              return(localOutput)

            })
            output <- Reduce(function(x, y){merge(x, y, by = intersect(names(x), names(y)))}, output, init = output[[1]])

            DomainNames <- getDomainNames(Param)
            Domains <- dcast_StQ(getData(object), ExtractNames(DomainNames))
            output <- merge(output, Domains, by = IDQuals, all.x = TRUE)
            output <- Impute(output, getImputation(Param))

            DD <- getDD(getData(object))
            VNC <- getVNC(DD)
            for (Var in Variables){

              localVar <- ExtractNames(Var)
              newDDdt <- DD$MicroData[Variable == localVar]
              newDDdt[, Variable := paste0('ObsErrorSTD', localVar)]

              auxUnitName <- IDDDToUnitNames(Var, DD)
              newVNCdt <- VNC$MicroData[UnitName == auxUnitName | IDQual != '' | NonIDQual != '']
              newVNCdt[UnitName == auxUnitName, IDDD := paste0('ObsErrorSTD', IDDD)]
              newVNCdt[UnitName == auxUnitName, UnitName := paste0('ObsErrorSTD', UnitName)]
              newVNC <- BuildVNC(list(MicroData = newVNCdt))

              newDD <- DD(VNC = newVNC, MicroData = newDDdt)
              newDD <- DD + newDD

              newData <- output[, c(IDQuals, Var), with = FALSE]
              setnames(newData, Var, paste0('ObsErrorSTD', IDDDToUnitNames(Var, DD)))
              newStQ <- melt_StQ(newData, newDD)

              setData (object) <- getData(object) + newStQ
            }

            setObsErrorSTD(object) <- c(getObsErrorSTD(object), paste0('ObsErrorSTD', Variables))

            if (length(getObjVariables) == 0) {

              setObjVariables(object) <- getVarNames(Param)

            }

            return(object)

          }
)
