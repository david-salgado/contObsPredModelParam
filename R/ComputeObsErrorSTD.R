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
#'                   Data = Data,
#'                   VarRoles = list(Units = 'NOrden', Domains = 'Tame_05._4.'))
#'
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._4.', 'ActivEcono_35._4._2.1.4._0'))
#' ObsErrVarMLEParam <- new(Class = 'ObsErrorSTDMLEParam',
#'                          RawData = FGList,
#'                          EdData = FDList,
#'                          VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                          Imputation = ImpParam)
#' ObsPredPar <- ComputeObsErrorSTD(ObsPredPar, ObsErrVarMLEParam)
#'
#' }
setGeneric("ComputeObsErrorSTD", function(object, Param) {standardGeneric("ComputeObsErrorSTD")})

#' @rdname ComputeObsErrorSTD
#'
#' @include contObsPredModelParam-class.R ObsErrorSTDParam-class.R
#'
#' @import data.table RepoTime StQ StQImputation
#'
#' @export
setMethod(f = "ComputeObsErrorSTD",
          signature = c("contObsPredModelParam", "ObsErrorSTDParam"),
          function(object, Param){

            RawPeriods <- getPeriods(Param@RawData)
            EdPeriods <- getPeriods(Param@EdData)
            CommonPeriods <- intersect(EdPeriods, RawPeriods)
            RawData <- Param@RawData[CommonPeriods]
            EdData <- Param@EdData[CommonPeriods]
            Units <- getUnits(object@Data)
            IDQuals <- names(Units)
            if (length(CommonPeriods) == 0) {

              stop('[contObsPredModelParam: validity] No common time periods between RawData and EdData.')

            }
            Variables <- Param@VarNames
            PeriodList <- lapply(CommonPeriods, function(Period){

              localVariables <- ExtractNames(Variables)
              localRawData.dm <- dcast_StQ(RawData[[Period]], localVariables)[, c(IDQuals, Variables), with = FALSE]
              localRawData.dm <- merge(Units, localRawData.dm, all.x = TRUE, by = IDQuals)
              localEdData.dm <- dcast_StQ(EdData[[Period]], localVariables)[, c(IDQuals, Variables), with = FALSE]
              localEdData.dm <- merge(Units, localEdData.dm, all.x = TRUE, by = IDQuals)
              localData.dm <- merge(localEdData.dm, localRawData.dm)
              for (Var in Variables){

                localData.dm[, Period := Period]
                localData.dm[, (paste0('ObsError.', Var)) := -get(paste0(Var, '.x')) + get(paste0(Var, '.y')) ]
                localData.dm[, (paste0('NumError.', Var)) := (get(paste0(Var, '.x')) != get(paste0(Var, '.y'))) * 1 ]
                localData.dm[, (paste0(Var, '.x')) := NULL]
                localData.dm[, (paste0(Var, '.y')) := NULL]

              }
              return(localData.dm)
            })

            nPeriods <- length(PeriodList)
            ProbList.dt <- rbindlist(PeriodList)

            output <- lapply(Variables, function(Var){

              localOutput <- ProbList.dt[, sum(get(paste0('ObsError.', Var)), na.rm = TRUE) / sum(get(paste0('NumError.', Var)), na.rm = TRUE), by = IDQuals]
              setnames(localOutput, 'V1', Var)
              localOutput[is.nan(get(Var)), (Var) := 0]
              localOutput[, (Var):= sqrt(get(Var))]
              return(localOutput)

            })
            output <- Reduce(function(x, y){merge(x, y, by = intersect(names(x), names(y)))}, output, init = output[[1]])

            DomainNames <- Param@Imputation@DomainNames
            Domains <- dcast_StQ(object@Data, ExtractNames(DomainNames))
            output <- merge(output, Domains, by = IDQuals, all.x = TRUE)
            output <- Impute(output, Param@Imputation)

            DD <- getDD(object@Data)
            VNC <- getVNC(DD)
            VNCcols <- names(VNC$MicroData)
            for (Var in Variables){

              localVar <- ExtractNames(Var)
              auxDDdt <- DatadtToDT(DD@MicroData)[Variable == localVar]
              auxDDdt[, Variable := paste0('ObsErrorSTD', localVar)]
              newDDdt <- new(Class = 'DDdt', auxDDdt)

              auxVNCdt <- VarNamesToDT(Var, DD)
              auxVNCdt[, IDDD := paste0('ObsErrorSTD', IDDD)]
              for (col in names(auxVNCdt)){ auxVNCdt[is.na(get(col)), (col) := ''] }
              for (idqual in IDQuals){ auxVNCdt[, (idqual) := '.'] }
              newCols <- setdiff(VNCcols, names(auxVNCdt))
              auxVNCdt[, (newCols) := '']
              auxVNCdt[, UnitName := paste0('ObsErrorSTD', IDDDToUnitNames(Var, DD))]
              setcolorder(auxVNCdt, VNCcols)
              newVNCdt <- list(MicroData = new(Class = 'VNCdt', auxVNCdt))
              newVNC <- BuildVNC(newVNCdt)

              newDD <- new(Class = 'DD', VarNameCorresp = newVNC, MicroData = newDDdt)
              newDD <- DD + newDD

              newData <- output[, c(IDQuals, Var), with = FALSE]
              setnames(newData, Var, paste0('ObsErrorSTD', IDDDToUnitNames(Var, DD)))
              newStQ <- melt_StQ(newData, newDD)
              object@Data <- object@Data + newStQ
            }

            object@VarRoles$ObsErrorSTD <- c(object@VarRoles$ObsErrorSTD, paste0('ObsErrorSTD', Variables))


            return(object)

          }
)
