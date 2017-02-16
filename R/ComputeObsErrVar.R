#' \code{ComputeObsErrVar} computes the observation error variance for each statistical unit
#'
#'
#' @param object Object of class \linkS4class{contObsPredModelParam} containing the statistical
#' units whose probability of measurement error for each variable is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{ObsErrVarParam} with the parameters determining
#' the method of computation of the observation error variance of each statistical unit.
#'
#' @return Object of class \linkS4class{ObsErrVarParam} with the measurement error
#' probabilities computed for each variable and each statistical unit.
#'
#'
#' @examples
#' \dontrun{
#'
#' ObsPredPar <- new(Class = 'contObsPredModelParam',
#'                   Data = Data,
#'                   VarRoles = list(Units = 'NOrden', Domains = 'Tame_05._2.'))
#'
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.', 'ActivEcono_35._4._2.1.4._0'))
#' ObsErrVarMLEParam <- new(Class = 'ObsErrVarMLEParam',
#'                          RawData = FF.StQList,
#'                          EdData = FD.StQList,
#'                          VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                          Imputation = ImpParam)
#' ComputeErrorProb(ObsPredPar, ObsErrVarMLEParam)
#'
#' }
setGeneric("ComputeObsErrVar", function(object, Param) {standardGeneric("ComputeObsErrVar")})

#' @rdname ComputeObsErrVar
#'
#' @include contObsPredModelParam-class.R ObsErrVarParam-class.R
#'
#' @import data.table RepoTime StQ StQImputation
#'
#' @export
setMethod(f = "ComputeObsErrVar",
          signature = c("contObsPredModelParam", "ObsErrVarParam"),
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
              localRawData.dm <- dcast_StQ(RawData[[Period]], localVariables)
              localRawData.dm <- merge(Units, localRawData.dm, all.x = TRUE, by = IDQuals)
              localEdData.dm <- dcast_StQ(EdData[[Period]], localVariables)
              localEdData.dm <- merge(Units, localEdData.dm, all.x = TRUE, by = IDQuals)
              localData.dm <- merge(localEdData.dm, localRawData.dm)
              for (Var in Variables){

                localData.dm[, Period := Period]
                localData.dm[, (paste0('Unit.p.', Var)) := (get(paste0(Var, '.x')) != get(paste0(Var, '.y'))) * 1]
                localData.dm[, (paste0(Var, '.x')) := NULL]
                localData.dm[, (paste0(Var, '.y')) := NULL]

              }
              return(localData.dm)
            })
            nPeriods <- length(PeriodList)
            ProbList.dt <- rbindlist(PeriodList)

            output <- lapply(Variables, function(Var){

              localOutput <- ProbList.dt[, sum(get(paste0('Unit.p.', Var)), na.rm = TRUE) / nPeriods, by = IDQuals]
              setnames(localOutput, 'V1', Var)
              return(localOutput)

            })
            output <- Reduce(function(x, y){merge(x, y, by = intersect(names(x), names(y)))}, output, init = output[[1]])

            DomainNames <- Param@Imputation@DomainNames
            Domains <- dcast_StQ(object@Data, ExtractNames(DomainNames))
            output <- merge(output, Domains, by = IDQuals, all.y = TRUE)
            output <- Impute(output, Param@Imputation)

            DD <- getDD(object@Data)
            VNC <- getVNC(DD)
            VNCcols <- names(VNC$MicroData)
            for (Var in Variables){

              localVar <- ExtractNames(Var)
              auxDDdt <- DatadtToDT(DD@MicroData)[Variable == localVar]
              auxDDdt[, Variable := paste0('ErrorProb', localVar)]
              newDDdt <- new(Class = 'DDdt', auxDDdt)

              auxVNCdt <- VarNamesToDT(Var, DD)
              auxVNCdt[, IDDD := paste0('ErrorProb', IDDD)]
              for (col in names(auxVNCdt)){ auxVNCdt[is.na(get(col)), (col) := ''] }
              for (idqual in IDQuals){ auxVNCdt[, (idqual) := '.'] }
              newCols <- setdiff(VNCcols, names(auxVNCdt))
              auxVNCdt[, (newCols) := '']
              auxVNCdt[, UnitName := paste0('ErrorProb', Var)]
              setcolorder(auxVNCdt, VNCcols)
              newVNCdt <- list(MicroData = new(Class = 'VNCdt', auxVNCdt))
              newVNC <- BuildVNC(newVNCdt)

              newDD <- new(Class = 'DD', VarNameCorresp = newVNC, MicroData = newDDdt)
              newDD <- DD + newDD

              newData <- output[, c(IDQuals, Var), with = FALSE]
              setnames(newData, Var, paste0('ErrorProb', Var))
              newStQ <- melt_StQ(newData, newDD)
              object@Data <- object@Data + newStQ
            }


            object@VarRoles$ErrorProb <- c(object@VarRoles$ErrorProb, paste0('ErrorProb', Variables))


            return(object)

          }
)
