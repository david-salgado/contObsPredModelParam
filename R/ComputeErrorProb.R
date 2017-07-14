#' \code{ComputeErrorProb} computes the probability of measurement error for each statistical unit
#'
#'
#' @param object Object of class \linkS4class{contObsPredModelParam} containing the statistical
#' units whose probability of measurement error for each variable is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{ErrorProbParam} with the parameters determining
#' the method of computation of the probability of measurement error of each statistical unit.
#'
#' @return Object of class \linkS4class{contObsPredModelParam} with the measurement error
#' probabilities computed for each variable and each statistical unit.
#'
#'
#' @examples
#' \dontrun{
#'
#' load('C:/FDStQList.RData')
#' load('C:/FGStQList.RData')
#' FGListofStQ <- getRepo(FGList)
#' FGListofStQ <- lapply(getPeriods(FD.StQList), function(Month){
#'
#'  out <- FGList[[Month]] + FD.StQList[[Month]][IDDD %in% c('Tame', 'ActivEcono')]
#'  return(out)
#' })
#' names(FGListofStQ) <- getPeriods(FD.StQList)
#' FGList <- BuildStQList(FGListofStQ)
#' FG <- FGList[['MM112016']]
#' FGList <- FGList[-11]
#' FDList <- FD.StQList[-11]
#' FD <- FD.StQList[['MM112016']]
#' rm(FD.StQList, FG.StQList, FGListofStQ)
#' ObsPredPar <- new(Class = 'contObsPredModelParam',
#'                   Data = FD,
#'                   VarRoles = list(Units = 'NOrden', Domains = 'Tame_05._2.'))
#'
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.'))
#' ErrorProbMLEParam <- new(Class = 'ErrorProbMLEParam',
#'                          RawData = FD.StQList,
#'                          EdData = FF.StQList,
#'                          VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                          Imputation = ImpParam)
#' ObsPredPar <- ComputeErrorProb(ObsPredPar, ErrorProbMLEParam)
#'
#' }
setGeneric("ComputeErrorProb", function(object, Param) {standardGeneric("ComputeErrorProb")})

#' @rdname ComputeErrorProb
#'
#' @include contObsPredModelParam-class.R ErrorProbParam-class.R
#'
#' @import data.table RepoTime StQ StQImputation
#'
#' @export
setMethod(f = "ComputeErrorProb",
          signature = c("contObsPredModelParam", "ErrorProbParam"),
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
          PeriodList <- lapply(CommonPeriods, function(Period){

              localVariables <- ExtractNames(Variables)
              localRawData.dm <- dcast_StQ(RawData[[Period]], localVariables)
              localRawData.dm <- merge(Units, localRawData.dm, all.x = TRUE, by = IDQuals)
              localEdData.dm <- dcast_StQ(EdData[[Period]], localVariables)
              localEdData.dm <- merge(Units, localEdData.dm, all.x = TRUE, by = IDQuals)
              localData.dm <- merge(localEdData.dm, localRawData.dm)
              for (Var in Variables){

                localData.dm[, Period := Period]
                localData.dm[, (paste0('Unit.p.', Var)) := (get(paste0(Var, '.x')) != get(paste0(Var, '.y'))) * 1L]
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

          DomainNames <- getDomainNames(Param)
          Domains <- dcast_StQ(getData(object), ExtractNames(DomainNames))
          output <- merge(output, Domains, by = IDQuals, all.x = TRUE)
          output <- Impute(output, getImputation(Param))

          DD <- getDD(getData(object))
          VNC <- getVNC(DD)
          for (Var in Variables){

            localVar <- ExtractNames(Var)
            newDDdt <- DD$MicroData[Variable == localVar]
            newDDdt[, Variable := paste0('ErrorProb', localVar)]

            auxUnitName <- IDDDToUnitNames(Var, DD)
            newVNCdt <- VNC$MicroData[UnitName == auxUnitName | IDQual != '' | NonIDQual != '']
            newVNCdt[UnitName == auxUnitName, IDDD := paste0('ErrorProb', IDDD)]
            newVNCdt[UnitName == auxUnitName, UnitName := paste0('ErrorProb', UnitName)]
            newVNC <- BuildVNC(list(MicroData = newVNCdt))

            newDD <- DD(VNC = newVNC, MicroData = newDDdt)
            newDD <- DD + newDD

            newData <- output[, c(IDQuals, Var), with = FALSE]

            setnames(newData, Var, paste0('ErrorProb', IDDDToUnitNames(Var, DD)))
            newStQ <- melt_StQ(newData, newDD)
            setData(object) <- getData(object) + newStQ
          }

          setErrorProb(object) <- c(getErrorProb(object), paste0('ErrorProb', Variables))

          if (length(getObjVariables(object)) == 0) {

            setObjVariables(object)<- getVarNames(Param)

          }

          return(object)

          }
)
