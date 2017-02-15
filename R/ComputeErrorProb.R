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
#' ComputeErrorProb(ObsPredPar, ErrorProbMLEParam)
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
              localData.dm <- merge(localEdData.dm, localEdData.dm)
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
          output <- Reduce(function(x, y){merge(x, y)}, output, init = output[[1]])

          DomainNames <- Param@Imputation@DomainNames
          Domains <- dcast_StQ(object@Data, ExtractNames(DomainNames))
          output <- merge(output, Domains, by = IDQuals, all.y = TRUE)
          output <- Impute(output, Param@Imputation)
          nVariables <- length(Variables)
          newVNCVar <- new(Class = 'VNCdt',
                           data.table(IDQual = rep('', nVariables),
                                      NonIDQual = rep('', nVariables),
                                      IDDD = paste0('p.', Variables),
                                      UnitName = paste0('p.', Variables), InFiles = 'FF'))
          newVNC <- BuildVNC(list(MicroData = newVNCVar))
          auxDT <- data.table(Variable = paste0('p.', Variables),
                              Sort = rep('IDDD', nVariables),
                              Class = rep('numeric', nVariables),
                              Length = rep('10', nVariables))
          for(qual.index in seq(along = IDQuals)){

            auxDT[, (paste0('Qual', qual.index)) := IDQuals[qual.index]]
          }
          auxDT[, ValueRegExp := '[0-9\\.]+']
          newDD <- new(Class = 'DD', VarNameCorresp = newVNC, MicroData = new(Class = 'DDdt', auxDT))
          for (Var in Variables){
            object@Data <- setValues(object = object@Data,
                                     newDD = newDD,
                                     DDslot = 'MicroData',
                                     Value = output[[Var]])
          }

          object@VarRoles$ErrorProb <- c(object@VarRoles$ErrorProb, paste0('p.', Variables))


          return(object)

          }
)
