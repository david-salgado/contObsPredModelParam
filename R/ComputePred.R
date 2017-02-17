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
#' FD.StQ <- ReadRepoFile('S:/E30183/E30183.FD_V1.MM032016.P_1', DD, perl = TRUE)
#' FF.StQ <- ReadRepoFile('S:/E30183/E30183.FF_V1.MM022016.D_1', DD, perl = TRUE)
#' ObsPredPar <- new(Class = 'contObsPredModelParam',
#'                   Data = FD.StQ,
#'                   VarRoles = list(Units = 'NOrden', Domains = c('Tame_05._2.', 'ActivEcono_35._4._2.1.4._0')))
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('PredCifraNeg_13.___', 'PredErrorSTDCifraNeg_13.___',
#'                              'PredPersonal_07.__2.__', 'PredErrorSTDPersonal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.', 'ActivEcono_35._4._2.1.4._0'))
#' PredlmParam <- new(Class = 'PredlmParam',
#'                    EdData = FF.StQ,
#'                    VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                    DomainNames = 'Tame_05._2.',
#'                    Imputation = ImpParam)
#'
#'
#' ComputePred(ObsPredPar, PredlmParam)
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
          signature = c("contObsPredModelParam", "PredParam"),
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
            VNCcols <- names(VNC$MicroData)
            for (Var in Variables){

              localVar <- ExtractNames(Var)
              for (prefix in c('Pred', 'PredErrorSTD')){
                auxDDdt <- DatadtToDT(DD@MicroData)[Variable == localVar]
                auxDDdt[, Variable := paste0(prefix, localVar)]
                newDDdt <- new(Class = 'DDdt', auxDDdt)
                localauxVNCdt <- VarNamesToDT(Var, DD)
                localauxVNCdt[, IDDD := paste0(prefix, IDDD)]
                for (col in names(localauxVNCdt)){ localauxVNCdt[is.na(get(col)), (col) := ''] }
                for (idqual in IDQuals){ localauxVNCdt[, (idqual) := '.'] }
                newCols <- setdiff(VNCcols, names(localauxVNCdt))
                localauxVNCdt[, (newCols) := '']
                localauxVNCdt[, UnitName := paste0(prefix, Var)]
                setcolorder(localauxVNCdt, VNCcols)
                newVNCdt <- list(MicroData = new(Class = 'VNCdt', localauxVNCdt))
                newVNC <- BuildVNC(newVNCdt)

                newDD <- new(Class = 'DD', VarNameCorresp = newVNC, MicroData = newDDdt)
                newDD <- DD + newDD

                #newData <- output[, c(IDQuals, Var), with = FALSE]
                #setnames(newData, Var, paste0(prefix, Var))
                newStQ <- melt_StQ(output[, c(IDQuals, paste0(prefix, Var)), with = FALSE], newDD)
                object@Data <- object@Data + newStQ
              }
            }


            object@VarRoles$PredValues <- c(object@VarRoles$PredValues, paste0('Pred', Variables))
            object@VarRoles$PredErrSTD <- c(object@VarRoles$PredErrSTD, paste0('PredErrSTD', Variables))


            return(object)

          }
)
