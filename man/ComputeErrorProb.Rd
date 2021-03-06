% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ComputeErrorProb.R
\docType{methods}
\name{ComputeErrorProb}
\alias{ComputeErrorProb}
\alias{ComputeErrorProb,contObsPredModelParam,ErrorProbParam-method}
\title{\code{ComputeErrorProb} computes the probability of measurement error for each statistical unit}
\usage{
ComputeErrorProb(object, Param)

\S4method{ComputeErrorProb}{contObsPredModelParam,ErrorProbParam}(object, Param)
}
\arguments{
\item{object}{Object of class \linkS4class{contObsPredModelParam} containing the statistical
units whose probability of measurement error for the variables is to be computed.}

\item{Param}{Object of virtual class \linkS4class{ErrorProbParam} with the parameters determining
the variables and the method of computation of the probability of measurement error.}
}
\value{
Object of class \linkS4class{contObsPredModelParam} with the measurement error
probabilities computed for each variable specified in the parameters and each statistical unit.
}
\description{
\code{ComputeErrorProb} computes the probability of measurement error for each statistical unit
}
\examples{
\dontrun{

load('C:/FDStQList.RData')
load('C:/FGStQList.RData')
FGListofStQ <- getRepo(FGList)
FGListofStQ <- lapply(getPeriods(FD.StQList), function(Month){

 out <- FGList[[Month]] + FD.StQList[[Month]][IDDD \%in\% c('Tame', 'ActivEcono')]
 return(out)
})
names(FGListofStQ) <- getPeriods(FD.StQList)
FGList <- BuildStQList(FGListofStQ)
FG <- FGList[['MM112016']]
FGList <- FGList[-11]
FDList <- FD.StQList[-11]
FD <- FD.StQList[['MM112016']]
rm(FD.StQList, FG.StQList, FGListofStQ)
ObsPredPar <- new(Class = 'contObsPredModelParam',
                  Data = FD,
                  VarRoles = list(Units = 'NOrden', Domains = 'Tame_05._2.'))

ImpParam <- new(Class = 'MeanImputationParam',
                VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
                DomainNames =  c('Tame_05._2.'))
ErrorProbMLEParam <- new(Class = 'ErrorProbMLEParam',
                         RawData = FD.StQList,
                         EdData = FF.StQList,
                         VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
                         Imputation = ImpParam)
ObsPredPar <- ComputeErrorProb(ObsPredPar, ErrorProbMLEParam)

}
}
