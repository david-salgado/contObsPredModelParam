#' @title Plot the main values in the optimization approach to selective editing for the specified
#' units.
#'
#' @description This method overloads the plot function to draw a picture with the information about
#' the queried units in the optimization approach to selective editing.
#'
#' @param x Object of class \linkS4class{contObsPredModelParam}.
#'
#' @param y Charactec vector with the names of the variable names.
#'
#' @param ... Arguments to be passed to methods, especially including
#'
#' \itemize{
#'
#'  \item \code{Query} as a \linkS4class{data.table} with the values of the identification
#'  variables of the queried statistical units;
#'
#'  \item \code{FT} as a \linkS4class{StQ} object with the values of the selected units in the
#'  optimization approach to selective editing;
#'
#'  \item \code{ErrorMoments} as a \code{numeric} vector with the error moments corresponding to
#'  queried units;
#'}
#'
#' @return Returns an invisible \code{NULL} plotting the one scatterplot per variable.
#'
#' @examples
#'
#' @rdname plot
#'
#' @import data.table ggplot2
#'
#'
#' @export
setMethod(f = "plot",
          signature = c("contObsPredModelParam", "character"),
          function(x, y, ...) {

            VarRoles <- x@VarRoles
            IDQual <- VarRoles$Units
            VarNames <- VarRoles$ObjVariables
            DomainNames <- VarRoles$Domains
            sigmaNames <- VarRoles$obsErrorSTD
            errorProbNames <- VarRoles$ErrorProb
            predValuesNames <- VarRoles$PredValues
            nuNames <- VarRoles$PredErrorSTD
            wNames <- VarRoles$DesignW

            nVar <- length(y)
            if (nVar > 4) stop('[contObsPredModelParam::plot] At most 4 variables can be plotted.\n')

            if (!all(y %in% VarNames)) stop('[contObsPredModelParam::plot] Some variables of y are not in the input object.\n')
            VarNames <- y
            names.list <- list(sigmaNames = sigmaNames, errorProbNames = errorProbNames,
                               predValuesNames = predValuesNames, nuNames = nuNames, wNames = wNames)

            namesQueried <- lapply(names.list, function(names){

              out <- list()
              for (Var in VarNames){

                out[[Var]] <- names[grep(Var, names)]
              }

              out <- unlist(out)

            })

            sigmaNames <- namesQueried[['sigmaNames']]
            errorProbNames <- namesQueried[['errorProbNames']]
            predValuesNames <- namesQueried[['predValuesNames']]
            nuNames <- namesQueried[['nuNames']]
            wNames <- namesQueried[['wNames']]


            ExtraPar <- list(...)
            if (!'Query' %in% names(ExtraPar)) stop('[contObsPredModelParam::plot] Query is a compulsory parameter.\n')
            if (!'FT' %in% names(ExtraPar)) stop('[contObsPredModelParam::plot] FT is a compulsory parameter.\n')
            if (!'ErrorMoments' %in% names(ExtraPar)) stop('[contObsPredModelParam::plot] ErrorMoments is a compulsory parameter.\n')

            if (!all(IDQual %in% names(ExtraPar$Query))) stop('[contObsPredModelParam::plot] Query does not have correct names.\n')
            if (length(ExtraPar$ErrorMoments) != nVar * dim(ExtraPar$Query)[1]) stop('[contObsPredModelParam::plot] ErrorMoments does not have correct length.\n')

            ObsPredPar.dm <- dcast_StQ(getData(x))[, unlist(VarRoles), with = FALSE]
            setkeyv(ExtraPar$Query, IDQual)
            setkeyv(ObsPredPar.dm, IDQual)
            QueriedDomain <- ObsPredPar.dm[ExtraPar$Query][, DomainNames, with = FALSE]
            QueriedDomain <- QueriedDomain[!duplicated(QueriedDomain)]
            if (dim(QueriedDomain)[1] > 1) stop('[contObsPredModelParam::plot] Query contains units from more than one domain.\n')
            setkeyv(ObsPredPar.dm, names(QueriedDomain))
            ModelParam <- ObsPredPar.dm[QueriedDomain][, c(IDQual, VarNames, wNames, predValuesNames, nuNames, sigmaNames), with = FALSE]

            # data.table con los cuantiles de los pesos de muestreo
            SamplingWeight.aux <- ModelParam[, wNames, with = FALSE]
            SamplingWeight <- lapply(names(SamplingWeight.aux), function(Var){

              QuantileSamplingWeight <- ecdf(SamplingWeight.aux[[Var]])(SamplingWeight.aux[[Var]])
              out <- cbind(ExtraPar$Query, QuantileSamplingWeight)
              out[, variable := strsplit(Var, 'DesignW')[[1]][2]]
              return(out)

            })
            SamplingWeight.dt <- rbindlist(SamplingWeight)

            # data.tables con los valores RawVar, PredValues, PredErrorSTD y ObsErrorSTD
            ModelParam.melt <- melt(ModelParam)
            RawVar <- ModelParam.melt[variable %in% VarNames][, c(IDQual, 'variable', 'value'), with = FALSE]
            setnames(RawVar, 'value', 'RawValues')
            PredValues <- ModelParam.melt[variable %in% predValuesNames][, c(IDQual, 'variable', 'value'), with = FALSE]
            PredValues <- lapply(VarNames, function(Var){

              rows <- grep(Var, PredValues[['variable']])
              out <- PredValues[rows][, variable := Var]
              return(out)

            })
            PredValues <- rbindlist(PredValues)
            setnames(PredValues, 'value', 'PredValues')
            PredErrorSTD <- ModelParam.melt[variable %in% nuNames][, c(IDQual, 'variable', 'value'), with = FALSE]
            PredErrorSTD <- lapply(VarNames, function(Var){

              rows <- grep(Var, PredErrorSTD[['variable']])
              out <- PredErrorSTD[rows][, variable := Var]
              return(out)

            })
            PredErrorSTD <- rbindlist(PredErrorSTD)
            setnames(PredErrorSTD, 'value', 'PredErrorSTD')
            ObsErrorSTD <- ModelParam.melt[variable %in% sigmaNames][, c(IDQual, 'variable', 'value'), with = FALSE]
            ObsErrorSTD <- lapply(VarNames, function(Var){

              rows <- grep(Var, ObsErrorSTD[['variable']])
              out <- ObsErrorSTD[rows][, variable := Var]
              return(out)

            })
            ObsErrorSTD <- rbindlist(ObsErrorSTD)
            setnames(ObsErrorSTD, 'value', 'ObsErrorSTD')

            # Obtenemos el data.table ModelParam que contiene todos los valores de los data.tables anteriores
            ModelParam.melt <- list(SamplingWeight.dt, RawVar, PredValues, PredErrorSTD, ObsErrorSTD)
            ModelParam <- Reduce(merge, ModelParam.melt)

            # Añadimos a ModelParam las columnas Flagged y Queried y las correspondientes a DomainNames
            FT <- ExtraPar$FT
            SelectedUnits <- unique(getData(FT)[, c(IDQual), with = FALSE])
            ModelParam[, Flagged := ifelse(NOrden %in% SelectedUnits[[IDQual]], 'Selected', 'Not Selected')]
            ModelParam[, Queried := 'Queried']
            for (Domain in DomainNames){

              ModelParam[, (Domain) := QueriedDomain[[Domain]]]
            }


            # data.table UnitParam con los valores QuantileGlobalScore de las unidades seleccionadas en el FT
            FT.dt <- dcast_StQ(FT)
            setkeyv(FT.dt, IDQual)
            FT.dt <- FT.dt[ExtraPar$Query]
            UnitParam <- FT.dt[, c(IDQual, 'Parametro_07._5.1.1.6.'), with = FALSE]
            setnames(UnitParam, 'Parametro_07._5.1.1.6.', 'QuantileGlobalScore')
            UnitParam <- UnitParam[!is.na(UnitParam[['QuantileGlobalScore']])]


            # data.table IDEdit.dt con los valores de IDEdit para las unidades seleccionadas en el FT
            IDEdit.aux <- FT.dt[, 'IDEdit', with = FALSE]
            IDEdit.aux <- unique(IDEdit.aux[!is.na(IDEdit.aux[['IDEdit']])][['IDEdit']])

            IDEdit.dt <- lapply(VarNames, function(Var){

              ModelParam.aux <- merge(ModelParam[, c(IDQual, 'variable'), with = FALSE], SelectedUnits, by = IDQual)
              rows <- grep(Var, ModelParam.aux[['variable']])
              Edit <- IDEdit.aux[grep(strsplit(Var, '_')[[1]][1], IDEdit.aux)]
              out <- ModelParam.aux[rows][, IDEdit := Edit]
              return(out)

            })
            IDEdit.dt <- rbindlist(IDEdit.dt)


            # data.table ErrorMoments.dt con los valores QuantileErrorMoment
            ErrorMoments <- ExtraPar$ErrorMoments
            ErrorMoments.length <- length(ErrorMoments) / nVar
            ErrorMoments.dt <- lapply(seq(along = VarNames), function(numVar){

              init <- ErrorMoments.length * (numVar - 1) + 1
              fin <- ErrorMoments.length * numVar
              moments <- data.table(QuantileErrorMoment = ecdf(ErrorMoments[init:fin])(ErrorMoments[init:fin]))
              out <- cbind(ExtraPar$Query, moments)
              out[, variable := VarNames[numVar]]
              return(out)

            })
            ErrorMoments.dt <- rbindlist(ErrorMoments.dt)

            # Unimos los data.table ModelParam, UnitParam, IDEdit.dt y ErrorMoments.dt para obtener el data.table final Data
            Data <- merge(ModelParam, UnitParam, by = IDQual, all.x = TRUE)
            Data <- merge(Data, IDEdit.dt, by = c(IDQual, 'variable'), all.x = TRUE)
            Data <- merge(Data, ErrorMoments.dt, by = c(IDQual, 'variable'))

            # Representación gráfica
            out.graph <- ggplot(Data, aes(x = RawValues, y = PredValues)) +
              geom_errorbar(aes(ymin = PredValues - PredErrorSTD, ymax = PredValues + PredErrorSTD), width = .1) +
              geom_point(aes(size = QuantileSamplingWeight, colour = Flagged, shape = Flagged)) +
              scale_shape_discrete(solid = FALSE) +#values = Data$Shape) +
              scale_colour_brewer(palette = "Set1") +
              labs(size = expression(c[omega])) +
              xlab('Raw values') +
              ylab('Predicted values') +
              geom_abline(intercept = 0, slope = 1) +
              geom_blank(data = Data, aes(x = RawValues, y = PredValues)) +
              facet_wrap( ~ variable, ncol = nVar, scales = 'free') +
              theme(plot.title = element_text(size = rel(1.5), lineheight = 1.5, face = 'bold', colour = 'black'),
                    panel.background = element_rect(fill = "white"))

            Data <- Data[Flagged == 'Selected']
            if (dim(Data)[1] == 0) return(out.graph)
            Data <- Data[, c(IDQual, DomainNames, 'IDEdit', 'QuantileErrorMoment', 'QuantileSamplingWeight', 'QuantileGlobalScore', 'RawValues', 'PredValues'), with = FALSE]
            Data <- as.data.frame(Data)
            names(Data) <- c(IDQual, DomainNames, 'IDEdit', expression(c[M[kk]]), expression(c[omega[k]]), expression(c[S[k]]), 'RawValues', 'PredValues')
            tblData <- gridExtra::tableGrob(Data, rows = NULL, theme = gridExtra::ttheme_default(base_size = 9, colhead = list(fg_params = list(parse=TRUE))))
            gridExtra::grid.arrange(out.graph, tblData,
                                    nrow = 2,
                                    as.table = TRUE,
                                    heights = c(3, 1))

            return(invisible(NULL))

          })
