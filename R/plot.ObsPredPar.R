#' @title Plot the main values in the optimization approach to selective editing for the specified
#' units and show information about it.
#'
#' @description This method overloads the plot function to draw a picture with the information about
#' the queried units in the optimization approach to selective editing.
#'
#' @param x Object of class \linkS4class{contObsPredModelParam}.
#'
#' @param y \code{Character} vector with the names of the variable names. These names must be the
#' names of the Edits in the FT argument.
#'
#' @param ... Arguments to be passed to methods, especially including
#'
#' \itemize{
#'
#'  \item \code{Query} as a \linkS4class{data.table} with the values of the identification
#'  variables of the queried statistical units;
#'
#'  \item \code{Ref} as a \code{Character} vector with the conditions that set the units with which
#'  to compare
#'
#'  \item \code{FT} as a \linkS4class{StQ} object with the values of the selected units in the
#'  optimization approach to selective editing;
#'
#'  \item \code{ErrorMoments} as a \linkS4class{ErrorMoments} object with the error moments
#'  corresponding to queried units;
#'}
#'
#' @return Returns an invisible \code{NULL} plotting the one scatterplot per variable and a
#' \linkS4class{data.table} with information about the results of selective editing.
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
            sigmaNames <- VarRoles$obsErrorSTD
            errorProbNames <- VarRoles$ErrorProb
            predValuesNames <- VarRoles$PredValues
            nuNames <- VarRoles$PredErrorSTD
            wNames <- VarRoles$DesignW

            if (length(names(y)) == 0)stop('[contObsPredModelParam::plot] Parameter y must be a named character vector.\n')
            nVar <- length(y)
            if (nVar > 4) stop('[contObsPredModelParam::plot] At most 4 variables can be plotted.\n')

            if (!all(y %in% VarNames)) stop('[contObsPredModelParam::plot] Some variables of y are not in the input object.\n')
            VarNames <- y
            names.list <- list(sigmaNames = sigmaNames, errorProbNames = errorProbNames,
                               predValuesNames = predValuesNames, nuNames = nuNames, wNames = wNames)

            namesQueried <- lapply(names.list, function(names){

              out <- list()
              for (Var in VarNames){

                out[[Var]] <- names[grep(paste0(Var, '$'), names)]
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
            if (!'Ref' %in% names(ExtraPar)) stop('[contObsPredModelParam::plot] Ref is a compulsory parameter.\n')
            if (!'FT' %in% names(ExtraPar)) stop('[contObsPredModelParam::plot] FT is a compulsory parameter.\n')
            if (!'ErrorMoments' %in% names(ExtraPar)) stop('[contObsPredModelParam::plot] ErrorMoments is a compulsory parameter.\n')

            if (!all(IDQual %in% names(ExtraPar$Query))) stop('[contObsPredModelParam::plot] Query does not have correct names.\n')
            if (!all(y %in% ExtraPar$ErrorMoments@VarNames)) stop('[contObsPredModelParam::plot] Some variables of y are not in the ErrorMoments parameter.\n')

            RefVars <- deparse(Ref)
            RefVars <- gsub('\"', '', RefVars)
            RefVars <- gsub('-', 'DasH', RefVars)
            RefVars <- gsub('.', 'DoT', RefVars, fixed = TRUE)
            RefVars <- gsub(':', 'CoLoN', RefVars)

            RefVars <- c(all.vars(parse(text = RefVars)), by)
            RefVars <- gsub('DoT', '.', RefVars)
            RefVars <- gsub('CoLoN', ':', RefVars)
            RefVars <- gsub('DasH', '-', RefVars)
            RefVars <- RefVars[1:length(Ref)]

            ObsPredPar.dm <- dcast_StQ(getData(x))[, unique(c(unlist(VarRoles), RefVars)), with = FALSE]
            setkeyv(ExtraPar$Query, IDQual)
            setkeyv(ObsPredPar.dm, IDQual)
            for (cond in Ref) {

              ObsPredPar.dm <- ObsPredPar.dm[eval(parse(text = cond))]
            }

            if (dim(merge(ObsPredPar.dm, Query))[1] == 0) stop('[contObsPredModelParam::plot] Some units of Query are not in the set of data fixed by the Ref parameter.\n')
            ModelParam <- ObsPredPar.dm[, c(IDQual, VarNames, wNames, predValuesNames, nuNames, sigmaNames), with = FALSE]


            # data.table con los cuantiles de los pesos de muestreo
            SamplingWeight.aux <- ModelParam[, c(IDQual, wNames), with = FALSE]
            SamplingWeight <- lapply(wNames, function(Var){

              QuantileSamplingWeight <- ecdf(SamplingWeight.aux[[Var]])(SamplingWeight.aux[[Var]])
              out <- cbind(SamplingWeight.aux[, IDQual, with = FALSE], QuantileSamplingWeight)
              out[, variable := strsplit(Var, 'DesignW')[[1]][2]]
              return(out)

            })
            SamplingWeight.dt <- rbindlist(SamplingWeight)


            # data.tables con los valores RawVar, PredValues, PredErrorSTD y ObsErrorSTD
            ModelParam.melt <- melt(ModelParam, id.vars = IDQual)[, c(IDQual, 'variable', 'value'), with = FALSE]
            RawVar <- ModelParam.melt[variable %in% VarNames]
            setnames(RawVar, 'value', 'RawValues')
            PredValues <- ModelParam.melt[variable %in% predValuesNames]
            PredValues <- lapply(VarNames, function(Var){

              rows <- grep(paste0(Var, '$'), PredValues[['variable']])
              out <- PredValues[rows][, variable := Var]
              return(out)

            })
            PredValues <- rbindlist(PredValues)
            setnames(PredValues, 'value', 'PredValues')
            PredErrorSTD <- ModelParam.melt[variable %in% nuNames]
            PredErrorSTD <- lapply(VarNames, function(Var){

              rows <- grep(paste0(Var, '$'), PredErrorSTD[['variable']])
              out <- PredErrorSTD[rows][, variable := Var]
              return(out)

            })
            PredErrorSTD <- rbindlist(PredErrorSTD)
            setnames(PredErrorSTD, 'value', 'PredErrorSTD')
            ObsErrorSTD <- ModelParam.melt[variable %in% sigmaNames]
            ObsErrorSTD <- lapply(VarNames, function(Var){

              rows <- grep(paste0(Var, '$'), ObsErrorSTD[['variable']])
              out <- ObsErrorSTD[rows][, variable := Var]
              return(out)

            })
            ObsErrorSTD <- rbindlist(ObsErrorSTD)
            setnames(ObsErrorSTD, 'value', 'ObsErrorSTD')


            # Obtenemos el data.table ModelParam que contiene todos los valores de los data.tables anteriores
            ModelParam.melt <- list(SamplingWeight.dt, RawVar, PredValues, PredErrorSTD, ObsErrorSTD)
            ModelParam <- Reduce(merge, ModelParam.melt)


            # Añadimos a ModelParam las columnas Flagged y Queried
            FT <- ExtraPar$FT
            SelectedUnits <- getData(FT)[IDEdit %in% names(VarNames)][, c(IDQual, 'IDEdit'), with = FALSE]

            if (dim(SelectedUnits)[1] == 0) stop('[contObsPredModelParam::plot] In the FT parameter there are no Edits with the names of the y parameter.\n')

            setkeyv(SelectedUnits, c(IDQual, 'IDEdit'))
            SelectedUnits <- SelectedUnits[!duplicated(SelectedUnits)]
            SelectedUnits[, Selected := paste0(get(IDQual), get('IDEdit'))]
            Selected <- SelectedUnits[['Selected']]
            ModelParam[, Key := variable]
            for (Var in seq(1:length(VarNames))) {

              ModelParam[, Key := gsub(VarNames[Var], names(VarNames[Var]), ModelParam[['Key']])]
            }
            ModelParam[, Key := paste0(get(IDQual), get('Key'))]
            ModelParam[, Flagged := ifelse(Key %in% Selected, 'Selected', 'Not Selected')]
            ModelParam[, Key := NULL]
            ModelParam[, Queried := ifelse(get(IDQual) %in% ExtraPar$Query[[IDQual]], 'Queried', 'Not Queried')]


            # data.table CuantGlob.dt con los valores QuantileGlobalScore de las unidades seleccionadas en el FT
            DD <- getDD(FT)
            FT.dt <- dcast_StQ(FT)
            # FT.dt <- FT.dt[ExtraPar$Query]
            CuantGlob <- UnitToIDDDNames('CuantGlob', DD)
            CuantGlob.dt <- FT.dt[, c(IDQual, CuantGlob), with = FALSE]
            setnames(CuantGlob.dt, CuantGlob, 'CuantGlob')
            CuantGlob.dt <- CuantGlob.dt[!is.na(CuantGlob.dt[['CuantGlob']])]
            setkeyv(CuantGlob.dt, IDQual)


            # data.table ErrorMoments.dt con los valores QuantileErrorMoment
            ErrorMoments <- ExtraPar$ErrorMoments
            Units.list <- ErrorMoments@Units
            Units <- rbindlist(Units.list)
            setkeyv(Units, IDQual)
            Units <- Units[ModelParam[, IDQual, with = FALSE]]
            Units <- Units[!duplicated(Units)]

            ErrorMoments.dt <- lapply(seq(along = VarNames), function(Var){

              numVar <- which(ErrorMoments@VarNames == VarNames[Var])
              output <- lapply(seq(1:dim(Units)[1]), function(i){

                cell <- Units[['cell']][i]
                Units.cell <- ErrorMoments@Units[[cell]]
                Units.cell[, Position := seq(1:dim(Units.cell)[1])]
                Unitposit <- merge(Units[i, IDQual, with = FALSE], Units.cell)[['Position']]
                numUnits <- dim(Units.cell)[1]
                init <- numUnits * (numVar - 1) + 1
                fin <- numUnits * numVar
                ErrorMoments.cell <- ErrorMoments@Moments[[cell]]$v
                moments <- ecdf(ErrorMoments.cell[init:fin])(ErrorMoments.cell[init:fin])
                out <- data.table(QuantileErrorMoment = moments[Unitposit], variable = VarNames[Var])
                out <- cbind(Units[i, IDQual, with = FALSE], out)
                return(out)
              })
              output <- rbindlist(output)
              return(output)
            })
            ErrorMoments.dt <- rbindlist(ErrorMoments.dt)

            # Unimos los data.table ModelParam, CuantGlob.dt y ErrorMoments.dt para obtener el data.table final Data
            Data <- merge(ModelParam, CuantGlob.dt, by = IDQual, all.x = TRUE)
            Data <- merge(Data, ErrorMoments.dt, by = c(IDQual, 'variable'))

            # Representación gráfica
            out.graph <- ggplot(Data, aes(x = RawValues, y = PredValues)) +
              geom_errorbar(aes(ymin = PredValues - PredErrorSTD, ymax = PredValues + PredErrorSTD), width = .1) +
              geom_point(aes(size = QuantileSamplingWeight, colour = Flagged, shape = Queried)) +
              scale_shape_discrete(solid = FALSE) +
              scale_colour_brewer(palette = "Set1") +
              labs(size = expression(c[omega])) +
              xlab('Raw values') +
              ylab('Predicted values') +
              geom_abline(intercept = 0, slope = 1) +
              geom_blank(data = Data, aes(x = RawValues, y = PredValues)) +
              facet_wrap( ~ variable, ncol = nVar, scales = 'free') +
              theme(plot.title = element_text(size = rel(1.5), lineheight = 1.5, face = 'bold', colour = 'black'),
                    panel.background = element_rect(fill = "white"))

            setcolorder(Data, c(IDQual, 'variable', 'RawValues', 'PredValues', 'PredErrorSTD', 'ObsErrorSTD', 'QuantileSamplingWeight',
                                'QuantileErrorMoment', 'CuantGlob', 'Flagged', 'Queried'))
            # Data <- as.data.frame(Data)
            # names(Data) <- c(IDQual, 'variable', 'RawValues', 'PredValues', 'PredErrorSTD', 'ObsErrorSTD', expression(c[omega[k]]),
            #                  expression(c[M[kk]]), expression(c[S[k]], 'Flagged', 'Queried'))

            print(out.graph)
            return(Data)

          })
