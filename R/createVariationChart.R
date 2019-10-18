#' Create Variation Charts
#'
#' Reusable code to quickly create customizable charts showing variation from a specified value.
#' @param data A \code{\link{data.table}} of the data you want to chart.
#' @param obs A character string of the column identifying observations (often dates).
#' @param vals A character string of the column identifying the data points to plot.
#' @param stages A character string of the column identifying the stages or groups of the data points. If NA, no stages are used. Defaults to NA.
#' @param variationTarget A character string specifying the value to measure variation from. Either the 'mean', or the name of a column. Defaults to 'mean'.
#' @param xAxisLabel A character string of the text to use as the x axis label. Defaults to NA.
#' @param yAxisLabel A character string of the text to use as the y axis label. Defaults to NA.
#' @param chartTitle A character string of the text to use as the title of the chart. Defaults to NA.
#' @param dataFormat A character string format as used by \code{\link{simpleFormat}()}. Defaults to \code{'Round2'}.
#' @param labelAllPoints A logical boolean (\code{TRUE}/\code{FALSE}). If \code{TRUE}, will add labels to every data point on the chart. Defaults to \code{FALSE} (no labels).
#' @param dataLabelSize A numeric value specifying the size of data point labels. Default to 3.25. Ignored when \code{labelAllPoints} is \code{FALSE}
#' @param pos.good A logical boolean (\code{TRUE}/\code{FALSE}) stating whether positive values should be marked green (good/\code{TRUE}) or red (not good/\code{FALSE}). Defaults to \code{TRUE}
#'
#' @details
#' \code{data} may include extra, unneeded columns. \code{createControlChart()} will try to coerce data to a \code{data.table} if not provided in this format.
#'
#' Stages are only relevant when using \code{'mean'} (default) as the \code{variationTarget}. If this is not the value set, \code{stages} will be ignored. 
#'
#' When \code{xAxisLabel} or \code{yAxisLabel} is NA (default), the field name from the data will be used as the axis title. If an empty string (\code{''})is passed, the axis label is completely removed.
#'
#' @seealso See \code{\link{simpleFormat}} for format options, \code{\link{TemplateHelp}} for other tools.
#'
#' @keywords graphs device methods
#' @return A ggplot2 object that can be printed or added to a file/image output.
#' Can also be manipulated as a normal ggplot object can by adding new layers (though this is likely unnecessary and cumbersome).
#' e.g. createVariationChart + geom_line(aes(label = somedata))
#' @export
#' @examples
#'
#' ## Creating a basic variation chart, formatting the values
#'
#' # Create a dataset for example purposes only.
#' my.dt<-data.table(date=seq(as.Date('2017-01-01'),as.Date('2017-04-10'),1), level=rnorm(100)/2.5)
#' # Alternatively: seq(as.Date('2017-01-01'),by=1,length.out=100)
#' my.dt[1,2] <- 3.5
#' my.dt[32,2] <- 2.75
#' my.dt[77,2] <- -3.8
#' my.dt[78,2] <- -3.9
#'
#' # Create the variation chart/run the function
#' createVariationChart(my.dt, 'date', 'level', dataFormat = 'Round2')
#' # Create same variation chart with special axis/chart labels
#' createVariationChart(my.dt, 'date', 'level', dataFormat = 'Round2',
#'                    xAxisLabel = 'Date of Occurrence', yAxisLabel = 'Recording Variation',
#'                    chartTitle = 'Variation over Time')
#'
#' ## Create variation chart with stages and data labels
#'
#' # Create a dataset for example purposes only.
#' my.dt<-data.table(observation=1:20, measurement=rnorm(20)/10, target=0.5, stage=rep(1:2,each=10))
#' my.dt[8,2] <- 0.9535
#' my.dt[17,2] <- -0.38975
#' my.dt[18,2] <- -0.39716
#'
#' # Create the variation chart/run the function with decimal percentages
#' createVariationChart(my.dt, 'observation', 'measurement', 'stage',
#'                    labelAllPoints = TRUE, dataLabelSize = 3, dataFormat = 'Percent2')
#' # Create the same variation chart with whole percentages
#' createVariationChart(my.dt, 'observation', 'measurement', 'stage'
#'                    labelAllPoints = TRUE, dataLabelSize = 4, dataFormat = 'Percent0')
#'


# Reusable function makes standard control chart
createVariationChart <- function(data,
                                 obs,
                                 vals,
                                 stages = NA,
                                 variationTarget = 'mean',
                                 xAxisLabel = NA,
                                 yAxisLabel = NA,
                                 chartTitle = NA,
                                 dataFormat = 'Round2',
                                 labelAllPoints = FALSE,
                                 dataLabelSize = 3.25,
                                 pos.good = TRUE) {
  if (missing(data))
    stop('Must supply data to chart.')
  if (missing(obs))
    stop('Must supply observation column (name) to chart.')
  if (missing(vals))
    stop('Must supply value column (name) to chart.')
  stopifnot(
    is.character(obs),
    is.character(vals),
    (is.character(variationTarget) || is.numeric(variationTarget)),
    is.character(dataFormat)
  )
  if (is.character(variationTarget)) {
    if (variationTarget == 'mean') {
      if (!is.na(stages)) {
        stopifnot(is.character(stages))
        stopifnot(stages %in% names(data))
      }
    } else {
      stopifnot(variationTarget %in% names(data))
    }
  }
  if (labelAllPoints) {
    stopifnot(is.numeric(dataLabelSize))
  }
  if (NA %in% match(c(obs, vals), names(data))) stop('Unable to find observation or values field based on names provided.')
  if (!('data.table' %in% class(data))) {
    data <- data.table(data)
  }
  
  obsClass <- class(data[[1,match(obs,names(data))]])
  if (obsClass == 'factor') stop('Observation class must be neither "factor" nor "character"')
  
  
  if (is.numeric(variationTarget)) {
    # Reduce data to columns used for chart.
    data <- data[, match(c(obs, vals), names(data)), with = FALSE]
    
    # Remove missing/NA values
    data <- data[complete.cases(data), ]
    data[, vcMean := variationTarget]
  } else {
    if (is.na(stages) || variationTarget != 'mean') {
      ## NO STAGES
      # Reduce data to columns used for chart.
      
      if (variationTarget == 'mean') {
        data <- data[, match(c(obs, vals), names(data)), with = FALSE]
      } else {
        data <- data[, match(c(obs, vals, variationTarget), names(data)), with = FALSE]
      }
      
      # Remove missing/NA values
      data <- data[complete.cases(data), ]
      data[, vcMean := mean(unlist(data[, 2]))]
  
    } else {
      ## USING STAGES
      # Reduce data to columns used for chart.
      data <- data[, match(c(obs, vals, stages), names(data)), with = FALSE]
      
      data[, StageN := .N, by = stages]
      
      #meanSummary <-
      #  data.table(aggregate(data[, 2], FUN = mean, by = data[, 3]))
      #setnames(meanSummary, c(stages, 'vcMean'))
      #data <- plyr::join(data, meanSummary, by = stages)
      data[,vcMean:=data[.(stage = stage), on = .(stage = stage),mean(measurement),by = .EACHI]$V1]
      
      
    }
  }
  
  if (is.numeric(variationTarget) || variationTarget == 'mean') {
    data[, vcDiff := data[, 2] - data[, vcMean]]
  } else {
    data[, vcDiff := data[, 2] - data[, variationTarget, with=FALSE]]
  }
  data[, vcDiffPos := data[,vcDiff]>0]
  
  if (pos.good) {
    colorScale <- c(
      'TRUE' = '#99cc66',
      'FALSE' = '#cc6666'
    )
  } else {
    colorScale <- c(
      'TRUE' = '#cc6666',
      'FALSE' = '#99cc66'
    )
  }
  
  obsMin <- min(unlist(data[,obs,with=FALSE]))
  obsMax <- max(unlist(data[,obs,with=FALSE]))
  
  if (obsClass == 'Date') {
    obsMin <- as.Date.numeric(obsMin, origin='1970-01-01') 
    obsMax <- as.Date.numeric(obsMax, origin='1970-01-01') 
  }
  
  ggp <- ggplot2::ggplot(data, 
                         ggplot2::aes_string(x = obs, 
                                             y = 'vcDiff', 
                                             fill = 'vcDiffPos')) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::guides(fill = FALSE) +
    ggplot2::scale_fill_manual(values = colorScale) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  if (!is.na(xAxisLabel)) {
    if (xAxisLabel == '') {
      ggp <- ggp + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    } else {
      ggp <- ggp + ggplot2::xlab(xAxisLabel)
    }
  }
  if (!is.na(yAxisLabel)) {
    if (yAxisLabel == '') {
      ggp <- ggp + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    } else {
      ggp <- ggp + ggplot2::ylab(yAxisLabel)
    }
  } else {
    ggp <- ggp + ggplot2::ylab(paste0('Variation from ',variationTarget))
  }
  if (!is.na(chartTitle)) {
    ggp <- ggp + ggplot2::ggtitle(chartTitle)
  }
  if (is.na(dataFormat)) {
    ggp <-
      ggp + ggplot2::scale_y_continuous(
        labels = function(x) {
          unlist(simpleFormat(x, dataFormat))
        }
      )
  }
  if (labelAllPoints) {
    ggp <-
      ggp + ggplot2::geom_text(
        label = unlist(simpleFormat(unlist(unname(data[, 2])), dataFormat)),
        size = dataLabelSize
      )
  }
  
  return(ggp)
}

