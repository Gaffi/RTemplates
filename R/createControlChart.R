#' Create Control Charts
#'
#' Reusable code to quickly create and customize Minitab-like control charts.
#' @param data A \code{\link{data.table}} (or an object that can be coerced into a \code{data.table}) of the data you want to chart.
#' @param obs A character string of the column identifying observations (often dates).
#' @param vals A character string of the column identifying the data points to plot.
#' @param stages A character string of the column identifying the stages or groups of the data points. If NA, no stages are used.
#' @param useSD A logical boolean (\code{TRUE}/\code{FALSE}). See details for usage.
#' @param specLimit A one- or two-element vector of numeric values specifying specification limits or targets.
#' @param xAxisLabel A character string of the text to use as the x axis label.
#' @param yAxisLabel A character string of the text to use as the y axis label.
#' @param chartTitle A character string of the text to use as the title of the chart.
#' @param dataFormat A character string format as used by \code{\link{simpleFormat}()}.
#' @param labelAllPoints A logical boolean (\code{TRUE}/\code{FALSE}). If \code{TRUE}, will add labels to every data point on the chart.
#' @param dataLabelSize A numeric value specifying the size of data point labels. Ignored when \code{labelAllPoints} is \code{FALSE}.
#' @param marginScale A numeric value specifying how much room should be given on the right-hand side of the chart for labels.
#' @param flagStageVariance A logical boolean (\code{TRUE}/\code{FALSE}) stating whether statistically significant changes between stages should be colored differently. Defaults to \code{TRUE} if \code{stages} is specified, otherwise \code{FALSE} (\code{!is.na(stages)}).
#' @param flagStageType A character string denoting the type of stage flagging to set.
#' @param flagSpecLimit A character string denoting the type of specification limit flagging to set.
#' @param maxUCL A numeric value specifying the maximum limit to restrict the control limit range to.
#' @param minLCL A numeric value specifying the minimum limit to restrict the control limit range to.
#'
#' @details
#' \code{data} may include extra, unneeded columns. \code{createControlChart()} will try to coerce data to a \code{data.table} if not provided in this format.
#'
#' When \code{useSD} is \code{TRUE}, function will use 3 standard deviations from the mean as the UCL/LCL. If \code{FALSE}, will use 2.66 * the average of the moving range from the mean (as is the default in Minitab).
#'
#' When \code{xAxisLabel} or \code{yAxisLabel} is NA (default), the field name from the data will be used as the axis title. If an empty string (\code{''})is passed, the axis label is completely removed.
#'
#' The range for \code{marginScale} is between 2 and 10, with 2 being the largest size and 10 being the smallest. Values outside this range will not create any usable or observable difference.
#'
#' When \code{flagStageVariance} is \code{TRUE} (default when \code{stages} is supplied), \code{flagStageType} must be supplied (defaults to \code{'all'}). Value \code{'all'} will compare differences between all stages to check for long-term trending, \code{'latest'} will only check the last two stages for short-term trending. Charts with no significant variation between the specified stages will display the control limit range as green in color, while those with significant variance will turn blue.
#'
#' When used, \code{flagSpecLimit} will change the data point color from black (within specifications) to red if the data point lies outside the given acceptable range. The value given should be where data points should lie - what are the acceptable ranges? Valid options are:
#' \itemize{
#' \item{'none' - No flagging should occur (specification limit is likely for reference only).}
#' \item{'above' - Valid points fall above the given \code{specLimit}.}
#' \item{'below' - Valid points fall below the given \code{specLimit}.}
#' \item{'inside' - Valid points fall inside the range of the upper and lower \code{specLimit}s, provided two are supplied.}
#' \item{'outside' - Valid points fall outside the range of the upper and lower \code{specLimit}s, provided two are supplied.}
#' }
#'
#' Parameters \code{maxUCL} and \code{minLCL} specify a limit (if any) to practical values to clamp calulcated control limits to. e.g. When reviewing percetnage metrics, it may not be possible to achieve over 100%, but the raw calculation may exceed that amount. Setting \code{maxUCL} will prevent the upper control limit from moving beyond 100%.
#'
#' @seealso See \code{\link{simpleFormat}} for format options, \code{\link{TemplateHelp}} for other tools.
#'
#' @keywords graphs device methods
#' @return A ggplot2 object that can be printed or added to a file/image output.
#' Can also be manipulated as a normal ggplot object can by adding new layers (though this is likely unnecessary and cumbersome).
#' e.g. createControlChart + geom_line(ggplot2::aes(label = someData))
#' @export
#' @examples
#' # Create a dataset for example purposes only.
#' my.dt <- data.table(
#'   observation = 1:60,
#'   measurement = c(
#'      rnorm(20)/20,
#'      rnorm(20)/20 + .15,
#'      rnorm(20)/20 - .25),
#'   stage = rep(LETTERS[1:3], each = 20))
#'
#' # Create a control chart using only the base required parameters.
#' createControlChart(my.dt, 'observation', 'measurement')
#'
#' # Create same control chart with upper and lower spec limits as
#' # a reference only (no flagging) and special axis/chart labels.
#' createControlChart(my.dt, 'observation', 'measurement',
#'                    dataFormat = 'Round2', specLimit=c(0, .1),
#'                    xAxisLabel = 'Date of Occurrence',
#'                    yAxisLabel = 'Recording Variation',
#'                    chartTitle = 'Variation over Time')
#'
#' # Create the control chart using the alternative SD calculation.
#' createControlChart(my.dt, 'observation', 'measurement',
#'                    dataFormat = 'Round2', useSD = TRUE)
#'
#' # Create the chart with stages, formatted as %.
#' createControlChart(my.dt, 'observation', 'measurement',
#'                    'stage', dataFormat = 'Percent2')
#'
#' # Create the chart with stages, formatted as % and
#' # specification limit flagging.
#' createControlChart(my.dt, 'observation', 'measurement',
#'                    'stage', dataFormat = 'Percent2',
#'                    specLimit = 0.15,
#'                    flagSpecLimit = 'below')
#'
#'


# Reusable function makes standard control chart
createControlChart <-
  function(data,
           obs,
           vals,
           stages = NA,
           useSD = FALSE,
           specLimit = NA,
           xAxisLabel = NA,
           yAxisLabel = NA,
           chartTitle = NA,
           dataFormat = 'Round2',
           labelAllPoints = FALSE,
           dataLabelSize = 3.25,
           marginScale = 10,
           flagStageVariance = !is.na(stages),
           flagStageType = 'all',
           flagSpecLimit = 'none',
           maxUCL = NA,
           minLCL = NA) {
    if (missing(data))
      stop('Must supply data to chart.')
    if (missing(obs))
      stop('Must supply observation column (name) to chart.')
    if (missing(vals))
      stop('Must supply value column (name) to chart.')
    
    stopifnot(
      is.logical(useSD),
      is.character(obs),
      is.character(vals),
      is.character(dataFormat),
      (marginScale >= 2 & marginScale <= 10)
    )
    
    if (!is.na(stages)) {
      stopifnot(is.character(stages))
      stopifnot(stages %in% names(data))
    }
    
    if (labelAllPoints) {
      stopifnot(is.numeric(dataLabelSize))
    }
    
    if (flagStageVariance) {
      if (!(flagStageType %in% c('all', 'latest'))) {
        stop(
          'When flagStageVariance is used,',
          'flagStageType must be either "all" or "latest".'
        )
      }
    }
    
    if (!(flagSpecLimit %in% c('none', 'above', 'below', 'inside', 'outside'))) {
      stop(
        'Incorrect option provided for flagSpecLimit.',
        'Must be one of "none", "above", "below", "inside", or "outside".'
      )
    }
    
    if (length(specLimit) <= 1) {
      if (is.na(specLimit)) {
        if (flagSpecLimit %in% c('above', 'below', 'inside', 'outside')) {
          warnPref <- if (!is.na(chartTitle)) {
            paste0('Warning for "', chartTitle, '": ')
          } else {
            'Warning: '
          }
          warning(
            warnPref,
            'Cannot apply flagSpecLimit if no specLimit supplied. Ignoring setting.'
          )
          flagSpecLimit <- 'none'
        }
      }
    } else {
      if (length(specLimit) == 1 &
          !(flagSpecLimit %in% c('none', 'above', 'below'))) {
        warning(
          'Parameter flagSpecLimit must be "none"(ignored),',
          '"above" or "below" if only a single specLimit ',
          'is supplied. Ignoring setting.'
        )
        flagSpecLimit <- 'none'
      }
    }
    
    if (NA %in% match(c(obs, vals), names(data))) {
      stop('Unable to find observation or values field based on names provided.')
    }
    if (!('data.table' %in% class(data))) {
      data <- data.table(data)
    }
    obsClass <- class(data[[1, match(obs, names(data))]])
    if (obsClass == 'factor') {
      stop('Observation class must be neither "factor" nor "character"')
    }
    if ((!is.na(maxUCL) & !is.numeric(maxUCL)) |
        (!is.na(minLCL) & !is.numeric(minLCL))) {
      stop('Control limit restrictions must be numeric values.')
    }
    if (is.na(stages)) {
      # Reduce data to columns used for chart.
      data <- data[, match(c(obs, vals), names(data)), with = FALSE]
      # Remove missing/NA values
      data <- data[complete.cases(data),]
      data[, ccMean := mean(unlist(data[, 2]))]
      data[, ccSD := ifelse(useSD,
                            sd(unlist(data[, 2])) * 3,
                            mean(abs(diff(unlist(
                              data[, 2]
                            )))) * 2.66)]
      data <-
        plyr::mutate(data, ccLCL = ccMean - ccSD, ccUCL = ccMean + ccSD)
      stageTests <- FALSE
    } else {
      # Reduce data to columns used for chart.
      data <-
        data[, match(c(obs, vals, stages), names(data)), with = FALSE]
      data <- data[complete.cases(data),]
      data[, StageN := .N, by = stages]
      meanSummary <-
        data.table(aggregate(data[, 2], FUN = mean, by = data[, 3]))
      names(meanSummary) <- c(stages, 'ccMean')
      if (useSD) {
        sdSummary <-
          data.table(aggregate(data[, 2], FUN = sd, by = data[, 3]))
      } else {
        sdSummary <- data.table(aggregate(
          data[, 2],
          FUN = function(x) {
            c(ModVal = mean(abs(unlist(diff(
              x
            )))))
          },
          by = data[, 3]
        ))
      }
      sdSummary[, 2] <- sdSummary[, 2] * ifelse(useSD, 3, 2.66)
      names(sdSummary) <- c(stages, 'ccSD')
      data <- plyr::join(data, meanSummary, by = stages)
      data <- plyr::join(data, sdSummary, by = stages)
      data[is.nan(ccSD), ccSD := 0]
      data <-
        plyr::mutate(data, ccLCL = ccMean - ccSD, ccUCL = ccMean + ccSD)
      data <- copy(data)
      # Dynamically find unique combinations of consecutive stages.
      # There is guaranteed to be numerous better (simpler code,
      # less CPU intesive, etc.) ways to do this, but this is how
      # my brain did it for now.
      fullStages <- as.factor(unname(unlist(unique(data[, 3]))))
      eg <- as.data.table(expand.grid(fullStages, fullStages))
      eg <- eg[as.integer(Var2) == as.integer(Var1) + 1,]
      # We really should be doing an Anderson-Darling test for
      # normality, but sample sizes may be too small, so we'll
      # skip and hope for the best. This is not good planning!
      # TODO: Check for size, skip if too small?
      minSize <- 3
      if (flagStageVariance) {
        if (flagStageType == 'all') {
          if (nrow(eg) >= minSize) {
            stageTests <- any(unlist(lapply(1:nrow(eg),
                                            function(x) {
                                              t.test(data[unlist(data[, 3]) == unlist(eg[x, 1]), 2],
                                                     data[unlist(data[, 3]) == unlist(eg[x, 2]), 2])$p.value < 0.05
                                            })))
          } else {
            stageTests <- FALSE
          }
        } else {
          if (nrow(eg) >= minSize) {
            # flagStageType equals 'latest'
            stageTests <-
              t.test(data[unlist(data[, 3]) == unlist(eg[nrow(eg), 1]), 2],
                     data[unlist(data[, 3]) == unlist(eg[nrow(eg), 2]), 2])$p.value < 0.05
          }
          else {
            stageTests <- FALSE
          }
        }
      } else {
        stageTests <- FALSE
      }
    }
    controlLimitLineColor <-
      if (flagStageVariance & stageTests) {
        '#0055aa' # Blue
      } else {
        '#00aa00' # Green
      }
    controlLimitFillColor <-
      if (flagStageVariance & stageTests) {
        '#aaccff' # Lighter Blue
      } else {
        '#aaffaa' # Lighter Green
      }
    colorScale <- c(
      'Mean' = '#0000aa',
      # Dark Blue
      'ccRed' = '#ff0000',
      # Red
      'Good' = '#000000',
      # Black
      'NonSpec' = '#dd0000',
      # Lighter Red
      'NonControl' = '#ffff00',
      # Yellow
      'SpecLimitLine' = '#aa00aa',
      # Purple
      'SpecLimitFill' = '#ffccff',
      # Lighter Purple
      'ControlLimitFill' = controlLimitFillColor,
      'ControlLimitLine' = controlLimitLineColor
    )
    obsMin <- min(unlist(data[, obs, with = FALSE]))
    obsMax <- max(unlist(data[, obs, with = FALSE]))
    obsSegment <- (obsMax - obsMin) / marginScale
    if (obsClass == 'Date') {
      obsMin <- as.Date.numeric(obsMin, origin = '1970-01-01')
      obsMax <- as.Date.numeric(obsMax, origin = '1970-01-01')
    }
    data <- copy(data)
    data[, ErrPts := 'Good']
    checks <-
      which(data[, 2] > data[, ccUCL] | data[, 2] < data[, ccLCL])
    data[checks, ErrPts := 'NonControl']
    if (flagSpecLimit != 'none') {
      if (flagSpecLimit == 'above') {
        checks <- which(data[, 2] < specLimit)
      } else if (flagSpecLimit == 'below') {
        checks <- which(data[, 2] > specLimit)
      } else if (flagSpecLimit == 'inside') {
        checks <-
          which(data[, 2] < min(specLimit) | data[, 2] > max(specLimit))
      } else if (flagSpecLimit == 'outside') {
        checks <-
          which(data[, 2] < max(specLimit) & data[, 2] > min(specLimit))
      }
      data[checks, ErrPts := 'NonSpec']
    }
    if (!is.na(maxUCL)) {
      data[ccUCL > maxUCL, ccUCL := maxUCL]
    }
    if (!is.na(minLCL)) {
      data[ccLCL < minLCL, ccLCL := minLCL]
    }
    dataRange <- range(data[, 2], data$ccUCL, data$ccLCL)
    yLLim <- dataRange[1] - ((dataRange[2] - dataRange[1]) / 10)
    yULim <- dataRange[2] + ((dataRange[2] - dataRange[1]) / 10)
    ggp <- ggplot2::ggplot(data,
                           ggplot2::aes_string(x = obs, y = vals)) +
      # Upper Control Limit Line/Label
      ggplot2::geom_line(ggplot2::aes(y = ccUCL, color = 'ControlLimitLine')) +
      ggplot2::annotation_custom(
        grob = grid::textGrob(
          label = paste0('UCL: ',
                         simpleFormat(data[data[, .N], ccUCL],
                                      dataFormat)),
          gp = grid::gpar(
            cex = dataLabelSize,
            fontsize = 3,
            col = controlLimitLineColor
          )
        ),
        ymin = yULim,
        ymax = yULim,
        xmin = obsMax + obsSegment,
        xmax = obsMax + obsSegment
      ) +
      # Lower Control Limit Line/Label
      ggplot2::geom_line(ggplot2::aes(y = ccLCL, color = 'ControlLimitLine')) +
      ggplot2::annotation_custom(
        grob = grid::textGrob(
          label = paste0('LCL: ',
                         simpleFormat(data[data[, .N], ccLCL],
                                      dataFormat)),
          gp = grid::gpar(
            cex = dataLabelSize,
            fontsize = 3,
            col = controlLimitLineColor
          )
        ),
        ymin = yLLim,
        ymax = yLLim,
        xmin = obsMax + obsSegment,
        xmax = obsMax + obsSegment
      ) +
      # Control Limit Area
      ggplot2::geom_ribbon(ggplot2::aes(ymax = ccUCL,
                                        ymin = ccLCL),
                           # Color does not come from ggplot's color scaling
                           fill = colorScale['ControlLimitFill'],
                           alpha = 0.33) +
      # Mean Line/Labels
      ggplot2::geom_line(ggplot2::aes(y = ccMean, color = 'Mean')) +
      ggplot2::annotation_custom(
        grob = grid::textGrob(
          paste0('M: ', simpleFormat(data[data[, .N], ccMean], dataFormat)),
          gp = grid::gpar(
            cex = dataLabelSize,
            fontsize = 3,
            col = colorScale['Mean']
          )
        ),
        ymin = data[data[, .N], ccMean],
        ymax = data[data[, .N], ccMean],
        xmin = obsMax + obsSegment,
        xmax = obsMax + obsSegment
      ) +
      # Colors
      ggplot2::guides(colour = FALSE) +
      ggplot2::scale_color_manual(values = colorScale) +
      # X-Axis Scale/Theme
      ggplot2::xlim(c(obsMin, obsMax + (1.75 * obsSegment))) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
    if (!is.na(specLimit[1])) {
      ggp <- ggp + ggplot2::geom_line(ggplot2::aes(
        y = specLimit[1],
        color = 'SpecLimitLine',
        group = 1
      ),
      linetype = 'dashed')
      if (length(specLimit) > 1) {
        ggp <- ggp + ggplot2::geom_line(ggplot2::aes(
          y = specLimit[2],
          color = 'SpecLimitLine',
          group = 1
        ),
        linetype = 'dashed') +
          ggplot2::geom_ribbon(ggplot2::aes(
            ymax = max(specLimit),
            ymin = min(specLimit)
          ),
          # Color does not come from ggplot's color_scale_*
          fill = colorScale['SpecLimitFill'],
          alpha = 0.5) +
          ggplot2::annotation_custom(
            grob = grid::textGrob(
              label = paste0(
                'Spec: ',
                simpleFormat(min(specLimit), dataFormat),
                '  to  ',
                simpleFormat(max(specLimit), dataFormat)
              ),
              gp = grid::gpar(
                cex = dataLabelSize,
                fontsize = 3,
                col = colorScale['SpecLimitLine']
              )
            ),
            ymin = yULim,
            ymax = yULim,
            xmin = obsMin + obsSegment,
            xmax = obsMin + obsSegment
          )
      } else {
        ggp <- ggp +
          ggplot2::annotation_custom(
            grob = grid::textGrob(
              label = paste0('Spec: ', simpleFormat(specLimit[1], dataFormat)),
              gp = grid::gpar(
                cex = dataLabelSize,
                fontsize = 3,
                col = colorScale['SpecLimitLine']
              )
            ),
            ymin = yULim,
            ymax = yULim,
            xmin = obsMin + obsSegment,
            xmax = obsMin + obsSegment
          )
      }
    }
    if (!is.na(xAxisLabel)) {
      if (xAxisLabel == '') {
        ggp <- ggp + ggplot2::theme(axis.title.x = ggplot2::element_blank())
      } else {
        ggp <- ggp + ggplot2::xlab(xAxisLabel)
      }
    }
    if (is.na(yAxisLabel)) {
      ggp <- ggp + ggplot2::scale_y_continuous(
        labels = function(x) {
          simpleFormat(x, dataFormat)
        },
        limits = c(yLLim, yULim)
      )
    } else {
      ggp <- ggp + ggplot2::scale_y_continuous(
        name = yAxisLabel,
        labels = function(x) {
          simpleFormat(x, dataFormat)
        },
        limits = c(yLLim, yULim)
      )
    }
    if (!is.na(chartTitle)) {
      ggp <- ggp + ggplot2::ggtitle(chartTitle)
    }
    if (labelAllPoints) {
      ggp <- ggp + ggplot2::geom_text(
        label = unlist(simpleFormat(unlist(unname(
          data[, 2]
        )), dataFormat)),
        angle = 20,
        hjust = 0,
        ggplot2::aes(color = ErrPts)
      )
    }
    ggp <- ggp +
      # Data Line/Points (done last so other elements don't overwrite)
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(color = ErrPts))
    return(ggp)
  }
