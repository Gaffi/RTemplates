#' Create Control Charts in R
#' 
#' Reusable code to quickly create Minitab-like control charts in R.
#' @param data A data.table of the data you want to chart. This can include extra, unneeded columns.
#' 
#' This will try to coerce data to a data.table if a non-data.table is passed.
#' @param obs A character string of the column identifying observations (often dates).
#' @param vals A character string of the column identifying the data points to plot.
#' @param stages A character string of the column identifying the stages or groups of the data points. If NA, no stages are used. Defaults to NA.
#' @param useSD A logical boolean (TRUE/FALSE). If TRUE, will use 3 * calculated standard deviations from the mean as the UCL/LCL. If FALSE, will use 2.66 * the average of the moving range (as is the default in Minitab) from the mean. Defaults to FALSE.
#' @param specLimit A one or two element vector of values specifying an upper and lower spec limit. Defaults to NA.
#' @param xAxisLabel A character string of the text to use as the x axis label. Defaults to NA, in which case the field name will be used.
#' @param yAxisLabel A character string of the text to use as the y axis label. Defaults to NA, in which case the field name will be used.
#' @param chartTitle A character string of the text to use as the title of the chart. Defaults to NA.
#' @param dataFormat A character string of the simpleFormat to be used. (See ?simpleFormat) Defaults to 'Round2'.
#' 
#' @keywords graphs device methods
#' @return A ggplot2 object that can be printed or added to a file/image output. 
#' Can also be manipulated as a normal ggplot object can by adding new layers (though this is likely unnecessary and cumbersome).
#' e.g. createControlChart + geom_line(aes(label = somedata))
#' @export
#' @examples 
#' 
#' ## Creating a basic control chart
#' 
#' # Create a dataset for example purposes only.
#' my.dt<-data.table(date=seq(as.Date('2017-01-01'),as.Date('2017-04-10'),1), level=rnorm(100)/2.5)
#' # Alternatively: seq(as.Date('2017-01-01'),by=1,length.out=100)
#' my.dt[1,2] <- 3.5
#' my.dt[32,2] <- 2.75
#' my.dt[77,2] <- -3.8
#' my.dt[78,2] <- -3.9
#' 
#' # Create the control chart/run the function
#' createControlChart(my.dt,'date','level')
#' # Create same control chart with upper and lower spec limits
#' createControlChart(my.dt,'date','level',specLimit=c(1,-1))
#' 
#' ## Create control chart with stages
#' 
#' # Create a dataset for example purposes only.
#' my.dt<-data.table(observation=1:100, measurement=rnorm(100)/2.5, stage=rep(1:4,each=25))
#' my.dt[1,2] <- 3.5
#' my.dt[32,2] <- 2.75
#' my.dt[77,2] <- -3.8
#' my.dt[78,2] <- -3.9
#' 
#' # Create the control chart/run the function
#' createControlChart(my.dt,'observation','measurement','stage')
#' # Create the same control chart with a single spec limit
#' createControlChart(my.dt,'observation','measurement','stage',specLimit=1)
#' 


# Reusable function makes standard control chart
createControlChart <- function(data, obs, vals, stages = NA, useSD = FALSE, 
                               specLimit = NA, xAxisLabel = NA, yAxisLabel = NA, 
                               chartTitle = NA, dataFormat = 'Round2') {
  if (!'data.table' %in% class(data)) {
    data <- data.table(data)
  }
  obsColumn <- match(obs,names(data))
  obsIsDate <- class(data[[1,obsColumn]]) == 'Date'
  if (is.na(stages)) {
    # Reduce data to columns used for chart.
    data<-data[, match(c(obs,vals),names(data)), with = FALSE]
    # Remove missing/NA values
    data<-data[complete.cases(data),]
    data[,ccMean:=mean(unlist(data[,2]))]
    data[,ccSD:=ifelse(useSD,sd(unlist(data[,2]))*3,mean(abs(diff(unlist(data[,2]))))*2.66)]
    data<-mutate(data,ccLCL=ccMean-ccSD,ccUCL=ccMean+ccSD)
  } else {
    # Reduce data to columns used for chart.
    data<-data[, match(c(obs,vals,stages),names(data)), with = FALSE]
    data[,StageN:=.N,by=stages]
    #aggregate(data[,2], FUN=mean, by=data[,3])
    meanSummary <- data.table(aggregate(data[,2], FUN=mean, by=data[,3]))
    names(meanSummary)<-c(stages,'ccMean')
    if (useSD) {
      sdSummary <- data.table(aggregate(data[,2], FUN=sd, by=data[,3]))
    } else {
      sdSummary <- data.table(aggregate(data[,2], FUN= function(x) c(ModVal=mean(abs(unlist(diff(x))))), by=data[,3]))
    }
    sdSummary[,2]<-sdSummary[,2]*ifelse(useSD,3,2.66)
    names(sdSummary)<-c(stages,'ccSD')
    
    data<-join(data,meanSummary,by=stages)
    data<-join(data,sdSummary,by=stages)
    
    data<-mutate(data,ccLCL=ccMean-ccSD,ccUCL=ccMean+ccSD)
  }
  
  colorScale<- c(
      'Mean'='#0000dd',
      'ControlLimit'='#aa0000',
      'TRUE'='#dd0000',
      'FALSE'='#000000',
      'SpecLimit'='#aa00aa',
      'FillSpace'='#aaffaa')
  
  obsMin <- min(unlist(data[,obs,with=FALSE]))
  obsMax <- max(unlist(data[,obs,with=FALSE]))
  obsSegment <- (obsMax-obsMin)/10
  if (obsIsDate) {
    obsMin <- as.Date.numeric(obsMin, origin='1970-01-01') 
    obsMax <- as.Date.numeric(obsMax, origin='1970-01-01') 
  }
  
  data$ErrPts<-(data[,2] > data[,ccUCL] | data[,2] < data[,ccLCL])
  ggp <- ggplot(data, aes_string(x=obs, y=vals)) + geom_line() + 
    geom_line(aes(y=ccUCL, color='ControlLimit')) + 
    geom_line(aes(y=ccLCL, color='ControlLimit')) + 
    geom_ribbon(aes(
      ymax = ccUCL, 
      ymin = ccLCL), 
      fill = colorScale['FillSpace'], # Color does not come from ggplot's color_scale_manual
      alpha = 0.25) +
    geom_line(aes(y=ccMean, color='Mean')) +
    geom_point(aes(color=ErrPts)) + 
    guides(colour = FALSE) +
    scale_color_manual(values = colorScale) +
    xlim(c(obsMin,obsMax + (1.75 * obsSegment))) + 
    geom_text(data = NULL, size = 3, 
      aes( # Mean label
      x=obsMax + (obsSegment / 5),
      y=data[data[,.N],ccMean]), 
      hjust = 0,
      label=paste0('M: ', simpleFormat(data[data[,.N],ccMean],dataFormat)),
      color = colorScale['Mean']) + # Color does not come from ggplot's color_scale_manual
    geom_text(data = NULL, size = 3, 
      aes( # UCL label
      x=obsMax + (obsSegment / 5),
      y=data[data[,.N],ccUCL]),
      hjust = 0,
      label=paste0('UCL: ',simpleFormat(data[data[,.N],ccUCL],dataFormat)),
      color = colorScale['ControlLimit']) + # Color does not come from ggplot's color_scale_manual
    geom_text(data = NULL, size = 3, 
      aes( # LCL label
      x=obsMax + (obsSegment / 5),
      y=data[data[,.N],ccLCL]),
      hjust = 0,
      label=paste0('LCL: ',simpleFormat(data[data[,.N],ccLCL],dataFormat)),
      color = colorScale['ControlLimit']) + # Color does not come from ggplot's color_scale_manual
    theme(axis.text.x = element_text(angle=45, hjust=1))
  if(!is.na(specLimit[1])) {
    ggp <- ggp + geom_line(aes(y=specLimit[1], color='SpecLimit', group=1),linetype = 'dashed') + 
      geom_text(data = NULL, size = 3, aes( # Spec Limit 1 label
        x=obsMax + (obsSegment / 5),
        y=specLimit[1]),
        hjust = 0,
        label=paste0('Spec: ',simpleFormat(specLimit[1],dataFormat)),
        color = colorScale['SpecLimit']) # Color does not come from ggplot's color_scale_manual
    if(length(specLimit) > 1) {
      ggp <- ggp + geom_line(aes(y=specLimit[2], color='SpecLimit', group=1),linetype = 'dashed') + 
        geom_text(data = NULL, size = 3, aes( # Spec Limit 2 label
          x=obsMax + (obsSegment / 5),
          y=specLimit[2]),
          hjust = 0,
          
          label=paste0('Spec: ',simpleFormat(specLimit[2],dataFormat)),
          color = colorScale['SpecLimit']) # Color does not come from ggplot's color_scale_manual
    }
  }
  if(!is.na(xAxisLabel)) {
    ggp <- ggp + xlab(xAxisLabel)
  }
  if(!is.na(yAxisLabel)) {
    ggp <- ggp + ylab(yAxisLabel)
  }
  if(!is.na(chartTitle)) {
    ggp <- ggp + ggtitle(chartTitle)
  }
  if(is.na(dataFormat)) {
    ggp <- ggp + scale_y_continuous(label = function(x){simpleFormat(x,dataFormat)})
  }
  return(ggp)
}

