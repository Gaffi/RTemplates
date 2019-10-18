#' @title 
#' Create Excel-Like Pivot Tables
#' @description
#' Simplify the pivoting of data to print/display similiar to an Excel pivot.
#' 
#' @param rawData The formatted data to be plotted.
#' @param field.row A character string of the name of the field to use for the pivot row.
#' @param field.col A character string of the name of the field to use for the pivot column.
#' @param field.val A character string of the name of the field to aggregate.
#' @param fun A character string denoting which function to aggregate with.
#' @param totals.row A logical boolean (\code{TRUE}/\code{FALSE}) stating whether or not a total for rows should be included. Defaults to \code{TRUE}.
#' @param totals.col A logical boolean (\code{TRUE}/\code{FALSE}) stating whether or not a total for columns should be included. Defaults to \code{TRUE}.
#' @param numFrmt A character string of the number format to be displayed. Defaults to \code{'Round2'}.
#' @param fill A value to use to replace missing values (or 0 when using \code{fun='count'}) in the aggregate. Defaults to \code{NULL}.
#' @details 
#' \code{rawData} may be a \code{\link{data.table}} or \code{\link{data.frame}} (possibly a matrix as well?).
#' \code{fun} must be one of 'sum', 'mean', or 'count'.
#' The value supplied to \code{numFrmt} will be passed to \code{\link{simpleFormat}}. Any value supplied must match a valid argument for this function.
#' If \code{fun = 'count'}, then \code{numFrmt} is ignored. (\code{'Round0'} is used instead.)
#' @keywords methods
#' @return A \code{\link{data.table}} of the pivoted data.
#' @seealso \code{\link{formatPivotTable}} \code{\link{simpleFormat}}
#' @export
#' @examples 
#' rawData <- data.table(Row = rep(LETTERS[1:5],each=10), Column = rep(LETTERS[26:22],each=5), Value = rnorm(300), Category = letters[10:14])
#' pivotData <- createPivotTable(rawData, field.row='Row', field.col='Column', field.val='Value')
#' pivotData <- createPivotTable(rawData, field.row='Row', field.col='Column', field.val='Category', fun='count', totals.row=FALSE)
#' pivotData <- createPivotTable(rawData, field.row='Row', field.col='Column', field.val='Value', fun='mean', numFrmt='Percent2', totals.row=FALSE, totals.col=FALSE, fill=0)
#' pivotData <- createPivotTable(rawData, field.row='Row', field.col='Column', field.val='Value', fun='count', fill='MISSING')
#' 


createPivotTable <- function(rawData, field.row, field.col, field.val, fun = 'sum',
                             totals.row = TRUE, totals.col = TRUE, numFrmt = 'Round2',
                             fill = ''){
  rawData <- data.table(rawData)
  validFuns <- c('sum','mean','count')
  if (!(fun %in% validFuns)) {
    stop(paste0('Argument fun must be one of the items in [',paste0(validFuns,collapse=", "),'].'))
  }
  if (!all(c(field.row, field.col, field.val) %in% names(rawData))) {
    stop('One or more field names were not found in the data set.')
  }
  
  rowData <- rawData[,get(eval(field.row))]
  colData <- rawData[,get(eval(field.col))]
  valData <- rawData[,get(eval(field.val))]
  
  if (fun != 'count' & !is.numeric(valData)) {
    stop(paste0("When using fun = '",fun,"', field.val must be numeric."))
  }
  
  subData <- data.table(rowData, colData, valData)
  if (fun == 'sum') {
    rowTotal <- simpleFormat(subData[,.(AGG = sum(valData)),by=.(rowData)][order(rowData)][,AGG],numFrmt)
    colTotal <- c('TOTAL',simpleFormat(subData[,.(AGG = sum(valData)),by=.(colData)][order(colData)][,AGG],numFrmt))
    fullTotal <- simpleFormat(subData[,.(AGG = sum(valData))][,AGG],numFrmt)
    pvData <- dcast(subData, rowData~colData, value.var = 'valData', fun.aggregate = eval(fun), fill = NA)
  } else if (fun == 'count') {
    numFrmt <- 'Round0'
    rowTotal <- simpleFormat(subData[,.(AGG = .N),by=.(rowData)][order(rowData)][,AGG],numFrmt)
    colTotal <- c('TOTAL',simpleFormat(subData[,.(AGG = .N),by=.(colData)][order(colData)][,AGG],numFrmt))
    fullTotal <- simpleFormat(subData[,.(AGG = .N)][,AGG],numFrmt)
    pvData <- dcast(subData, rowData~colData, value.var = 'valData', fun.aggregate = length, fill = NA)
  } else { #fun == 'mean'
    rowTotal <- simpleFormat(subData[,.(AGG = mean(valData)),by=.(rowData)][order(rowData)][,AGG],numFrmt)
    colTotal <- c('TOTAL',simpleFormat(subData[,.(AGG = mean(valData)),by=.(colData)][order(colData)][,AGG],numFrmt))
    fullTotal <- simpleFormat(subData[,.(AGG = mean(valData))][,AGG],numFrmt)
    pvData <- dcast(subData, rowData~colData, value.var = 'valData', fun.aggregate = eval(fun), fill = NA)
  }
  
  names(pvData)[1] <- field.row
  
  pvData <- cbind(pvData[,1],simpleFormat(pvData[,2:length(names(pvData))],numFrmt))

  if (totals.row & totals.col) {
    pvData <- data.table(rbind(as.matrix(pvData),colTotal))
    pvData <- cbind(pvData, TOTAL = c(rowTotal, fullTotal))
  } else if (totals.row) {
    pvData <- cbind(pvData, TOTAL = rowTotal)
  } else if (totals.col) {
    pvData <- data.table(rbind(as.matrix(pvData),colTotal))
  }
  orig.names <- names(pvData)
  pvData <- sapply(pvData, trimws)
  pvData[pvData == 'NA'] <- fill
  pvData <- data.table(pvData)
  setnames(pvData, orig.names)
  return(pvData)
  
}

