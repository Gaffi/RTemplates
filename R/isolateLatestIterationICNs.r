#' @title 
#' Isolate Latest Iteration ICNs
#' @description
#' Given a listing of bill data (must include ICN), group all related ICN bases and return all data for only those that are the highest iteration.
#' 
#' @param data A \code{\link{data.table}} containing the data to filter.
#' @param full A logical boolean (\code{TRUE}/\code{FALSE}) stating whether the returned result should include all data (\code{TRUE}) or only the ICNs (\code{FALSE}). Defaults to \code{TRUE}.
#' @param verbose A logical boolean (\code{TRUE}/\code{FALSE}) stating whether output messages should be displayed. Defaults to \code{TRUE}.
#' @details 
#' \code{isolateLatestIterationICNs()} will try to coerce data to a \code{data.table} if not provided in this format.
#' @keywords methods
#' @return A \code{\link{data.table}} containing the filtered data. Full dataset if \code{full = TRUE}, single column of ICNs if \code{full = FALSE}.
#' @export
#' @examples 
#' library (RTemplates)
#' sql <- "SELECT ICN, FNLZD_PAID_POSTING_DT 
#'         FROM BILLREVW.BILL_HDR
#'         WHERE FNLZD_PAID_POSTING_DT BETWEEN '01-JAN-2016'
#'            AND '31-DEC-2016'
#'         AND BILLREVW_BE_ID = '008'"
#' 
#' dt <- runSQLQuery(sql,"wcdw64",verbose=TRUE)
#' isolateLatestIterationICNs(dt)
#' 
 

isolateLatestIterationICNs <- function(data, full = TRUE, verbose = TRUE) {
  if (!('data.table' %in% class(data))) {
    writeLog('Non-data.table structure supplied. Converting...')
    data <- data.table(data)
  }
  
  data[,ICN_BASE:=substring(ICN,1,12)]
  data[,ICN_IT:=substring(ICN,13,14)]
  if (verbose) writeLog('Identifying latest iterations...')
  data2 <- data[,.(LATEST=max(ICN_IT)), by=.(ICN_BASE)]
  data2[,ICN:=paste0(ICN_BASE,LATEST)]
  if (verbose) writeLog(paste0('Isolated ',length(data2[,ICN]),' ICNs (out of ',length(data[,ICN]),' in original set).'))
  if (full) {
    return(data[ICN %in% data2[,ICN]])
  } else {
    return(data2[,ICN])
  }
}
