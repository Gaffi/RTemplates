#' @title 
#' Store data in a temporary database table
#' @description
#' Wrapper to store data in a temporary table. Data can be an R dataset or pulled directly from the destination or external source database.
#' 
#' @param DBTo A character string of the name of the ODBC database connection to send data to.
#' @param data An R dataset (\code{\link{data.frame}} or \code{\link{data.table}}) to stop in the temporary table.
#' @param tempName A character string of the name to use for the temporary table. Defaults to \code{'TEMP'}.
#' @param index A named list containing information about how to create indicies. See details for more information.
#' @param verbose A logical boolean (\code{TRUE}/\code{FALSE}) stating whether output messages should be displayed. Defaults to \code{FALSE}.
#' @keywords interface connections database io methods
#' @return Result will be a table in the requested database with a name given as defined in the details section.
#' @details 
#' You must have appropriate read/write access to the database identified in \code{DBTo} in order for this funciton to work.
#' 
#' The table created in the database will be named \code{<username>.<tempName>} if \code{tempName} supplied, otherwise, \code{<username>.temp}
#' 
#' The named list supplied to the \code{index} argument should be a vector of column names, with each named element being the intended name of the index. If left blank, no indexes will be produced. The named list should be constructed as follows:
#' \itemize{
#'   \item TYPE: Named lists with names \code{'UNIQUE'} and/or \code{'NORMAL'} noting the type of index to create.
#'   \item NAME: Named lists with names to be used as the index name. The index will be named according to the table-naming scheme above: \code{<table_name>_<index_name>}.
#'   \item COLUMNS: Character/String elemements corresponding to the column names to be used in the index.
#' }
#' See the examples section for a correct, working example of an index list.
#' @export
#' @examples 
#' 
#' myODBCDatabase <- 'ODBCConName'
#' myData <- data.table(A=1:100000, B=LETTERS[1:10], C=rnorm(100000))
#' saveDataToTempTable(DBTo = myODBCDatabase, data = myData)
#' # With indexing:
#' myIndex <- list( # Top level name indicates the type (UNIQUE/NORMAL)
#'                 UNIQUE = list( # Second level name indicates the name of the index
#'                               KEY = 'A'),
#'                  # Top level name indicates the type (UNIQUE/NORMAL)
#'                 NORMAL = list( # Second level name indicates the name of the index
#'                              ALT = c('B','C'))
#'                )
#' saveDataToTempTable(myODBCDatabase, myData, tempName = 'MY_TEMP_TABLE', index = myIndex, verbose = TRUE)


saveDataToTempTable <- function(DBTo, data, tempName = 'TEMP', index = NULL, verbose = FALSE) {
  if (missing(data)) {stop("No data supplied.")}
  
  if (!is.null(index)) {
    for (curIdxType in names(index)) {
      for (curIdx in names(index[[curIdxType]])) {
        for (curFld in index[[curIdxType]][[curIdx]]) {
          if (!(curFld %in% names(data))) {
            stop(paste0("Column '", curFld, "' from ", curIdxType, " index '", curIdx ,"' not found in data."))
          }
        }
      }
    }
  }
  
  DBToCreds <- getUserPass(DBTo, paste0('Enter ', DBTo,' Username'))
  
  con <- odbcConnect(DBTo, uid = DBToCreds[1], pwd = DBToCreds[2], 
                     rows_at_time = 500, believeNRows=FALSE)
  
  tempTableName <- toupper(paste0(DBToCreds[1], '.', tempName))
  
  tryCatch(
    {
      if(verbose) {writeLog('Dropping table if it already exists.')}
      dropResult <- sqlQuery(con, paste0('drop table ', tempTableName))
    },
    error=function(e){})
  if(verbose) {writeLog(paste0('Sending data to ', DBTo,'...'))}
  sqlSave(con, data, tempTableName, rownames = FALSE, verbose = verbose, varTypes = c(A='INTEGER',B='NUMBER'))
  
  
  if (!is.null(index)) {
    if(verbose) {writeLog('Running index query(ies)...')}
    for (curIdxType in names(index)) {
      for (curIdx in names(index[[curIdxType]])) {
        if(verbose) {writeLog(paste0(curIdxType, ' index: ', curIdx))}
        indexResult <- sqlQuery(
          con, paste0('create ', 
                      if(toupper(curIdxType) == 'UNIQUE') {'UNIQUE '} else {''},
                      'index ',
                      tempTableName, '_', 
                      curIdx, ' on ',
                      tempTableName, ' (',
                      paste(index[[curIdxType]][[curIdx]],collapse=','), ')'))
      }
    }
  }

  close(con)
}
