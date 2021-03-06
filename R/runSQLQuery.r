#' @title 
#' ODBC Connection Template
#' @description
#' Wrapper to run SQL queries against a database
#' 
#' @param SQL A character string of the query to execute or a filename to run.
#' @param DB A character string of the name of the database to connect to. Must be a configured database in the Windows ODBC connection list.
#' @param creds A 2 element vector of character strings. Defaults to NA.
#' @param replacements A named list of replacements to make in SQL code. Defaults to NA.
#' @param verbose A logical boolean (\code{TRUE}/\code{FALSE}) stating whether output messages should be displayed. Defaults to \code{FALSE}.
#' @keywords interface connections database io methods
#' @return A \code{\link{data.table}} object containing the output of the query.
#' @seealso \code{\link{getUserPass}}, \code{\link{replaceMultInStr}}
#' @details 
#' \code{runSQLQuery} collects user login info (if not supplied via \code{creds}) and correctly opens and closes a database connection while returning the results of an executed query.
#' 
#' \code{SQL} can either be supplied as a direct SQL string, or the path and name of a file containing SQL. File types can be \code{'.txt'} or \code{'.sql'}. SQL passed must be a complete and correctly formed query.
#' 
#' If \code{creds} is supplied, the data should be in the same vector format which is returned by \code{\link{getUserPass}}. That is, the first element is a username, second element is a password. If supplying this argument, it should generally be used in conjunction with \code{\link{getUserPass}}, though it is not explicitly required. If \code{creds} is not supplied, \code{runSQLQuery} will prompt for a username/password.
#' 
#' \code{replacements} is used by the function \code{\link{replaceMultInStr}} and must be supplied for all attributes/variables found within your SQL query, else the query may fail to execuse properly.
#' @export
#' @examples 
#' 
#' # Retrieve first 10 records from BILL_HDR table in the WCDW.
#' queryString <- "SELECT * FROM BILLREVW.BILL_HDR BH WHERE ROWNUM <= 10"
#' myData <- runSQLQuery(queryString) # Default is WCDW/'WCDW64'
#' 
#' # Retrieve first 10 records from TOPIC table in the WCRPT with user/pass supplied.
#' userAndPass <- getUserPass()
#' queryString <- "SELECT * FROM TOPIC T WHERE ROWNUM <= 10"
#' myData <- runSQLQuery(queryString, 'WCRPT64', userAndPass) # Database connection must be defined in your Windows ODBC configuration.
#' 
runSQLQuery <- function(SQL, DB, creds = NA, replacements = NA, verbose = FALSE) {
  if (length(creds) < 2) {creds <- getUserPass(helperLabel = DB, helperUsername = paste0('Please enter your ',DB,' username'))}
  if (nchar(creds[1] > 0 & nchar(creds[2] > 0))) {
    if (beginsEndsWith(SQL, '.txt', FALSE) | beginsEndsWith(SQL, '.sql', FALSE)) {
      if (verbose) writeLog('Filename passed as SQL argument. Loading text from file...')
      SQL <- loadTextFromFile(SQL)
    }
    
    if (!missing(replacements)) {
      if (verbose) writeLog('Replacements supplied. Applying replacements to SQL...')
      SQL <- replaceMultInStr(SQL, replacements)
    }
    timingReportStart<-Sys.time()
    con <- RODBC::odbcConnect(DB, uid=creds[1], pwd=creds[2], rows_at_time = 500, believeNRows=FALSE)
    if (verbose) writeLog('Query running. Please wait...')
    d <- data.table(RODBC::sqlQuery(con, SQL))
    close(con)
    if (verbose) writeLog(paste0('Query completed in ',format(.POSIXct(difftime(Sys.time(), timingReportStart, units = 'secs'),tz='GMT'),'%H:%M:%S'),'.'))
  } else{
    if (verbose) writeLog('No username and/or password provided. Returning empty data.table.')
    d <- data.table()
  }
    
  return(d)
}
