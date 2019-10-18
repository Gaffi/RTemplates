#' @title 
#' ODBC Connection Template
#' @description
#' Wrapper to run SQL queries against a database
#' 
#' @param SQL A character string of the query to execute or a filename to run.
#' @param DB A character string of the name of the database to connect to. Must be a configured database in the Windows ODBC connection list.
#' @param ... Arguments to pass to \code{\link{runSQLQuery}}.
#' @param startDate A \code{\link{Date}} specifying the beginning of the range to query. Optional.
#' @param endDate A \code{\link{Date}} specifying the end of the range to query. Optional.
#' @param by Increment of the date sequence. Optional. See 'Details'. Defaults to \code{'1 month'}.
#' @param altVar A \code{\link{character}} string specifying the name of the alternate field to replace in the SQL. Optional.
#' @param altVals A vector of elements (\code{character} or \code{numeric}) to pass as values to the alternate field. Optional.
#' @param altBy A \code{\link{numeric}} value, specifying how many items from \code{altVals} should be grouped together (the size of the groups). Optional.
#' @param combine A boolean (\code{TRUE}/\code{FALSE}) that specifies whether the output should be combined into one single \code{\link{data.table}} (\code{TRUE}) or a \code{\link{list}} of \code{data.table}s (\code{FALSE}). Defaults to \code{TRUE}.
#' @keywords interface connections database io methods
#' @return A \code{\link{data.table}} or \code{\link{list}} containing the output of the queries.
#' @seealso \code{\link{runSQLQuery}}, \code{\link{seq.Date}}
#' @details 
#' \code{SQL} can either be supplied as a direct SQL string, or the path and name of a file containing SQL. File types can be \code{'.txt'} or \code{'.sql'}. SQL passed must be a complete and correctly formed query.
#'
#' If using the date functionality with \code{startDate} and \code{endDate}, SQL code passed must include both \code{&SD} and \code{&ED} as start and end date respectively. These placeholders are used to swap out the required date ranges determined by the function. The form should be \code{<date field> BETWEEN &SD AND &ED}.
#'
#' If using the the alternative replacement functionality with \code{altVar} and \code{altVals}, SQL code passed should be (but is not exlicitly required to be) structured to look for this value in an \code{IN} list. e.g., \code{<alt field> IN (<altVar>)}. If \code{altBy} is anything other than \code{1}, then the SQL will result in an error, otherwise.
#' 
#' The \code{...} (dot) arguments are the optional arguments used by \code{\link{runSQLQuery}}. All required arguments for \code{runSQLQuery} are also required here.
#' For dates, \code{by} can be specified in several ways:
#' \itemize{
#' \item{A number, taken to be in days.}
#' \item{A object of class difftime.}
#' \item{A character string, containing one of "day", "week", "month", "quarter" or "year". This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s".}
#' }
#' See \code{\link{seq.Date}} for further information about \code{by}.
#' When specifying a \code{'month'} value to \code{by}, the function will parse the months to full month dates (i.e. 2017-01-01 through 2017-01-31). However, if a \code{startDate} or \code{endDate} is supplied that lies in the middle of a month, the resulting query will pull for only the dates that fall within the start/end date range.
#' If passing a list of text values to \code{altVals}, elements must be encoded with proper quotations. e.g., c("'ABC'","'DEF'","'GHI'").
#' @export
#' @examples 
#' 
#' dt <- runSQLQuerySplit(SQL = 'Example.sql', DB='DBName',
#'             verbose = TRUE, creds=creds,
#'             replacements = list('&BEID' = "'008'"),
#'             startDate = as.Date('2017-01-05'),
#'             endDate = as.Date('2017-01-10'),
#'             by = '1 day')
#'             
#' dt <- runSQLQuerySplit(SQL = 'SELECT * FROM TABLE WHERE DATE BETWEEN &SD AND &ED', DB='DBName',
#'             verbose = TRUE, creds=creds,
#'             replacements = list('&BEID' = "'008'"),
#'             startDate = as.Date('2017-01-05'),
#'             endDate = as.Date('2017-03-10'),
#'             by = '1 month')
#'             
#' dt <- runSQLQuerySplit(SQL = SQL, DB = 'DBName', 
#'             verbose = TRUE, creds=creds,
#'             altVar = '&OIP', 
#'             altVals = c("'GAL'","'CNA'","'NYF'","'FIR'"), 
#'             altBy = 2)
#'             
#' dt <- runSQLQuerySplit(SQL = SQL, DB = 'DBName',
#'             verbose = TRUE, creds=creds,
#'             startDate = as.Date('2017/12/01'),
#'             endDate = as.Date('2018/01/31'), 
#'             altVar = '&OIP', 
#'             altVals = c("'GAL'","'CNA'","'NYF'","'FIR'"), 
#'             verbose = TRUE, 
#'             altBy = 2)

runSQLQuerySplit <- function(SQL, 
                             DB,
                             ..., 
                             startDate = NA, 
                             endDate = NA, 
                             by = '1 month',
                             altVar = NA,
                             altVals = NA,
                             altBy = 1,
                             combine = TRUE) {
  dots <- list(...)
  
  if(is.na(startDate) & is.na(altVar)) {stop('Must supply dates (startDate/endDate) or other variable (altVar) argument.')}
  if(!is.na(startDate)) {
    if(startDate > endDate) {stop('startDate must come before endDate.')}
  }
  # if(missing(startDate)) {stop('Must supply starting date (startDate) argument.')}
  # if(missing(endDate)) {stop('Must supply ending date (endDate) argument.')}
  
  dotVerbose <- if (is.null(dots$verbose)) {
    FALSE
  } else {
    if (is.logical(dots$verbose)) {
      dots$verbose
    } else {
      stop('If verbose is specified, then it must be either TRUE/FALSE.')
    }
  }
  
  if (is.null(dots$creds)) {
    creds <- # PRE-LOAD DB CREDENTIALS TO AVOID HAVING TO REPEATEDLY ENTER THEM FOR EACH RUN
      getUserPass(paste0(DB, ' User Info'),
                  paste0('Please enter ', DB, ' Username:'),
                  paste0('Please enter ', DB, ' Password:')) 
  } else {
    creds <- dots$creds
  }
  
  # PROCESS FOR DATES ONLY
  if (!is.na(startDate) & !is.na(endDate) & is.na(altVar)) {
    # CREATE SEQUENCE OF DATES TO RUN REPORT OVER
    # IF GROUPING BY MONTH AND MID-MONTH DATES ARE SPECIFIED, THEN
    # LIMIT TO PROPER START/END OF MONTH BY RESETTING EACH MONTH 
    # VALUE TO THE FIRST OF THE MONTH RATHER THAN MIDDLE OF MONTH 
    if (by %like% 'month') {
      replFillSeq <- seq(as.Date(format(startDate,'%Y-%m-01')), endDate, by = by)
    } else {
      replFillSeq <- seq(startDate, endDate, by = by)
    }
    
    # CREATE A LIST BASED ON THE ABOVE SEQUENCE THAT ASSIGNS
    # START AND END DATES FOR EACH RANGE
    replFill <- 
      list(
        Start = format(replFillSeq[1:(length(replFillSeq))], "'%d-%b-%Y'"),
        End = format(
          as.Date(
            unlist(
              lapply(replFillSeq,
                     function(x){
                       seq.Date(x, by=by, length.out = 2)[2]-1
                     }
              )
            ),
            origin='1970-01-01'
          ),
          "'%d-%b-%Y'")
      )
    
    listdata <- 
      mapply(
        function(x, y) {
          if (as.Date(x, "'%d-%b-%Y'") < startDate) {x <- format(startDate, "'%d-%b-%Y'")}
          if (as.Date(y, "'%d-%b-%Y'") > endDate) {y <- format(endDate, "'%d-%b-%Y'")}
          
          dateReplacements <- list('&SD' = x, '&ED' = y)
          
          dotReplacements <- if (is.null(dots$replacements)) {
            dateReplacements
          } else {
            append(dateReplacements, dots$replacements)
          }
          
          if(dotVerbose) {
            writeLog(
              paste0('Running for span ', x, ' through ', y)
            )
          }
          
          return(
            runSQLQuery(
              SQL = SQL, DB = DB, verbose = dotVerbose, 
              creds = creds, replacements = dotReplacements
            )
          )
        }, 
        replFill$Start, 
        replFill$End, 
        SIMPLIFY=FALSE)
  }
  
  # PROCESS FOR ALTVAR ONLY
  if (is.na(startDate) & is.na(endDate) & !is.na(altVar)) {
    # CREATE A LIST BASED ON ALTVALS, GROUPED INTO ALTBY-SIZED CHUNKS
    replVals <- 
      split(
        altVals,
        ceiling(seq_along(altVals)/altBy)
      )
    
    listdata <- 
      lapply(
        replVals,
        function(z) {
          varReplacements <- list(paste0(z, collapse=','))
          names(varReplacements) <- altVar
          
          dotReplacements <- if (is.null(dots$replacements)) {
            varReplacements
          } else {
            append(varReplacements, dots$replacements)
          }
          
          if(dotVerbose) {
            writeLog(
              paste0('Running for group: "', paste0(z, collapse=','), '"')
            )
          }
          
          return(
            runSQLQuery(
              SQL = SQL, DB = DB, verbose = dotVerbose, 
              creds = creds, replacements = dotReplacements
            )
          )
        }
      )
  }
  
  # PROCESS FOR ALTVAR AND DATES
  if (!is.na(startDate) & !is.na(endDate) & !is.na(altVar)) {
    # CREATE SEQUENCE OF DATES TO RUN REPORT OVER
    # IF GROUPING BY MONTH AND MID-MONTH DATES ARE SPECIFIED, THEN
    # LIMIT TO PROPER START/END OF MONTH BY RESETTING EACH MONTH 
    # VALUE TO THE FIRST OF THE MONTH RATHER THAN MIDDLE OF MONTH 
    if (by %like% 'month') {
      replFillSeq <- seq(as.Date(format(startDate,'%Y-%m-01')), endDate, by = by)
    } else {
      replFillSeq <- seq(startDate, endDate, by = by)
    }
    
    # CREATE A LIST BASED ON THE ABOVE SEQUENCE THAT ASSIGNS
    # START AND END DATES FOR EACH RANGE
    replFill <- 
      list(
        Start = format(replFillSeq[1:(length(replFillSeq))], "'%d-%b-%Y'"),
        End = format(
          as.Date(
            unlist(
              lapply(replFillSeq,
                     function(x){
                       seq.Date(x, by=by, length.out = 2)[2]-1
                     }
              )
            ),
            origin='1970-01-01'
          ),
          "'%d-%b-%Y'")
      )
    
    # CREATE A LIST BASED ON ALTVALS, GROUPED INTO ALTBY-SIZED CHUNKS
    replVals <- 
      split(
        altVals,
        ceiling(seq_along(altVals)/altBy)
      )
    
    listdata <- 
      mapply(
        function(x, y) {
          if (as.Date(x, "'%d-%b-%Y'") < startDate) {x <- format(startDate, "'%d-%b-%Y'")}
          if (as.Date(y, "'%d-%b-%Y'") > endDate) {y <- format(endDate, "'%d-%b-%Y'")}
          
          dateReplacements <- list('&SD' = x, '&ED' = y)
          
          if(dotVerbose) {
            writeLog(
              paste0('Running for span ', x, ' through ', y)
            )
          }
          rbindlist(
            lapply(
            replVals,
            function(z) {
              varReplacements <- list(paste0(z, collapse=','))
              names(varReplacements) <- altVar
              
              dotReplacements <- if (is.null(dots$replacements)) {
                append(varReplacements, dateReplacements)
              } else {
                append(varReplacements, dateReplacements, dots$replacements)
              }
              
              if(dotVerbose) {
                writeLog(
                  paste0('Running for span ', x, ' through ', y, ', for group: "', paste0(z, collapse=','), '"')
                )
              }
              
              return(
                runSQLQuery(
                  SQL = SQL, DB = DB, verbose = dotVerbose, 
                  creds = creds, replacements = dotReplacements
                )
              )
            }
          )
          )
        }, 
        replFill$Start, 
        replFill$End, 
        SIMPLIFY=FALSE)
  }
  
  if (combine) {
    # RETURN A SINGLE TABLE WITH RESULTS
    # REMOVE ANY EMPTY TABLES BEFORE RBINDLIST SINCE THAT CREATES ERRORS.
    return(rbindlist(listdata[lapply(listdata,nrow)>0]))
  } else {
    # RETURN A LIST OF TABLES WITH RESULTS (INCLUDING EMPTY RESULT SETS)
    return(listdata)
  }
}
