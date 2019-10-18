#' @title 
#' Date Selection
#' @description
#' Provide a user form to prompt the user for date(s), which will then return the inputted values in various formats.
#' @param numFields A \code{\link{numeric}} value specifying the number of date fields to prompt. Defaults to \code{2}.
#' @param outputFormat A \code{\link{character}} string specifying format type of the output. Defaults to \code{'R'}. See details for more info.
#' @keywords interface io methods
#' @return A vector containing the formatted dates.
#' @seealso \code{\link{fgui::gui}}, \code{\link{format.Date}}
#' @details 
#' \code{numFields} must be between 1 and 4, inclusive. Each corresponding field on the form will be labeled "Date x", where x is each integer within the range provided.
#' The default value is 2, (\code{Date 1}, \code{Date 2}) which generally allows for the selection of a start and end date, but this need not necessarily be used in this way.
#' 
#' \code{outputFormat} must be one of:
#' \itemize{
#' \item{\code{'R'} - Default. Will return \code{Date} objects. e.g. \code{as.Date('2018-07-15')}}
#' \item{\code{'Print'} - Will return strings, formatted for normal reading. e.g. \code{07/15/2018}}
#' \item{\code{'SQL'} - will return strings, formatted to be used in SQL queries. e.g. \code{'15-JUL-2018'} (note the quotes)}
#' }
#' @export
#' @examples 
#' 
#' dates <- getUserDates()
#' dates <- getUserDates(1, 'Print')
#' dates <- getUserDates(numFields = 3, 'SQL')


getUserDates <- function(numFields = 2, outputFormat = 'R') {
  invldNumFieldText <- 'numFields must be an integer between 1 and 4, inclusive. Please try again.'
  invldOutputFormatText <- 'outputFormat must be one of "R", "Print", or "SQL". Please try again.'
  
  if (numFields < 1 | numFields > 4) stop(invldNumFieldText)
  if (as.integer(numFields) != numFields) stop(invldNumFieldText)
  
  if (!outputFormat %in% c("R", "Print", "SQL")) stop(invldOutputFormatText)
  
  guiOpts <- switch(
    numFields,
    function(Date.1){},
    function(Date.1, Date.2){},
    function(Date.1, Date.2, Date.3){},
    function(Date.1, Date.2, Date.3, Date.4){}
  )
  
  guiTitle <- if(numFields == 1) {
    "Enter Date (YYYY-MM-DD)"
  } else {
    "Enter Dates (YYYY-MM-DD)"
  }
  
  outDates <-
    unlist(
      fgui::gui(
        guiOpts,
        title = guiTitle,
        output = NULL,
        modal = TRUE
      )
    )

  tryCatch(
    {
      outDates <- switch(
        outputFormat,
        'R' = as.Date(fasttime::fastPOSIXct(outDates)),
        'SQL' = lapply(outDates, format.Date, "'%d-%b-%Y'"),
        'Print' = lapply(outDates, format.Date, "%m/%d/%Y")
      )
    },
    error = function(e) {
      writeLog(e)
      outDates <- NULL
    }
  )
  
  return(outDates)
}
