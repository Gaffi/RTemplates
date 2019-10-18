#' @title 
#' Get Previous Weekday
#' @description
#' Returns the previous weekday based on a date provided to the function. 
#' @details 
#' Date provided must be formatted as a valid R Date. This function will not attempt to convert.
#' The date value returned will typically be \code{date - 1}, however, in teh case of running on a Saturday, Sunday, or Monday, the previous Friday date is returned.
#' @param date Origin date from which to find the previous date.
#' @keywords methods date
#' @return A date value of the preceeding weekday.
#' @seealso \code{\link{as.Date}}
#' @export
#' @examples 
#' beginsEndsWith('test','te',TRUE) # returns TRUE
#' beginsEndsWith('test','test1',TRUE) # returns FALSE
#' beginsEndsWith('test','t',FALSE) # returns TRUE
#' beginsEndsWith('test','t',FALSE, TRUE) # returns FALSE
#' 

getPrevWeekday <- function(date) {
  if(!(class(date)=='Date')) {
    stop('Value supplied must be a date. If using a date string, first convert with as.Date().')
  }
  # If previous date is Monday-Friday...
  if(format(date - 1, '%w') %in% 1:5) {
    # ... then return the date
    return(date - 1)
  } else{
    # ... else re-run the function recursively on the previous date
    Recall(date - 1)
  }
}