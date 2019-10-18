#' @title 
#' Write Log to Console
#' @description
#' Extends the \code{\link{writeLines}} function to add a timestamp when logging.
#' 
#' @param text A character string of the text to post.
#' @keywords methods
#' @return Writes \code{text} to console with the current timestamp.
#' @seealso \code{\link{writeLines}}
#' @export
#' @examples 
#' writeLog('Testing log.')
#' 
 
writeLog <- function(text, inc.date = TRUE) {
  if (inc.date) {
    writeLines(paste0(
      Sys.Date(),
      '-',
      format(Sys.time(), '%H:%M:%S'),
      ' :: ',
      text))
  } else {
    writeLines(paste0(
      format(Sys.time(), '%H:%M:%S'),
      ' :: ',
      text))
  }
}