#' @title 
#' Read a text file
#' @description
#' Read in a text file (.txt, .sql, etc.) and parse the text to a character string.
#' 
#' @param filename A character string of the name of the file to read. If the full path is not provided, R will use the current working directory.
#' @param stripEscape A boolean indicating whether or not to remove special/escape characters (\code{\\n})form the read text. Defaults to \code{FALSE}.
#' @keywords methods io
#' @return A character string of the contents of the file read.
#' @seealso \code{\link{readChar}}
#' @export
#' @examples 
#' myQuery <- loadTextFromFile('query.sql')
#' 

loadTextFromFile <- function(filename, stripEscape = FALSE) {
  if (stripEscape) {
    return(gsub(pattern='\\s',replacement=" ",x=readChar(filename, file.info(filename)$size)))
  } else {
    return(readChar(filename, file.info(filename)$size))
  }
}
