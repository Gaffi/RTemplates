#' @title 
#' Check if some text begins or ends with some other text
#' @description
#' Extends the \code{\link{substring}} function to check to see if a string begins or ends with a specified string.
#' 
#' @param text A character string of the text to search through.
#' @param find A character string of the text to search for.
#' @param beginSearch A logical boolean (\code{TRUE}/\code{FALSE}) that specifies if a search should be performed at the beginning of the string. \code{TRUE} = beginning, \code{FALSE} = end.
#' @param caseSensitive A logical boolean (\code{TRUE}/\code{FALSE}) that specifies whether text case should be matched exactly or ignored. \code{TRUE} = match case, \code{FALSE} = ignore case. Defaults to FALSE.
#' @keywords methods
#' @return A logical boolean (\code{TRUE}/\code{FALSE}) that states whether or not the \code{find} parameter was found within \code{text}.
#' @seealso \code{\link{substring}}
#' @export
#' @examples 
#' beginsEndsWith('test','te',TRUE) # returns TRUE
#' beginsEndsWith('test','test1',TRUE) # returns FALSE
#' beginsEndsWith('test','t',FALSE) # returns TRUE
#' beginsEndsWith('test','t',FALSE, TRUE) # returns FALSE
#' 

beginsEndsWith <- function(text, find, beginSearch, caseSensitive = FALSE) {
  if (!caseSensitive) {
    text <- tolower(text)
    find <- tolower(find)
  }
  if (nchar(find) > nchar(text)){
    return(FALSE)
  }
  if (beginSearch) {
    return(substring(text, 1, nchar(find)) == find)
  } else {
    return(substring(text, nchar(text) - nchar(find) + 1) == find)
  }
}

