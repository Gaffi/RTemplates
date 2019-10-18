#' @title 
#' Replace Multiple Variables in a Large String
#' @description
#' Replaces a series of variable placeholders with specified string replacements.
#' 
#' @param text A character string of the text to find replacements in.
#' @param replacements A named vector/list of each variable/placeholder and it's respective replacement.
#' @keywords methods
#' @return A character string containing the original \code{text} with all the specified replacements made.
#' @seealso \code{\link{gsub}}
#' @details 
#' 
#' @export
#' @examples 
#' #Simple replacement
#' replaceMultInStr('This is a &Var1 of some &Var2.',c('&Var1' = 'replacement', '&Var2' = 'text'))
#' # Outputs: "This is a replacement of some text."
#' 
#' #Placeholders need to be distinct. This doesn't work:
#' replaceMultInStr('This is a &Var1 of some &Var10.',c('&Var1' = 'replacement', '&Var10' = 'text'))
#' # Outputs: "This is a replacement of some replacement0."
#' 
replaceMultInStr <- function(text, replacements) {
  to_repl <- names(replacements)
  
  for (i in 1:length(to_repl)) {
    text <- gsub(to_repl[i],replacements[i],text)
  }
  
  return(text)
}
