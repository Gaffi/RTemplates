#' Select format for control chart labels
#' 
#' Allow customized formatting to be passed to the createControlChart() function.
#' @param value A numeric value to be formatted.
#' @param formatType A character string of the type to be used.
#'
#' @details Valid options for formatType are:
#' \itemize{
#'   \item 'Round0' = format(round(value,0),nsmall=0) # Rounding to whole numbers
#'   \item 'Round2' = format(round(value,2),nsmall=2) # Rounding to two decimal places
#'   \item 'Percent0' = paste0(formatC(round(value*100,0),format='f',digits=0),'\%') # Whole percent
#'   \item 'Percent2' = paste0(formatC(round(value*100,2),format='f',digits=2),'\%') # Percent to two decimal places
#'   \item 'Comma0' = format(round(value,0),big.mark=',',nsmall=0) # Rounding to whole numbers with thousands separator
#'   \item 'Comma2' = format(round(value,2),big.mark=',',nsmall=2) # Rounding to two decimal places with thousands separator
#'   \item 'Currency0' = paste0('$',format(round(value,0),big.mark=',',nsmall=0)) # Currency with no cents
#'   \item 'Currency2' = paste0('$',format(round(value,2),big.mark=',',nsmall=2)) # Currency with cents
#'   \item 'CurrencyK' = paste0('$',format(round(value/1000,2),big.mark=',',nsmall=2),' k') # Currency in thousands of $
#'   \item 'CurrencyM' = paste0('$',format(round(value/1000000,2),big.mark=',',nsmall=2),' mil') # Currency in millions of $
#' }
#' @keywords methods
#' @return A characer string of the value formatted as specified.
#' 
#' @export
#' @examples 
#' 
#' simpleFormat(12.34, 'Round0') #returns '12'
#' simpleFormat(0.9987654, 'Percent2') #returns '99.88%'
#' simpleFormat(12345678.90, 'Currency2') #returns '$12,345,678.90'
#' simpleFormat(12345678.90, 'CurrencyK') #returns '$12,345.68 k'
#' simpleFormat(12345678.90, 'CurrencyM') #returns '$12.35 mil'
#' 


simpleFormat <- function(value, formatType) {
  if (formatType == 'Round0') {
    return(format(round(value,0),nsmall=0))
  }
  
  if (formatType == 'Round2') {
    return(format(round(value,2),nsmall=2))
  }
  
  if (formatType == 'Percent0') {
    return(paste0(formatC(round(value*100,0),format='f',digits=0),'%'))
  }
  
  if (formatType == 'Percent2') {
    return(paste0(formatC(round(value*100,2),format='f',digits=2),'%'))
  }
  
  if (formatType == 'Comma0') {
    return(format(round(value,0),big.mark=',',nsmall=0))
  }
  
  if (formatType == 'Comma2') {
    return(format(round(value,2),big.mark=',',nsmall=2))
  }
  
  if (formatType == 'Currency0') {
    return(paste0('$',format(round(value,0),big.mark=',',nsmall=0)))
  }
  
  if (formatType == 'Currency2') {
    return(paste0('$',format(round(value,2),big.mark=',',nsmall=2)))
  }
  
  if (formatType == 'CurrencyK') {
    return(paste0('$',format(round(value/1000,2),big.mark=',',nsmall=2),' k'))
  }
  
  if (formatType == 'CurrencyM') {
    return(paste0('$',format(round(value/1000000,2),big.mark=',',nsmall=2),' mil'))
  }
  
}
