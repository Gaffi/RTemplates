#' Simplified formatting option for common formats
#' 
#' Formats numbers in a variety of common formats without having to learn/understand the finer details of formatting in R.
#' @param value A numeric value to be formatted.
#' @param formatType A character string of the type to be used.
#'
#' @details Valid options for \code{formatType} are:
#' \itemize{
#'   \item 'Round0' = \code{format(round(value,0),nsmall=0,scientific=FALSE)} # Rounding to whole numbers
#'   \item 'Round2' = \code{format(round(value,2),nsmall=2,scientific=FALSE)} # Rounding to two decimal places
#'   \item 'Percent0' = \code{paste0(formatC(round(value*100,0),format='f',digits=0),'\%')} # Whole percent
#'   \item 'Percent2' = \code{paste0(formatC(round(value*100,2),format='f',digits=2),'\%')} # Percent to two decimal places
#'   \item 'Comma0' = \code{format(round(value,0),big.mark=',',nsmall=0,scientific=FALSE)} # Rounding to whole numbers with thousands separator
#'   \item 'Comma2' = \code{format(round(value,2),big.mark=',',nsmall=2,scientific=FALSE)} # Rounding to two decimal places with thousands separator
#'   \item 'Currency0' = \code{paste0('$',format(round(value,0),big.mark=',',nsmall=0,scientific=FALSE))} # Currency with no cents
#'   \item 'Currency2' = \code{paste0('$',format(round(value,2),big.mark=',',nsmall=2,scientific=FALSE))} # Currency with cents
#'   \item 'CurrencyK' = \code{paste0('$',format(round(value/1000,0),big.mark=',',nsmall=0,scientific=FALSE),' K')} # Currency in thousands of $
#'   \item 'CurrencyM' = \code{paste0('$',format(round(value/1000000,0),big.mark=',',nsmall=0,scientific=FALSE),' mil')} # Currency in millions of $
#' }
#' @keywords methods
#' @return A characer string of the value formatted as specified.
#' 
#' @export
#' @examples 
#' 
#' simpleFormat(12.3456789, 'Round0') # returns '12'
#' simpleFormat(12.3456789, 'Round2') # returns '12.35'
#' simpleFormat(123456789, 'Comma2') # returns '123,456,789.00'
#' simpleFormat(0.9987654, 'Percent2') # returns '99.88%'
#' simpleFormat(12345678.90, 'Currency2') # returns '$12,345,678.90'
#' simpleFormat(12345678.90, 'CurrencyK') # returns '$12,346 K'
#' simpleFormat(12345678.90, 'CurrencyM') # returns '$12 mil'
#' 


simpleFormat <- function(value, formatType) {
  if (formatType == 'Round0') {
    return(format(round(value,0),nsmall=0,scientific=FALSE))
  }
  
  if (formatType == 'Round2') {
    return(format(round(value,2),nsmall=2,scientific=FALSE))
  }
  
  if (formatType == 'Percent0') {
    return(lapply(lapply(lapply(value*100,round,digits=0),formatC,format='f',digits=0),paste0,'%'))
  }
  
  if (formatType == 'Percent2') {
    return(lapply(lapply(lapply(value*100,round,digits=2),formatC,format='f',digits=2),paste0,'%'))
  }
  
  if (formatType == 'Comma0') {
    return(format(round(value,0),big.mark=',',nsmall=0,scientific=FALSE))
  }
  
  if (formatType == 'Comma2') {
    return(format(round(value,2),big.mark=',',nsmall=2,scientific=FALSE))
  }
  
  if (formatType == 'Currency0') {
    return(lapply(lapply(lapply(value,round,digits=0),format,big.mark=',',nsmall=0,scientific=FALSE),function(x){return(paste0('$',x))}))
  }
  
  if (formatType == 'Currency2') {
    return(lapply(lapply(lapply(value,round,digits=2),format,big.mark=',',nsmall=2,scientific=FALSE),function(x){return(paste0('$',x))}))
  }
  
  if (formatType == 'CurrencyK') {
    return(lapply(lapply(lapply(value/1000,round,digits=0),format,big.mark=',',nsmall=0,scientific=FALSE),function(x){return(paste0('$',x,' K'))}))
  }
  
  if (formatType == 'CurrencyM') {
    return(lapply(lapply(lapply(value/1000000,round,digits=0),format,big.mark=',',nsmall=0,scientific=FALSE),function(x){return(paste0('$',x,' mil'))}))
  }
  
}
