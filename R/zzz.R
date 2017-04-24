#' Reporting Team R Template Package
#' 
.onLoad <- function(libname, pkgname) {
  packageStartupMessage('R Reporting Template package loaded...\n')
  packageStartupMessage('Please use reportingTemplatesHelp() for documenation.\n')
}

#' @title
#' R Reporting Template Package
#' 
#' @description R Reporting Template Package
#' 
#' @details
#' For all functions, type the following in your R console:
#' 
#'     - "?functionName" - Displays more detailed and specific documentation for each function.
#'        
#'     - "functionName" (without parentheses) - Displays the raw code of the function.
#' @keywords documentation misc utilities
#' @export
#' @examples 
#' Charting Functions:
#'    createControlChart 
#'       Template for creating Minitab-like control charts.
#' 
#' Miscallaneous Functions:
#'    simpleFormat   
#'       Shortcut to convert values to various number formats. 

reportingTemplatesHelp <- function() {
  ?reportingTemplatesHelp
}

