#' R Template Package
.onLoad <- function(libname, pkgname) {
  packageStartupMessage('Template package loaded...\n')
  packageStartupMessage('Please use TemplateHelp() for documenation.\n')
}

#' @title
#' R Template Package
#' 
#' @description Templates for quick access to various functions within the R programming language.
#' 
#' @details
#' How to use this help:
#' \itemize{
#' \item For all functions, type the following in your R console:
#'     \itemize{
#'     \item \code{?functionName} - Displays more detailed and specific documentation for each function.
#'     \item \code{functionName} (without parentheses) - Displays the raw code of the function.
#'     }
#' }
#'
#' Full list of available functions:
#' \itemize{
#' \item Charting/Table Functions:
#'    \itemize{ 
#'    \item \code{\link{createControlChart}} (Updated 2018-11-29)
#'    \itemize{\item Template for creating Minitab-like control charts.}
#'    \item \code{\link{createPivotTable}} (Updated 2018-05-24)
#'    \itemize{\item Create pivot tables from raw data.}
#'    \item \code{\link{createVariationChart}} (Updated 2017-09-11)
#'    \itemize{\item Template for creating charts showing deviation from a value.}
#'    \item \code{\link{formatPivotTable}} (Updated 2017-09-05)
#'    \itemize{\item Prepare pivot tables for image export/output.}
#'    }
#'    
#' \item Text/Format Functions:
#'    \itemize{
#'    \item \code{\link{beginsEndsWith}} (Updated 2017-05-08)
#'    \itemize{\item Shortcut to see if a string begins or ends with a specific value.}
#'    \item \code{\link{findTextFiles}} (Updated 2018-10-02)
#'    \itemize{\item Search a directory for files containing speicfic text.}
#'    \item \code{\link{loadTextFromFile}} (Updated 2018-04-03)
#'    \itemize{\item Reads in a text file to a variable. Good for loading query files.}
#'    \item \code{\link{replaceMultInStr}} (Updated 2017-05-08)
#'    \itemize{\item Perform multiple, simultaneous find/replace operations on a string.}
#'    \item \code{\link{scanTextFiles}} (Updated 2018-03-22)
#'    \itemize{\item Read files to find specific items containing specific text.}
#'    \item \code{\link{simpleFormat}} (Updated 2019-03-01)
#'    \itemize{\item Shortcut to convert values to various number formats.}
#'    }
#'    
#' \item Database/Query Functions:
#'    \itemize{
#'    \item \code{\link{isolateLatestIterationICNs}} (Updated 2018-05-07)
#'    \itemize{\item Filter Bill Review data to only latest iteration ICNs.}
#'    \item \code{\link{locationTiming}} (Updated 2018-06-19)
#'    \itemize{\item Read Bill Review data to calculate timing between locations.}
#'    \item \code{\link{parseCLOB}} (Updated 2019-09-24)
#'    \itemize{\item Allows selective filtering/cleanup of CLOB objects when returned raw from a query.}
#'    \item \code{\link{runSQLQuery}} (Updated 2017-10-04)
#'    \itemize{\item Properly connect to, query from, and close connection to a database.}
#'    \item \code{\link{runSQLQuerySplit}} (Updated 2018-07-09)
#'    \itemize{\item Run a series of \code{runSQLQuery} commands over a given date range.}
#'    \item \code{\link{saveDataToTempTable}} (Updated 2017-08-10)
#'    \itemize{\item Write data to a temporary table in an Oracle database with optional indexing.}
#'    }
#'    
#' \item Miscellaneous Functions:
#'    \itemize{
#'    \item \code{\link{getPrevWeekday}} (Updated 2017-06-19)
#'    \itemize{\item Find the previous weekday date (ignores weekends) given a starting date.}
#'    \item \code{\link{getUserDates}} (Updated 2018-10-02)
#'    \itemize{\item Pop up form to collect dates input by the user.}
#'    \item \code{\link{getUserPass}} (Updated 2017-06-15)
#'    \itemize{\item Pop up form to collect username and masked password.}
#'    \item \code{\link{progress.console}} (Updated 2017-07-11)
#'    \itemize{\item Display a progress bar in the console window.}
#'    \item \code{\link{TAT}} (Updated 2019-01-15)
#'    \itemize{\item Calculates the amount of time between two dates.}
#'    \item \code{\link{writeLog}} (Updated 2017-09-05)
#'    \itemize{\item Extends the \code{\link{writeLines}} function to add a timestamp when logging to console/\code{\link{sink}}.}
#'    }
#' }
#' 
#' Latest Updates:
#' \itemize{
#' \item 2019-09-24:
#'    \itemize{
#'    \item Added new function \code{\link{parseCLOB}}, which allows selective filtering/cleanup of CLOB objects when returned raw from a query.
#'    }
#' \item 2019-07-11:
#'    \itemize{
#'    \item Added skipping/bypassing \code{\link{t.test}} checking in \code{\link{createControlChart}} when there is not enough data to perform the test.
#'    }
#' \item 2019-05-21:
#'    \itemize{
#'    \item Corrected some bad links (e.g. gpar() ==> grid::gpar()) that were created as a result of the recent refactoring from 2019-05-13. More corrections may be required.
#'        \itemize{
#'        \item \code{\link{createControlChart}}
#'        \item \code{\link{formatPivotTable}} 
#'        \item \code{\link{runSQLQuery}}
#'        }
#'    }
#' \item 2019-05-13:
#'    \itemize{
#'    \item Updated \code{\link{scanTextFiles}} (and by extension \code{\link{findTextFiles}}) to ignore case on filename. i.e. \code{'.txt'} and \code{'.TXT'} will both be found with and variation of the input.
#'    \item Significant changes to \code{\link{createControlChart}}, including adding new features for automatic flagging and some code style/efficiency refactoring. **\emph{This will likely break any previous usage of \code{createControlChart()}.}**
#'    }
#' \item 2019-03-01:
#'    \itemize{
#'    \item Updated/simplified \code{\link{simpleFormat}} currency output. (e.g. 'CurrencyK' output of '$12,345.68 k' now returns '$12,346 K')
#'    }
#' \item 2019-01-15:
#'    \itemize{
#'    \item Added 2019 holiday dates to \code{\link{TAT}}.
#'    \item Corrected issue with \code{sameDayCounts} parameter of \code{\link{TAT}} ignoring holidays, etc. and calculating as zero/negative.
#'    }
#' \item 2018-11-29:
#'    \itemize{
#'    \item *\emph{Really}* updated \code{\link{createControlChart}} to properly format y axis.
#'    \item Removed scientific notation output of \code{\link{simpleFormat}} values. Was a problem with very big numbers.
#'    }
#' \item 2018-10-02:
#'    \itemize{
#'    \item Updated \code{\link{createControlChart}} to properly format y axis.
#'    \item Changed depends/imports to allow for quicker loading. (Ongoing updates)
#'    \item Related to \code{fgui}/\code{tcltk}/\code{fasttime}.
#'       \itemize{
#'       \item \code{\link{findTextFiles}}
#'       \item \code{\link{getUserDates}}
#'       \item \code{\link{getUserPass}}
#'       \item \code{\link{TAT}}
#'       }
#'    }
#' }
#' 
#' @keywords documentation misc utilities
#' @export

TemplateHelp <- function() {
  ?TemplateHelp
}