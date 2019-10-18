#' Parse regular/standardized data from CLOB-style data.
#'
#' @param data A vector of character strings (one item per line) to parse. (See details)
#' @param dataHeaders A vector of character strings to identify useful fields within \code{data}.
#' @param headerNames (Optional, see details) Either a vector of character strings or a logical boolean (\code{TRUE}/\code{FALSE}).
#'
#' @details
#' \code{parseCLOB} will take a vector of character string elements and attempt to parse them into a more usable format of data.
#'
#' You may supply \code{data} as a single element vector of a path/filename. If a full path is provided, the function will load that file. If only a name is provided, the function will attempt to load that named file from the current working directory.
#'
#' The parameter \code{headerNames} determines how to label the final data fields. The default is \code{TRUE} but can take either a vector of names or a logical boolean argument. When a character vector is supplied, the values given will be used in place of the column labels and must contain the same number of elements as dataHeaders. When a logical boolean is supplied, a \code{TRUE} input will use the values from \code{dataHeaders} as names, while a \code{FALSE} input will use the default column names generated (\code{V1,V2,...,VN}). Output fields will be ordered the same as the \code{headers} provided as inputs to this function.
#'
#' Strings provided in \code{dataHeaders} should contain all characters of the field header/prefix.
#' For example, if the value is 'FIELD - 123456', using \code{'FIELD - '} would return the value \code{'123456'}, whereas \code{'FIELD'} would yield \code{' - 123456'}.
#' Currently, this function will only correcty match and return values that begin with the specified \code{dataHeaders}. This may be expanded in the future to account for other scenarios.
#'
#' @keywords methods
#' @return A \code{\link{data.table}}, providing the requested fields and applicable values for each.
#'
#' @export
#' @examples
#' # Define headers in exmaple data
#' allHeaders <- paste0(c('ABC_ID','DEF_ID',LETTERS[1:5]),' - ')
#' # Create example data
#' set.seed(1)
#' c1 <- paste0(allHeaders[1], rep(1:5, 5))
#' c2 <- paste0(allHeaders[2], rep(1:5, each = 5))
#' c3 <- paste0(allHeaders[3], round(rnorm(25),2))
#' c4 <- paste0(allHeaders[4], round(rnorm(25),2))
#' c5 <- paste0(allHeaders[5], round(rnorm(25),2))
#' c6 <- paste0(allHeaders[6], round(rnorm(25),2))
#' c7 <- paste0(allHeaders[7], round(rnorm(25),2))
#' dtCLOB <- paste0(t(cbind(c1,c2,c3,c4,c5,c6,c7)))
#'
#' # Define headers we want to extract
#' extractHeaders <- allHeaders[c(1,3,5,6)]
#' parsedData <- parseCLOB(data = dtCLOB, dataHeaders = extractHeaders, headerNames = c('ID','A','C','D'))
#'
#' # Display the results
#' parsedData
#' #    ID     A     C     D
#' # 1:  1 -0.63   0.4  0.29
#' # 2:  2  0.18 -0.61 -0.44
#' # 3:  3 -0.84  0.34     0
#' # 4:  4   1.6 -1.13  0.07
#' # 5:  5  0.33  1.43 -0.59
#' # 6:  1 -0.82  1.98 -0.57
#' # 7:  2  0.49 -0.37 -0.14
#' # ...
#'
#' # Example loading a text file
#' parsedData <- parseCLOB(data = 'textfile.txt', dataHeaders = c('Col1', 'Col2'))


parseCLOB <- function(data, dataHeaders, headerNames = TRUE) {
  if (!is.logical(headerNames)) {
    if (length(headerNames) != length(dataHeaders)) {
      stop('When headerNames is supplied, it must contain the same number of elements as dataHeaders.')
    }
  }

  if (length(data) == 1) {
    data <- scan(data, 'character', sep = '\n')
  }

  combinedHeaders <- paste0('^',dataHeaders, collapse = '|')
  dtSub <- lapply(data, function(x){
    if (grepl(combinedHeaders,x)) return(x)
  })
  filterItems <- which(sapply(dtSub, is.null))
  if (length(filterItems) > 0) {
    dtSub <- dtSub[-filterItems]
  }

  columns <- lapply(dataHeaders, function(x){
    colPrep <- dtSub[grep(paste0('^', x), dtSub)]
    colPrep <- sapply(substr(colPrep, nchar(x) + 1, nchar(colPrep)),list)
    return(colPrep)
  })

  ### TODO: Check if all lists have same number of elements and adjust if needed.
  colLens <- unlist(lapply(columns,length))
  if (!all(colLens[1] == colLens)) {
    check1 <- which(colLens[1] != colLens)
    check2 <- which(colLens[length(colLens)] != colLens)
    checkT <- table(c(check1, check2))
    checkN <- max(checkT)
    errorItems <- unname(which(checkT != checkN))
    stop(
      'Mismatched column counts for {',
      paste0("'",dataHeaders[errorItems],"'",collapse = ', '),
      '}. Please check your data/column headers and try again or share this error with James.'
    )
  }

  dtFinal <- as.data.table(t(rbindlist(columns)))

  if (is.logical(headerNames)) {
    if (headerNames) {
      names(dtFinal) <- dataHeaders
    }
    # else leave names with default "V" values.
  } else {
    names(dtFinal) <- headerNames
  }

  return(dtFinal)
}
