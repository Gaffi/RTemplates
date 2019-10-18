#' @title 
#' Find files containing some text.
#' @description
#' Reads through files in a directory and returns a list of the text files found that contain a specified text string.
#' 
#' @param srchText A character string of the text to search for.
#' @param path A valid path that contains files to search through.
#' @param ignoreCase A logical boolean (\code{TRUE}/\code{FALSE}) that specifies whether text case should be matched exactly or ignored. Defaults to \code{TRUE}.
#' @param subdirs A logical boolean (\code{TRUE}/\code{FALSE}) stating whether or not subdirectories of \code{path} should be scanned. Defaults to\code{FALSE}.
#' @param extension A character vector of extensions to limit file search.
#' @details 
#' If \code{path} is not specified, R's working directory (\code{\link{getwd}}) will be used.
#' 
#' When specifying file types/extensions with the \code{extension} argument, you may pass a single string (\code{'.txt'}) or a list of multiple options (\code{c('.txt','.csv')})
#' Extension strings need not have the preceding period; either with or without will work. i.e. \code{'.txt'} is equivalent to \code{'txt'}.
#' 
#' @keywords methods text
#' @return A list of files contatining the value assigned to \code{srchText}.
#' @seealso \code{\link{loadTextFromFile}}, \code{\link{list.files}}
#' @export
#' @examples 
#' scanTextFiles('default parameters')
#' scanTextFiles('specific path', path = 'P:/test/') # Note the direction of slashes.
#' scanTextFiles('case sensitive, all subdirectories, specific extensions', ignoreCase = FALSE, subdirs = TRUE, extention = c('.txt','csv','sql','.r'))

scanTextFiles <- function(srchText, path = NA, ignoreCase = TRUE, 
                          subdirs = FALSE, extension = NA) {
  foundFiles <- c()
  extPattern <- ''
  if (is.na(path)) path <- getwd()

  for (curExt in extension) {
    if (!is.na(curExt)) {
      if (grepl('\\.', curExt)) {
        curExt <- gsub('\\.','\\\\.',curExt)
      } else {
        curExt <- paste0('\\.', curExt)
      }
      curExt <- paste0(curExt,'$')
      if (extPattern == '') {
        extPattern <- curExt
      } else {
        extPattern <- paste(extPattern, curExt, sep = '|')
      }
    }
  }
  
  writeLog(paste0('Searching in ', path ,' for files to scan.\n--This may take a moment...'))
  scanFiles <- list.files(path = path, recursive = subdirs, full.names = TRUE,
                          include.dirs = TRUE, pattern = extPattern,
                          ignore.case = TRUE)
  numToScan <- length(scanFiles)
  writeLog(paste0('Found ', numToScan, ' files to scan. Please wait...'))
  fileCount <- 0
  for (curFile in scanFiles) {
    fileCount <- fileCount + 1
    progress.console(progCurrent = fileCount, progTotal = numToScan, 
                     appendMsg = paste0(
                       ' (',length(foundFiles),
                       ' ',if(length(foundFiles) == 1) {'match'} else {'matches'},
                       ' found so far.)'))
    if (grepl(srchText, loadTextFromFile(curFile), ignore.case = ignoreCase)) {
      foundFiles <- c(foundFiles, curFile)
    }
  }
  cat('\n')
  writeLog('File scanning process complete.')
  return(foundFiles)
}
