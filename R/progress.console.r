#' @title 
#' Display a progress bar in the console window
#' @description
#' Shows a progress bar on the text console that updates as needed.
#' 
#' @param progCurent A numeric value giving the current step of the progress.
#' @param progTotal A numeric value giving the total step
#' @param dispLength An integervalue that specifies how many characters to expand the prgress bar to. Defaults to 30.
#' @param showPercent A logical boolean (\code{TRUE}/\code{FALSE}) stating whether or not a percentage should be displayed along side the raw progress count. Defaults to \code{TRUE}.
#' @param appendMsg A character vector of additional informational text to append to the end of the progress bar. Optional.
#' 
#' @keywords methods text
#' @return Prints the progress bar using text to the console window.
#' @export
#' @examples 
#' # Basic Example:
#' totalSteps <- 25
#' for(i in 1:totalSteps) {
#'    progress.console(i,totalSteps)
#'    Sys.sleep(.5)
#' }
#' 
#' # More Features:
#' totalSteps <- 37
#' for(i in 1:totalSteps) {
#'    progress.console(progCurrent = i, progTotal = totalSteps,
#'       dispLength = 45, showPercent = FALSE,
#'       appendMsg = paste0(' - Other ','messages as needed - ',i)
#'    )
#'    Sys.sleep(.5)
#' }

progress.console <- function(progCurrent, progTotal, 
                             dispLength = 30, showPercent = TRUE,
                             appendMsg = NA) {
  flush.console()
  progCount <- floor(progCurrent/progTotal * dispLength)
  progPercent <- if(showPercent) {
    paste0(" (", simpleFormat(progCurrent/progTotal, 'Percent2') ,")")
  } else {''}
  cat(paste0("\r", progCurrent, "/", progTotal, progPercent, " - ", 
             paste0("|", paste(rep.int("=", progCount), collapse = ""), 
                    paste(rep.int(".", dispLength - progCount), collapse = ""), 
                    "|")))
  if (!is.na(appendMsg)) {cat(appendMsg)}
  cat(rep(' ', dispLength * 3))
}
