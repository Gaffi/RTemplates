#' @title 
#' Find files containing some text.
#' @description
#' Wrapper/GUI for \code{\link{scanTextFiles}}, parameters for which will be gathered through a user form.
#' 
#' @details 
#' When saving a text file of results, the file (\code{FoundFiles.txt}) will be saved in the path on the selection form.
#' If not saving results, the user form is closed/cancelled, or no results are found, then \code{NA} is returned by the function instead of a list.
#' 
#' @keywords methods
#' @return A \code{\link{list}} of files found.
#' @seealso \code{\link{scanTextFiles}}
#' @export
#' @examples 
#' findTextFiles() # Return a list of files
#' findTextFiles(TRUE) # Save a text file with the list instead.

findTextFiles <- function() {
  varSearchDialog <- function() {
    # Create a variable to keep track of the state of the dialog window:
    # done = 0; If the window is active
    # done = 1; If the window has been closed using the OK button
    # done = 2; If the window has been closed using the Cancel button or destroyed
    done <- tcltk::tclVar(0)
    result <- list()

    submit <- function() {
      result[['path']] <<- if(tcltk::tclvalue(varDirSrchVal) == '') {NA} else {gsub('\\','/',tcltk::tclvalue(varDirSrchVal), fixed=TRUE)}
      result[['srchText']] <<- tcltk::tclvalue(varSrchVal)
      result[['ignoreCase']] <<- tcltk::tclvalue(varCaseVal)
      result[['subdirs']] <<- tcltk::tclvalue(varSubDVal)
      result[['extension']] <<- tcltk::tclvalue(varExtVal)
      result[['save']] <<- tcltk::tclvalue(varSaveVal)
      result[['savePath']] <<- if(tcltk::tclvalue(varDirSavVal) == '') {NA} else {gsub('\\','/',tcltk::tclvalue(varDirSavVal), fixed=TRUE)}
      tcltk::tclvalue(done) <- 1
    }
    
    cancel <- function() {
      result <- list()
      tcltk::tclvalue(done) <- 2
    }
    
    tt <- tcltk::tktoplevel()
    tcltk::tkwm.title(tt, 'Search Parameters')	
    
    # Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, 
    # assign 2 to done.
    tcltk::tkbind(tt,"<Destroy>",function() tcltk::tclvalue(done)<-2)
    tcltk::tkbind(tt,"<Return>", cancel)
    tcltk::tkbind(tt,"<Escape>", submit)
    
    varDirSrchVal <- tcltk::tclVar("P:/")
    varDirSavVal <- tcltk::tclVar("P:/")
    varSrchVal <- tcltk::tclVar("text")
    varExtVal <- tcltk::tclVar(".txt")
    varSubDVal <- tcltk::tclVar(FALSE)
    varCaseVal <- tcltk::tclVar(TRUE)
    varSaveVal <- tcltk::tclVar(TRUE)
    
    varDirectoryDialogSearch <- function() {
      tcltk::tclvalue(varDirSrchVal) <- tcltk::tk_choose.dir(caption = "Select a folder to search")
    }
    
    varDirectoryDialogSave <- function() {
      tcltk::tclvalue(varDirSavVal) <- tcltk::tk_choose.dir(caption = "Select a folder to save to")
    }
    
    tcltk::tkgrid(tcltk::tklabel(tt, text = 'Search Directory:'), 
                  tcltk::tkentry(tt, textvariable = varDirSrchVal),
                  tcltk::tkbutton(tt, text = "Browse...", command = varDirectoryDialogSearch),
                  pady = 10, padx = 10, columnspan = 4)
    
    tcltk::tkgrid(tcltk::tklabel(tt, text = 'Search Term:'), 
                  tcltk::tkentry(tt, textvariable = varSrchVal),
                  tcltk::tklabel(tt, text = 'File Extension:'), 
                  tcltk::tkentry(tt, textvariable = varExtVal),
                  pady = 10, padx = 10, columnspan = 4)
    
    tcltk::tkgrid(tcltk::tklabel(tt, text = 'Check subdirectories?'), 
                  tcltk::tkcheckbutton(tt, variable = varSubDVal),
                  tcltk::tklabel(tt, text = 'Ignore case?'), 
                  tcltk::tkcheckbutton(tt, variable = varCaseVal),
                  pady = 10, padx = 10, columnspan = 4)
    
    tcltk::tkgrid(tcltk::tklabel(tt, text = 'Save results to text file?'), 
                  tcltk::tkcheckbutton(tt, variable = varSaveVal),
                  pady = 10, padx = 10, columnspan = 4)
    
    tcltk::tkgrid(tcltk::tklabel(tt, text = 'Save Directory:'), 
                  tcltk::tkentry(tt, textvariable = varDirSavVal),
                  tcltk::tkbutton(tt, text = "Browse...", command = varDirectoryDialogSave),
                  pady = 10, padx = 10, columnspan = 4)
    
    submit.but <- tcltk::tkbutton(tt, text="Submit", command=submit)
    cancel.but <- tcltk::tkbutton(tt, text='Cancel', command=cancel)
    
    tcltk::tkgrid(submit.but, cancel.but, pady = 10, padx = 10, columnspan = 4)
    tcltk::tkfocus(tt)
    
    # Do not proceed with the following code until the variable done is non-zero.
    #   (But other processes can still run, i.e. the system is not frozen.)
    tcltk::tkwait.variable(done)
    
    tcltk::tkdestroy(tt)
    return(result)
  }
  
  scanOpts <- varSearchDialog()
  
  if (length(scanOpts) > 0) {
    foundFiles <- scanTextFiles(
      srchText = scanOpts[['srchText']], 
      path = scanOpts[['path']], 
      ignoreCase = as.logical(as.numeric(scanOpts[['ignoreCase']])),
      subdirs = as.logical(as.numeric(scanOpts[['subdirs']])),
      extension = scanOpts[['extension']]
    )
    if (as.logical(as.numeric(scanOpts[['save']]))) {
      filePath <- scanOpts[['savePath']]
      fpSplit <- strsplit(filePath,'')
      if (fpSplit[length(fpSplit)] != '/') {filePath <- paste0(filePath,'/')}
      fileName <- paste0(filePath, 'FoundFiles.txt')
      write('', fileName)
      lapply(foundFiles, write, fileName, append=TRUE)
      return(NA)
    } else {
      return(foundFiles)
    }
  } else {
    writeLog('No results found.')
    return(NA)
  }
}
