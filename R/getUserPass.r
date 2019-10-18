#' Collect Username and Masked Password
#' 
#' Reusable code to more securely accept usernames/passwords, rather than having to hard-code this information.
#' Typically used for logging in to the database, though this could apply to any other uses as well.
#' @param helperLabel A character string stating the name of the resource to connect to.
#' @param helperUsername A character string of the label to prompt for the username.
#' @param helperPassword A character string of the label to prompt for the password.
#' @keywords IO methods
#' @return A vector containing two character strings. [1] = username; [2] = password
#' @export
#' @examples 
#' # Base usage
#' loginInfo <- getUserPass()
#' username <- loginInfo[1]
#' userpass <- loginInfo[2]
#' # Custom labels
#' loginInfo <- getUserPass(helperLabel = 'My Database', 
#'                          helperUsername = 'Enter login for My Database:',
#'                          helperPassword = 'Enter password for My Database:')
#' username <- loginInfo[1]
#' userpass <- loginInfo[2]

getUserPass <- function(helperLabel = NA, helperUsername = NA, helperPassword = NA){
  if(!is.na(helperLabel)) {
    if(class(helperLabel) != 'character') {
      stop('helperLabel must be a character string.')
    }
  }
  if(!is.na(helperUsername)) {
    if(class(helperUsername) != 'character') {
      stop('helperUsername must be a character string.')
    }
  }
  if(!is.na(helperPassword)) {
    if(class(helperPassword) != 'character') {
      stop('helperPassword must be a character string.')
    }
  }
  #Create window and variables to be returned
  wnd <- tcltk::tktoplevel()
  userVar <- tcltk::tclVar("")
  passVar <- tcltk::tclVar("")
  
  #Set window title
  tcltk::tktitle(wnd) <- paste0(
    if(!is.na(helperLabel)){helperLabel},
      if(!is.na(helperLabel)){' - '},
      "Username/Password")
  
  #User Name Label
  tcltk::tkgrid(tcltk::tklabel(wnd, 
                 text = if(is.na(helperUsername)) {
                   "Enter username:"
                 } else {
                   helperUsername
                 }
                 ), padx = 10, pady = 10)
  
  #User Name Box
  tcltk::tkgrid(userBox <- tcltk::tkentry(wnd, textvariable = userVar), padx = 10, pady = 10)
  
  #Label
  tcltk::tkgrid(tcltk::tklabel(wnd, 
                 text = if(is.na(helperPassword)) {
                   "Enter password:"
                 } else {
                   helperPassword
                 }
                 ), padx = 10, pady = 10)
  
  #Password Box
  tcltk::tkgrid(passBox <- tcltk::tkentry(wnd, textvariable = passVar, show="*"), padx = 10, pady = 10)
  
  #Hitting return will submit the form
  tcltk::tkbind(passBox, "<Return>", function() tcltk::tkdestroy(wnd))
  tcltk::tkbind(userBox, "<Return>", function() tcltk::tkdestroy(wnd))
  
  #OK button
  tcltk::tkgrid(tcltk::tkbutton(wnd, text = "OK", command = function() tcltk::tkdestroy(wnd)), padx = 10, pady = 10)
  
  #Wait for user to click OK
  tcltk::tkraise(wnd)
  tcltk::tkwait.window(wnd)
  
  password <- tcltk::tclvalue(passVar)
  username <- tcltk::tclvalue(userVar)
  return(c(username, password))
}  
