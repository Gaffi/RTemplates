#' @title 
#' Generate Location Timings
#' @description
#' Parse and group item history to get total time per individual instance of group activity (rather than grouping across all instances of a group as a single unit).
#' 
#' @param locData A \code{\link{data.table}} containing the data to parse.
#' @param ICN A \code{\link{character}} vector/string specifying the name of the ICN field. Defaults to DB-standard 'ICN'.
#' @param Location A \code{\link{character}} vector/string specifying the name of the location field. Defaults to DB-standard 'SYST_LCTN_ID'.
#' @param TimeStamp A \code{\link{character}} vector/string specifying the name of the timestamp field. Defaults to DB-standard 'LAST_ACTN_TMSTAMP'.
#' @param units A \code{\link{character}} vector/string indicating the units/format for the output resulting TAT. Defaults to 'days'.
#' @keywords methods
#' @return A \code{\link{data.table}} or \code{\link{list}} containing the output of the queries.
#' @seealso \code{\link{difftime}} \code{\link{merge}}
#' @details 
#' \code{locData} must contain at least three columns (names can vary):
#' \itemize{
#' \item{\code{ICN}}
#' \item{\code{Location}}
#' \item{\code{TimeStamp}}
#' }
#' Other columns may be included, but will be stripped from the data and not returned.
#' 
#' See \code{\link{seq.Date}} for further information about the \code{units} parameter.
#' @export
#' @examples 
#' 
#' dt <- runSQLQuery('History.sql', DB='DBName',
#'             verbose = TRUE)
#' dtLocTime <- locationTiming(dt)
#' dtLocTimeAlt <- locationTiming(
#'   dt,
#'   ICN = 'Control_Number',
#'   Location = 'Location', 
#'   TimeStamp = 'Timestamp',
#'   units = 'hours')


locationTiming <- function(locData, 
                           ICN = 'ICN', 
                           Location = 'SYST_LCTN_ID', 
                           TimeStamp = 'LAST_ACTN_TMSTAMP',
                           units = 'days') {
  
  if (!is.character(ICN)) {stop('ICN must be a character string.')}
  if (!is.character(Location)) {stop('Location must be a character string.')}
  if (!is.character(TimeStamp)) {stop('TimeStamp must be a character string.')}
  if (!is.data.table(locData)) {
    tryCatch({locData <- data.table(locData)},
             error = function(e){
               stop('locData was not a usable data format. Please supply a data.table or data.table-convertable dataset.')
             })
  }
  if(!ICN %in% names (locData)){stop(paste0('ICN column ("',ICN,'") not found in data.'))}
  if(!Location %in% names (locData)){stop(paste0('Location column ("',Location,'") not found in data.'))}
  if(!TimeStamp %in% names (locData)){stop(paste0('TimeStamp column ("',TimeStamp,'") not found in data.'))}
  
  timData <- locData[,c(ICN, Location, TimeStamp), with = FALSE]
  setnames(timData,1:3,c('ICN','SYST_LCTN_ID','LAST_ACTN_TMSTAMP'))
  timData[,ICN:=as.character(ICN)]
  timData <- timData[order(LAST_ACTN_TMSTAMP)][order(ICN)]
  timData[,TargetNum:=rowid(ICN)]
  timData[,SourceNum:=TargetNum - 1]
  timDataMerge <- merge(timData, timData,
                       by.x = c(
                         "ICN", "TargetNum"),
                       by.y = c(
                         "ICN", "SourceNum"),
                       suffixes = c('.Source','.Target'))
  
  #silver bullet, rleid
  timDataMerge[,LOC_GRP := rleid(SYST_LCTN_ID.Source), by = ICN]
  
  timDataMerge <- timDataMerge[,c(1,3,4,7,9)]
  
  timDataMerge <- timDataMerge[
    timDataMerge[
      ,
      list(STOPTIME=max(LAST_ACTN_TMSTAMP.Target)),
      .(ICN, LOC_GRP)],
    on=.(ICN, LOC_GRP)]
  
  timDataMerge <- timDataMerge[
    timDataMerge[
      ,
      list(STARTTIME=min(LAST_ACTN_TMSTAMP.Source)),
      .(ICN, LOC_GRP)],
    on=.(ICN, LOC_GRP)]
  
  timDataMerge <- timDataMerge[,c(1:2,5:7)]
  
  timDataMerge[,LOC_TIME := difftime(
    STOPTIME, STARTTIME, 
    units = units)]
  
  names(timDataMerge)[2]<-'LOC'
  
  return(unique(timDataMerge)[,.(ICN,LOC,LOC_TIME)])
}
