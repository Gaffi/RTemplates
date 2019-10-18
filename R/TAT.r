#' Calculate Time Between Two Dates
#' 
#' Calculates the amount of time between two date/timestamps, reported in days or time with options for special exclusions.
#' @param startDate A \code{\link{Date}} or datetime (\code{\link{POSIXct}}) (or a vector of same) specifying the earliest date/time in the period.
#' @param endDate A \code{\link{Date}} or datetime (\code{\link{POSIXct}}) (or a vector of same) specifying the latest date/time in the period.
#' @param excludeWeekends A logical boolean (\code{TRUE}/\code{FALSE}) stating whether weekend dates should be counted (\code{FALSE}) or excluded (\code{TRUE}). Defaults to \code{TRUE}.
#' @param excludeHolidays A logical boolean (\code{TRUE}/\code{FALSE}) stating whether holiday dates should be counted (\code{FALSE}) or excluded (\code{TRUE}). Defaults to \code{TRUE}.
#' @param specialExclusions A vector of \code{\link{Date}} values to be excluded. Can overlap weekend/default holidays.
#' @param sameDayCounts A logical boolean (\code{TRUE}/\code{FALSE}) stating whether same-day spans should be counted as 0 (\code{FALSE}) or 1 (\code{TRUE}). Defaults to \code{TRUE}.
#' @param calcDays <TO BE IMPLEMENTED> A logical boolean (\code{TRUE}/\code{FALSE}) stating whether the value returned should be in days (\code{TRUE}) or time (\code{FALSE}). Defaults to \code{TRUE}.
#' @param dayStartTime <TO BE IMPLEMENTED>
#' @param dayEndTime <TO BE IMPLEMENTED>
#'
#' @details
#' Calculates turn around time between two dates, either as whole-day numbers or (not yet implemented) partial days/hours.
#' 
#' If dates are not provided to the function, a pop up will appear asking for the values to be entered.
#' 
#' @keywords methods
#' @return A \code{\link{numeric}} value, giving the number of days, or a time value.
#' 
#' @export
#' @examples 
#' TAT()
#' #Varies, depending upon user input.
#' TAT(as.Date('2017-07-01'),as.Date('2017-07-31'))
#' #19
#' TAT(as.Date('2017-07-01'),as.Date('2017-07-31'), excludeWeekends = FALSE)
#' #29
#' TAT(as.Date('2017-07-01'),as.Date('2017-07-31'), excludeWeekends = FALSE, excludeHolidays = FALSE)
#' #31
#' TAT(as.Date('2017-07-01'),as.Date('2017-07-31'), excludeHolidays = FALSE)
#' #21
#' #Run for multiple ranges. Good for getting TAT for a series of items.
#' sDates <- c(as.Date('2018-01-01'), as.Date('2018-02-01'))
#' eDates <- c(as.Date('2018-02-01'), as.Date('2018-03-01'))
#' TAT(sDates, eDates)
#' #23, 21

TAT <- function(startDate = NA, endDate = NA, 
                excludeWeekends = TRUE, excludeHolidays = TRUE,
                specialExclusions = NA, sameDayCounts = TRUE,
                calcDays = TRUE, dayStartTime = NA, dayEndTime = NA) {
  
  if (is.na(startDate) | is.na(endDate)) {
    if (is.na(startDate) & is.na(endDate)) {
      userDates <- getUserDates(2,"R")
      if (is.na(userDates[1]) | is.na(userDates[2])) {
        stop('No valid dates supplied. Please try again.')
      }
      startDate <- userDates[1]
      endDate <- userDates[2]
    } else {
      stop('startDate and endDate must either both be supplied or both left out. Please try again.')
    }
  }
  
  holidays <- as.Date(c(
    '2012-01-02', '2012-01-16', '2012-05-28', '2012-07-04',
    '2012-09-03', '2012-11-12', '2012-11-22', '2012-11-23',
    '2012-12-25', '2013-01-01', '2013-01-21', '2013-05-27',
    '2013-07-04', '2013-09-02', '2013-11-11', '2013-11-28',
    '2013-11-29', '2013-12-25', '2014-01-01', '2014-01-20',
    '2014-05-26', '2014-07-04', '2014-09-01', '2014-11-27',
    '2014-11-28', '2014-12-25', '2015-01-01', '2015-01-19',
    '2015-05-25', '2015-07-03', '2015-09-07', '2015-11-26',
    '2015-11-27', '2015-12-25', '2016-01-01', '2016-01-18',
    '2016-05-30', '2016-07-04', '2016-09-05', '2016-11-24',
    '2016-11-25', '2016-12-26', '2017-01-02', '2017-01-16',
    '2017-05-29', '2017-07-03', '2017-07-04', '2017-09-04',
    '2017-11-23', '2017-11-24', '2017-12-25', '2018-01-01',
    '2018-01-15', '2018-05-28', '2018-07-04', '2018-09-03',
    '2018-11-22', '2018-11-23', '2018-12-25', '2019-01-01',
    '2019-01-21', '2019-05-27', '2019-07-04', '2019-09-02',
    '2019-11-28', '2019-11-29', '2019-12-25', '2020-01-01'
  ))
  
  if (calcDays) {
    vecSeqPOSIXt <- Vectorize(seq.POSIXt, USE.NAMES=TRUE)
    if (length(startDate) > 1) {
      if (length(unique(startDate)) > 1) {
        ttlDays <- vecSeqPOSIXt(fasttime::fastPOSIXct(startDate), fasttime::fastPOSIXct(endDate), '1 day')
        ttlDays <- lapply(ttlDays, as.Date)
      } else {
        #ttlDays <- list(rep(seq.Date(startDate[1],endDate)
      }
    } else {
      ttlDays <- seq(startDate, endDate, 'day')
    }
    
    if(class(ttlDays) == 'matrix'){
      ttlDays <- lapply(1:ncol(ttlDays), function(x) {
        as.POSIXct(ttlDays[,x], tz = 'GMT', origin = "1970-01-01 00:00.00 GMT")
      })
    }
    
    hol <- spec <- wkEnd <- lapply(ttlDays, function(x) {
      rep(FALSE, length(x))
    })
    
    if (excludeWeekends) {
      wkEnd <- lapply(ttlDays, function(x) {
        weekdays(x) %in% c('Saturday', 'Sunday')
      })
    }
    
    if (excludeHolidays) {
      hol <- lapply(ttlDays, function(x) {
        x %in% holidays
      })
    }
    
    if (!is.na(specialExclusions)) {
      spec <- lapply(ttlDays, function(x) {
        x %in% specialExclusions
      })
    }
    
    result <-
      unlist(
        lapply(
          1:length(ttlDays), 
          function(x) {
            length(ttlDays[[x]][!(wkEnd[[x]] | hol[[x]] | spec[[x]])])
          }
        ) 
      )
    
    if (length(startDate) == 1) {
      result <- sum(unlist(result))
    }
    
    # If the startDate fits into any exclusion groups, ignore sameDayCounts flag.
    sameDayAdj <- unlist(
      lapply(
        1:length(result), 
        function(x){
          if((wkEnd[[x]][1] | hol[[x]][1] | spec[[x]][1])) {
            return(0)
          } else {
            # Otherwise, use the user-specified adjustment.
            return(as.integer(!sameDayCounts) * 1)
          }
        }
      )
    )
    
    return(result - sameDayAdj)
  } else {
    # Placeholder for potential future hours calculation.
  }
  
}
