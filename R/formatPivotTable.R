#' @title 
#' Format Excel-Like Pivot Tables
#' @description
#' Format pivoted data to print/display similiar to an Excel pivot.
#' 
#' @param pvData The formatted data to be plotted.
#' @param pvTitle A optional character string to use as the table's overall title.
#' @param pvSubT A optional character string to use as the table's subtitle.
#' @param txtScale A numeric value representing the relative size of the values in the table.
#' @param hdrScale A numeric value representing the relative size of the column headers.
#' @param ttlScale A numeric value representing the relative size of title (if supplied).
#' @param subTtlScale A numeric value representing the relative size of the subtitle (if supplied).
#' @param width A numeric value representing the width (in inches) the final table should fill.
#' @param height A numeric value representing the height (in inches) the final table should fill.
#' @details 
#' \code{pvData} may be a \code{\link{data.table}} or \code{\link{data.frame}} (possibly a matrix as well?).
#' It is not necessary include \code{pvTitle} when using \code{pvSubT}, as the function will format according to whichever arrangement is presented. However, scaling for both is dependent upon the explict argument given (\code{ttlScale} and \code{subTtlScale}, respectively.). However, both used together is also acceptable.
#' If \code{width} and/or \code{height} are omitted, the table will be sized to fit the data.
#' @keywords methods
#' @return A \code{\link{tableGrob}} prepared to print to pdf/image/other output format.
#' @seealso \code{\link{createPivotTable}}
#' @export
#' @examples 
#' pTable <- formatPivotTable(pvData,'TITLE','Subtitle')
#' pTable <- formatPivotTable(pvData,'TITLE', ttlScale = 2)
#' pTable <- formatPivotTable(pvData, txtScale = 0.5, hdrScale = 0.75)
#' 


formatPivotTable <- function(pvData, pvTitle = NA, pvSubT = NA, 
                             txtScale = 0.75, hdrScale = 1.0,
                             ttlScale = 1.0, subTtlScale = 1.0,
                             width = NA, height = NA){
  
  pvTheme <- ttheme_default(
    core = list(fg_params=list(cex = txtScale)),
    colhead = list(fg_params=list(cex = hdrScale)),
    rowhead = list(fg_params=list(cex = hdrScale)))
  
  
  genTbl <- gridExtra::tableGrob(data.table(pvData),rows=NULL,theme=pvTheme)
  
  ttlWidth <- unit(0, 'in')
  
  if (!(is.na(pvTitle) & is.na(pvSubT))) {
    
    padding <- unit(0.5,"line")
    
    elemList <- list()
    
    if (!is.na(pvTitle)) {
      genTitle <- grid::textGrob(pvTitle, gp = grid::gpar(fontsize = ttlScale * 14))
      ttlWidth <- unit(
        max(
          convertWidth(ttlWidth,'in',valueOnly=TRUE),
          convertWidth(grid::grobWidth(genTitle),'in',valueOnly=TRUE)
        ),'in'
      )
      
      genTbl <- gtable_add_rows(genTbl, pos = 0,
                                heights = grid::grobHeight(genTitle) + padding)
      if (length(elemList) == 0) {
        elemList <- list(title=genTitle)
      } 
    }
    
    if (!is.na(pvSubT)) {
      genSubT <- grid::textGrob(pvSubT, gp = grid::gpar(fontsize = subTtlScale * 12))
      ttlWidth <- unit(
        max(
          convertWidth(ttlWidth,'in',valueOnly=TRUE),
          convertWidth(grid::grobWidth(genSubT),'in',valueOnly=TRUE)
        ),'in'
      )
      genTbl <- gtable_add_rows(genTbl, pos = 0,
                                heights = grid::grobHeight(genSubT) + padding)
      if (length(elemList) == 0) {
        elemList <- list(subtitle=genSubT)
      } else {
        oldnames <- names(elemList)
        elemList <- append(elemList, list(subtitle=genSubT))
        names(elemList) <- c(oldnames,'subtitle')
      }
    }
    
    if (length(elemList) == 0) {
      elemList <- list(grid::nullGrob())
    } else {
      oldnames <- names(elemList)
      elemList <- append(elemList,list(grid::nullGrob()))
      names(elemList) <- c(oldnames,'nullspot')
    }
    gtagl <- rep(1,length(elemList))
    gtagt <- c(1:(length(elemList)-1), nrow(genTbl))
    gtagr <- c((2:length(elemList)), 1)
    genTbl <- gtable::gtable_add_grob(genTbl, 
                              elemList,
                              t=gtagt,
                              l=gtagl,
                              r=ncol(genTbl)
                              )
  
  }
  
  if (!is.na(width)) {
    ttlWidth <- unit(width,'in')
  } 
  if (!is.na(height)) {
    genTbl$heights <- genTbl$height + unit(height/nrow(genTbl), "in")
  }
  
  missed <- convertWidth(sum(genTbl$widths), "in", valueOnly = TRUE) -
    convertWidth(ttlWidth, "in", valueOnly = TRUE)
  if (missed < 0) {
    genTbl$widths <- genTbl$widths + unit(1.05*(abs(missed)/ncol(genTbl)), "in")
  }
  
  return(genTbl)
}

