#' Add quality flags
#'
#' Function that adds flags for system control probes and transcripts that are located outside cell boundaries. Required to detect transcript dispersion.
#'
#' @param transcripts a data frame containing raw transcripts
#'
#' @return A data frame containing raw transcripts with CellBoundaryFlag and SystemControlFlag columns.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' transcripts <- setQualityFlags(transcripts)
#' }
setQualityFlags <- function(transcripts) {

  # Set cell boundary flag
  transcripts$CellBoundaryFlag <- "PASS"
  transcripts$CellBoundaryFlag[transcripts$CellId == 0] <- "FAIL"

  # Set system controls flag
  transcripts$SystemControlFlag <- "PASS"
  transcripts$SystemControlFlag[grep("SystemControl", transcripts$target)] <- "FAIL"

  return(transcripts)

}
