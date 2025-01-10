#' Clean raw transcripts
#'
#' Function to discard transcripts flagged by setQualityFlags() and setZsliceFlag().
#'
#' @param transcripts a data frame containing raw transcripts
#' @param useCellBoundaryFlag a logical to determine if the CellBoundaryFlag column should be used for cleaning the data
#' @param useSystemControlFlag a logical to determine if the SystemControlFlag column should be used for cleaning the data
#' @param useZstackFlag a logical to determine if the ZstackFlag column should be used for cleaning the data
#'
#' @return A data frame containing only high confidence transcripts.
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'
#' cleaned_transcripts <- cleanTranscripts(transcripts)
#' }

cleanTranscripts <- function(transcripts, useCellBoundaryFlag = TRUE, useSystemControlFlag = TRUE, useZstackFlag = TRUE) {

  # Check for CellBoundaryFlag
  if ( !("CellBoundaryFlag" %in% colnames(transcripts)) ) {

    stop("Missing CellBoundaryFlag. Please run setQualityFlags().")

  }

  # Check for SystemControlFlag
  if ( !("SystemControlFlag" %in% colnames(transcripts)) ) {

    stop("Missing SystemControlFlag. Please run setQualityFlags().")

  }

  # Check for ZstackFlag
  if ( !("ZstackFlag" %in% colnames(transcripts)) ) {

    stop("Missing ZstackFlag Please run setZsliceFlag().")

  }

  # Check useCellBoundaryFlag
  if ( useCellBoundaryFlag == TRUE ) {
    useCellBoundaryFlag <- "PASS"
  } else {
    useCellBoundaryFlag <- c("PASS", "FAIL")
  }

  # Check useSystemControlFlag
  if ( useSystemControlFlag == TRUE ) {
    useSystemControlFlag <- "PASS"
  } else {
    useSystemControlFlag <- c("PASS", "FAIL")
  }

  # Check useZstackFlag
  if ( useZstackFlag == TRUE ) {
    useZstackFlag <- "PASS"
  } else {
    useZstackFlag <- c("PASS", "FAIL")
  }

  # Filter by useCellBoundaryFlag and useSystemControlFlag
  cleaned_transcripts <- transcripts %>%
    filter(CellBoundaryFlag %in% useCellBoundaryFlag,
           SystemControlFlag %in% useSystemControlFlag)

  # Calculate percentage
  allCount <- nrow(transcripts)
  firstPercent <- nrow(cleaned_transcripts)/nrow(transcripts)*100
  firstCount <- nrow(cleaned_transcripts)

  # Filter using useZstackFlag
  cleaned_transcripts <- transcripts %>%
    filter(ZstackFlag %in% useZstackFlag)

  # Calculate percentage
  secondPercent <- nrow(cleaned_transcripts)/nrow(transcripts)*100
  secondCount <- nrow(cleaned_transcripts)

  # Report
  message("useCellBoundaryFlag and useSystemControlFlag discarded ", round(100-firstPercent, 2), "% (",
      firstCount, " out of ", allCount, ") of all transcripts")
  message("An additional ", round(100-secondPercent, 2), "% (",
      secondCount, " out of ", allCount, ") of all transcripts were discarded by useZstackFlag")

  return(cleaned_transcripts)

}

