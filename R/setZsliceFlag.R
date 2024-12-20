#' Add Z-slice flag
#'
#' Function to add a flag for Z-slices that are to be removed. Should only be used after having run plotTranscriptsByCellBoundary().
#'
#' @param transcripts a data frame containing raw transcripts
#' @param index An integer that determines which Z-slices will be flagged
#'
#' @return A data frame containing raw transcripts with the ZstackFlag column.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' transcripts <- setZsliceFlag(transcripts, 7)
#' }
setZsliceFlag <- function(transcripts, index) {

  # Set Z-slice flag
  transcripts$ZstackFlag <- "PASS"
  transcripts$ZstackFlag[transcripts$z >= index] <- "FAIL"

  # Determine unique Z-slices
  uniqueZslices <- unique(transcripts$z)

  # Report summary
  cat("Flagged all transcripts detected in the following Z-slices:", paste0(uniqueZslices[uniqueZslices >= index], collapse = ", "))

  return(transcripts)

}
