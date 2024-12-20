#' Read raw transcripts files
#'
#' Reads all CosMx SMI raw transcript files (*complete_code_cell_target_call_coord.csv) in the specified path. Raw transcript files can be found in the 'AnalysisResults' folder after exporting the data from AtoMx SIP.
#'
#' @param path 	a character vector containing the path name to read the raw transcripts files from
#' @param standardiseZstacks a logical to determine if Z-slice nomenclature is to be standardised between FOvs
#'
#' @return A data frame containing the raw transcripts from all FOVs found in the specified folder.
#' @export
#'
#' @import tidyverse
#'
#' @examples
#' \dontrun{
#'
#' transcripts <- readTranscripts("path/to/input/directory")
#' }

readTranscripts <- function(path, standardiseZstacks = FALSE) {

  # Function to standardise Zstacks nomenclature
  convertZslices <- function(fov_transcripts) {

    fov_transcripts$z <- factor(as.numeric(fov_transcripts$z))
    levels(fov_transcripts$z) <- seq(1, length(unique(fov_transcripts$z)))
    fov_transcripts$z <- as.numeric(fov_transcripts$z)
    fov_transcripts <- data.frame(fov_transcripts)
    return(fov_transcripts)

  }

  # Find all *complete_code_cell_target_call_coord.csv files in the path and read used fread
  all <- list.files(path = path, pattern = "*complete_code_cell_target_call_coord.csv", recursive = TRUE, full.names = TRUE) %>%
    map_df(~fread(.))

  if ( standardiseZstacks == TRUE ) {
    all_split <- all %>%
      group_by(fov) %>%
      group_split()

    all <- do.call(rbind, lapply(all_split, convertZslices))
  }

  # Report the number of transcripts and FOVs
  cat(paste0("Finished collecting ", nrow(all), " raw transcripts from ", length(unique(all$fov)), " FOVs."))

  return(all)

}
