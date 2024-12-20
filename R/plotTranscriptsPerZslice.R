#' Create heatmap of transcript distribution across Z-slices
#'
#' A function to draw a heatmap to visualise the transcript distribution across Z-slices.
#'
#' @param transcripts a data frame containing raw transcripts
#' @param by a character vector specifying the column used to calculate and plot
#'
#' @return A pheatmap object.
#' @export
#'
#' @import pheatmap
#' @import RColorBrewer
#'
#' @examples
#' \dontrun{
#'
#' plotTranscriptsPerZslice(transcripts)
#' plotTranscriptsPerZslice(transcripts, by = "SampleID")
#' }

plotTranscriptsPerZslice <- function(transcripts, by = "fov") {

  # Calculate proportion of transcripts per Z-slice
  z_stats <- transcripts %>%
    group_by(across(all_of(c(by, "z")))) %>%
    summarise(Transcripts = n()) %>%
    mutate(Proportion = Transcripts / sum(Transcripts))

  # Convert from long to wide format
  z_stats <- pivot_wider(z_stats, id_cols = {{by}}, names_from = "z", values_from = "Proportion") %>%
    column_to_rownames(by)

  # Reorder columns
  z_stats <- z_stats[,paste0(seq(min(transcripts$z), max(transcripts$z)))]

  # Plot heatmap
  stats_heatmap <- pheatmap(t(z_stats),
                            cluster_rows = FALSE, cluster_cols = FALSE,
                            fontsize = 5,
                            color = RColorBrewer::brewer.pal(9, "Reds"))

  return(stats_heatmap)

}

