#' Plot transcript summary
#'
#' Initialises a ggplot object of either the number of raw transcripts or the code class distribution.
#'
#' @param transcripts a data frame containing raw transcripts
#' @param by a character vector specifying the column used to calculate and plot
#' @param codeclass a logical to determine if the code class distribution is to be plotted
#'
#' @return A ggplot object.
#' @export
#'
#' @import scales
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'
#' plotTranscripts(transcripts)
#' plotTranscripts(transcripts, by = "SampleID")
#' plotTranscripts(transcripts, by = "SampleID", codeclass = TRUE)
#' }

plotTranscripts <- function(transcripts, by = "fov", codeclass = FALSE) {

  if ( codeclass == FALSE ) {

    # Calculate the number of transcripts
    stats <- transcripts %>%
      group_by(across(all_of(by))) %>%
      summarise(Transcripts = n(),
                Cells = n_distinct(CellId))

    # Create barplot with the number of transcripts
    stats_barplot <- ggplot(stats, aes_string(x = by, y = "Transcripts")) +
      geom_bar(stat = "identity") + xlab("FOVs") + ylab("Number of transcripts (in millions)") +
      scale_y_continuous(labels = label_number(scale = 1e-6)) +
      theme_classic(base_size = 22) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())

    return(stats_barplot)

    } else if ( codeclass == TRUE ) {

    # Calculate code class proportion
    stats <- transcripts %>%
      group_by(across(all_of(c(by, "codeclass")))) %>%
      summarise(Transcripts = n()) %>%
      mutate(Proportion = Transcripts / sum(Transcripts))

    # Update factor order
    stats$codeclass <- factor(stats$codeclass, levels = c("SystemControl", "Negative", "Endogenous"))

    # Create barplot with code class proportions
    stats_barplot <- ggplot(stats, aes_string(x = by, y = "Proportion", fill = "codeclass")) +
      geom_bar(stat = "identity") + xlab("FOVs") + ylab("Proportion of transcripts") +
      scale_fill_manual(values = c("#00A14B", "#ED1C24", "#D1D3D4")) +
      theme_classic(base_size = 22) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.key.spacing = unit(1, "pt"))

    return(stats_barplot)

  }

}





