#' Plot flag summary
#'
#' Initialises a ggplot object to plot the proportion of transcripts flagged by setQualityFlags() and setZsliceFlag().
#'
#' @param transcripts a data frame containing raw transcripts
#' @param by a character vector specifying the column used to calculate and plot
#'
#' @return A ggplot object.
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'
#' plotFlags(transcripts)
#' plotFlags(transcripts, by = "SampleID")
#' }
plotFlags <- function(transcripts, by = "fov") {

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

  transcripts$CombinedFlag <- paste0(transcripts$SystemControlFlag, "_",
                                         transcripts$CellBoundaryFlag, "_",
                                         transcripts$ZstackFlag)

  CombineFlagFOV <- transcripts %>%
    group_by(fov, CombinedFlag) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n))

  CombineFlagFOV$CombinedFlag <- factor(CombineFlagFOV$CombinedFlag)
  levels(CombineFlagFOV$CombinedFlag) <- c("SystemControl", "SystemControl", "SystemControl", "SystemControl",
                                           "CellBoundary&Zstack", "CellBoundary", "Zstack", "Pass")
  CombineFlagFOV$CombinedFlag <- factor(CombineFlagFOV$CombinedFlag, levels = rev(c("SystemControl", "CellBoundary",
                                                                                    "CellBoundary&Zstack", "Zstack", "Pass")))

  stats_flags <- ggplot(CombineFlagFOV, aes(x = fov, y = prop, fill = CombinedFlag)) +
    geom_bar(stat = "identity") + xlab("FOVs") + ylab("Proportion of transcripts") +
    scale_fill_manual(values = rev(c("#00A14B", "#EE7477", "#AB2025", "#00AEEF", "#F1F2F2"))) +
    ylim(c(0,1)) +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.spacing = unit(1, "pt"))

  return(stats_flags)

}
