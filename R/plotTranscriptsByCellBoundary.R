#' Plot proportion of transcripts outside cell boundaries
#'
#' Initialises a ggplot object to plot the proportion of transcripts detected outside cell boundaries by FOV and Z-slice. A change point in this proportion can be estimated using envcpt.
#'
#' @param transcripts a data frame containing raw transcripts
#' @param estimateChangepoint a logical to determine if a change point should be estimated
#'
#' @return A ggplot object.
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import EnvCpt
#'
#' @examples
#' \dontrun{
#'
#' plotTranscriptsByCellBoundary(transcripts)
#' plotTranscriptsByCellBoundary(transcripts, estimateChangepoint = TRUE)
#' }

plotTranscriptsByCellBoundary <- function(transcripts, estimateChangepoint = FALSE) {

  # Check for CellBoundaryFlag
  if ( !("CellBoundaryFlag" %in% colnames(transcripts)) ) {

    stop("Missing CellBoundaryFlag. Please run setQualityFlags().")

  }

  # Check for SystemControlFlag
  if ( !("SystemControlFlag" %in% colnames(transcripts)) ) {

    stop("Missing SystemControlFlag. Please run setQualityFlags().")

  }

  # Calculate proportion of transcripts outside cell boundaries per Z-slice
  z_stats <- transcripts %>%
    group_by(fov, z, CellBoundaryFlag) %>%
    summarise(Transcripts = n()) %>%
    mutate(Proportion = Transcripts / sum(Transcripts)) %>%
    filter(CellBoundaryFlag == "FAIL")

  # Plot lineplot
  stats_lineplot <- ggplot(z_stats, aes(x = z, y = Proportion, colour = factor(fov))) +
    geom_line() + geom_point(size = 0.5) +
    scale_colour_grey(start = 0.1, end = 0.9) +
    xlab("Z-slice") + ylab("Proportion of transcripts") +
    scale_x_continuous(breaks = seq(min(transcripts$z), max(transcripts$z))) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    theme_classic(base_size = 22) +
    theme(legend.position = "none")

  # Models the proportion of transcripts outside cell boundaries to estimate a changepoint
  if ( estimateChangepoint == TRUE ) {

    # Function to estiamte a changepoint
    changepoint <- function(fov_stats) {

      fov_changepoint <- data.frame(fov = unique(fov_stats$fov),
                                    data.frame(t(envcpt(c(fov_stats$z, fov_stats$Proportion))$summary["nparam",])))

      return(fov_changepoint)

      }

    # Function to find the mode of a numeric vector
    mode <- function(codes) {
      which.max(tabulate(codes))
    }

    # Only model Z-slices that are detected in 80% of FOVs
    z_stats_select <- z_stats %>%
      group_by(z) %>%
      summarise(FOVs = n()) %>%
      filter(FOVs >= max(z_stats$fov)*0.8)
    z_stats_list <- z_stats[z_stats$z %in% (z_stats_select$z),]

    # Convert to list
    z_stats_list <- z_stats_list %>%
      group_by(fov) %>%
      group_split()

    # Detect changepoint
    z_stats_changepoints <- data.frame(t(sapply(z_stats_list, changepoint)))

    # Look for meanar1cpt and trendcpt models and find the mode (most common changepoint) across FOVs
    z_stats_changepoints <- data.frame(Mode = t(z_stats_changepoints %>%
      summarise(meanar1cpt_mode = mode(as.numeric(meanar1cpt)),
                trendcpt_mode = mode(as.numeric(trendcpt)))))

    # Add estiamted changepoint as abline
    stats_lineplot <- stats_lineplot + geom_vline(xintercept = c(z_stats_changepoints$Mode))

  }

  return(stats_lineplot)

}
