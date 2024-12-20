#' Plot relationship cell and transcript number
#'
#' Initialises a ggplot object of the relationship between the number of cells and the number of raw transcripts. Outliers can be detected by modelling this relationship using lmrob.
#'
#' @param transcripts a data frame containing raw transcripts
#' @param by a character vector specifying the column used to
#' @param detectOutliers a logical to determine if outliers should be detected
#'
#' @return A ggplot object.
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import robustbase
#'
#' @examples
#' \dontrun{
#'
#' plotTranscriptsPerCell(transcripts)
#' plotTranscriptsPerCell(transcripts, detectOutliers = TRUE)
#' plotTranscriptsPerCell(transcripts, by = "SampleID", detectOutliers = TRUE)
#' }

plotTranscriptsPerCell <- function(transcripts, by = "fov", detectOutliers = FALSE) {

  # Calculate number of transcripts and number of cells
  stats <- transcripts %>%
    group_by(across(all_of(by))) %>%
    summarise(Transcripts = n(),
              Cells = n_distinct(CellId))

  #Only model the relationship if detectOutliers is set to TRUE
  if ( detectOutliers == TRUE ) {

    #Linear relationship to detect outliers using MM estimator
    model <- lmrob(Cells ~ Transcripts, data = stats, setting = "KS2014")
    outliers <- summary(model)$control$eps.outlier
    stats$Outliers <- ifelse(summary(model)$rweights <= outliers, TRUE, FALSE)

    # Create a dotplot with the number of transcripts and the number of cells
    # Includes regression line based on the lmrob model
    ggplot(stats, aes_string(x = "Transcripts", y = "Cells", color = "Outliers")) +
      geom_point(size = 3, shape = 19) +
      scale_color_manual(values = c("#000000", "#BE1E2D")) +
      geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2]) +
      xlab("Number of transcripts (in millions)") + ylab("Number of cells") +
      scale_x_continuous(labels = label_number(scale = 1e-6)) +
      theme_classic(base_size = 22) +
      theme(legend.position = "none")

  } else {

    # Create a dotplot with the number of transcripts and the number of cells
    ggplot(stats, aes_string(x = "Transcripts", y = "Cells")) +
      geom_point(size = 3, shape = 19, colour = "#000000") +
      xlab("Number of transcripts (in millions)") + ylab("Number of cells") +
      scale_x_continuous(labels = label_number(scale = 1e-6)) +
      theme_classic(base_size = 22) +
      theme(legend.position = "none")

  }

}

