#' Visualize Study KRI with Confidence Intervals
#'
#' @description
#' Creates a layered visualization comparing a study's KRI metric against portfolio
#' confidence intervals. The plot shows:
#' \itemize{
#'   \item Portfolio/comparison group confidence intervals (light blue ribbon)
#'   \item Individual study confidence intervals (orange ribbon, optional)
#'   \item Actual study metric line (black line with points)
#' }
#'
#' This visualization helps identify when a study's performance deviates from
#' expected portfolio norms.
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point labs theme_minimal theme element_text element_blank
#' @importFrom rlang .data
#'
#' @param dfStudyKRI data.frame. Actual study-level metric data from `Transform_CumCount`.
#'   Must contain columns specified by `strStudyMonthCol` and `strMetricCol`.
#'   If a `StudyID` column exists, data will be automatically filtered to `strStudyID`.
#' @param dfGroupBounds data.frame. Portfolio/comparison group confidence intervals
#'   from `Analyze_StudyKRI_PredictBoundsGroup`. Must contain: `StudyMonth`,
#'   `MedianMetric`, `LowerBound`, `UpperBound`.
#' @param dfStudyBounds data.frame or NULL. Individual study confidence intervals
#'   from `Analyze_StudyKRI_PredictBounds` (optional). If a `StudyID` column exists,
#'   data will be automatically filtered to `strStudyID`. If provided, must contain:
#'   `StudyMonth`, `MedianMetric`, `LowerBound`, `UpperBound`.
#' @param strStudyID character. Study ID to filter data and display in title/subtitle.
#' @param strStudyMonthCol character. Column name for study month (default: "StudyMonth").
#' @param strMetricCol character. Column name for metric (default: "Metric").
#' @param nMaxMonth integer or NULL. Maximum study month to display. If NULL (default),
#'   shows all available months.
#' @param strTitle character or NULL. Plot title. If NULL, generates default title.
#' @param strSubtitle character or NULL. Plot subtitle. If NULL, generates default
#'   showing comparison study count.
#' @param strYlab character. Y-axis label (default: "Metric").
#' @param strXlab character. X-axis label (default: "Study Month").
#'
#' @return A ggplot2 object that can be displayed, saved, or further customized.
#'
#' @examples
#' \dontrun{
#' # Assuming workflow has been run
#' lAnalysis <- lAnalyzed$Analysis_kri0001
#'
#' # Define target study
#' target_study <- "STUDY1"
#'
#' # Option 1: Pass full datasets - function will filter automatically
#' p <- Visualize_StudyKRI(
#'   dfStudyKRI = lAnalysis$Analysis_Transformed,
#'   dfGroupBounds = lAnalysis$Analysis_GroupBounds,
#'   dfStudyBounds = lAnalysis$Analysis_Bounds,
#'   strStudyID = target_study,
#'   strYlab = "Cumulative AE Rate per Visit"
#' )
#'
#' # Option 2: Pre-filter data manually (equivalent to above)
#' dfStudyKRI <- lAnalysis$Analysis_Transformed[
#'   lAnalysis$Analysis_Transformed$StudyID == target_study, ]
#' dfStudyBounds <- lAnalysis$Analysis_Bounds[
#'   lAnalysis$Analysis_Bounds$StudyID == target_study, ]
#'
#' p <- Visualize_StudyKRI(
#'   dfStudyKRI = dfStudyKRI,
#'   dfGroupBounds = lAnalysis$Analysis_GroupBounds,
#'   dfStudyBounds = dfStudyBounds,
#'   strStudyID = target_study,
#'   strYlab = "Cumulative AE Rate per Visit"
#' )
#'
#' print(p)
#'
#' # Save plot
#' # ggplot2::ggsave("study_comparison.png", p, width = 10, height = 6)
#' }
#'
#' @export
Visualize_StudyKRI <- function(
  dfStudyKRI,
  dfGroupBounds,
  dfStudyBounds = NULL,
  strStudyID,
  strStudyMonthCol = "StudyMonth",
  strMetricCol = "Metric",
  nMaxMonth = NULL,
  strTitle = NULL,
  strSubtitle = NULL,
  strYlab = "Metric",
  strXlab = "Study Month"
) {
  # Input validation
  if (!is.data.frame(dfStudyKRI)) {
    stop("dfStudyKRI must be a data.frame")
  }
  
  if (!is.data.frame(dfGroupBounds)) {
    stop("dfGroupBounds must be a data.frame")
  }
  
  if (!is.null(dfStudyBounds) && !is.data.frame(dfStudyBounds)) {
    stop("dfStudyBounds must be a data.frame or NULL")
  }
  
  # Check required columns in dfStudyKRI
  required_kri <- c(strStudyMonthCol, strMetricCol)
  missing_kri <- setdiff(required_kri, names(dfStudyKRI))
  if (length(missing_kri) > 0) {
    stop(sprintf(
      "dfStudyKRI missing required columns: %s",
      paste(missing_kri, collapse = ", ")
    ))
  }
  
  # Check required columns in dfGroupBounds
  required_group <- c("StudyMonth", "LowerBound", "UpperBound", "MedianMetric")
  missing_group <- setdiff(required_group, names(dfGroupBounds))
  if (length(missing_group) > 0) {
    stop(sprintf(
      "dfGroupBounds missing required columns: %s",
      paste(missing_group, collapse = ", ")
    ))
  }
  
  # Check required columns in dfStudyBounds if provided
  if (!is.null(dfStudyBounds)) {
    required_study <- c("StudyMonth", "LowerBound", "UpperBound", "MedianMetric")
    missing_study <- setdiff(required_study, names(dfStudyBounds))
    if (length(missing_study) > 0) {
      stop(sprintf(
        "dfStudyBounds missing required columns: %s",
        paste(missing_study, collapse = ", ")
      ))
    }
  }
  
  if (!is.character(strStudyID) || length(strStudyID) != 1) {
    stop("strStudyID must be a single character string")
  }
  
  if (!is.null(nMaxMonth)) {
    if (!is.numeric(nMaxMonth) || length(nMaxMonth) != 1 || nMaxMonth <= 0) {
      stop("nMaxMonth must be NULL or a positive number")
    }
  }
  
  # Filter dfStudyKRI by strStudyID if StudyID column exists
  if ("StudyID" %in% names(dfStudyKRI)) {
    dfStudyKRI <- dfStudyKRI[dfStudyKRI$StudyID == strStudyID, ]
  }
  
  # Filter dfStudyBounds by strStudyID if StudyID column exists
  if (!is.null(dfStudyBounds) && "StudyID" %in% names(dfStudyBounds)) {
    dfStudyBounds <- dfStudyBounds[dfStudyBounds$StudyID == strStudyID, ]
  }
  
  # Filter to maximum month if specified
  if (!is.null(nMaxMonth)) {
    dfStudyKRI <- dfStudyKRI[dfStudyKRI[[strStudyMonthCol]] <= nMaxMonth, ]
    dfGroupBounds <- dfGroupBounds[dfGroupBounds$StudyMonth <= nMaxMonth, ]
    if (!is.null(dfStudyBounds)) {
      dfStudyBounds <- dfStudyBounds[dfStudyBounds$StudyMonth <= nMaxMonth, ]
    }
  }
  
  # Check for empty data after filtering
  if (nrow(dfStudyKRI) == 0) {
    stop("No data available for dfStudyKRI after filtering")
  }
  
  if (nrow(dfGroupBounds) == 0) {
    stop("No data available for dfGroupBounds after filtering")
  }
  
  # Generate default title if not provided
  if (is.null(strTitle)) {
    strTitle <- sprintf("Study %s KRI Comparison", strStudyID)
  }
  
  # Generate default subtitle if not provided
  if (is.null(strSubtitle)) {
    if ("StudyCount" %in% names(dfGroupBounds)) {
      n_studies <- unique(dfGroupBounds$StudyCount)[1]
      strSubtitle <- sprintf("Comparison Portfolio: %d studies", n_studies)
    } else {
      strSubtitle <- "Comparison Portfolio Envelope"
    }
  }
  
  # Start with group bounds (background layer - light blue)
  p <- ggplot2::ggplot(dfGroupBounds, ggplot2::aes(x = .data$StudyMonth)) +
    # Group CI ribbon (light blue background)
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$LowerBound, ymax = .data$UpperBound, fill = "Portfolio CI"),
      alpha = 0.5
    ) +
    # Group median line (dashed blue)
    ggplot2::geom_line(
      ggplot2::aes(y = .data$MedianMetric, color = "Portfolio Median"),
      linetype = "dashed",
      linewidth = 0.5
    )
  
  # Add individual study bounds if provided (middle layer - orange)
  if (!is.null(dfStudyBounds) && nrow(dfStudyBounds) > 0) {
    p <- p +
      ggplot2::geom_ribbon(
        data = dfStudyBounds,
        ggplot2::aes(
          x = .data$StudyMonth,
          ymin = .data$LowerBound,
          ymax = .data$UpperBound,
          fill = "Study CI"
        ),
        alpha = 0.4
      )
  }
  
  # Add actual study metric line (foreground layer - black)
  p <- p +
    ggplot2::geom_line(
      data = dfStudyKRI,
      ggplot2::aes(
        x = .data[[strStudyMonthCol]],
        y = .data[[strMetricCol]],
        color = "Actual Study Data"
      ),
      linewidth = 1
    ) +
    # Add points for emphasis
    ggplot2::geom_point(
      data = dfStudyKRI,
      ggplot2::aes(
        x = .data[[strStudyMonthCol]],
        y = .data[[strMetricCol]],
        color = "Actual Study Data"
      ),
      size = 2
    )
  
  # Add labels
  p <- p +
    ggplot2::labs(
      title = strTitle,
      subtitle = strSubtitle,
      x = strXlab,
      y = strYlab
    ) +
    # Add manual scales for legend
    ggplot2::scale_fill_manual(
      name = "Confidence Intervals",
      values = c("Portfolio CI" = "lightblue", "Study CI" = "orange"),
      breaks = c("Portfolio CI", "Study CI")
    ) +
    ggplot2::scale_color_manual(
      name = "Metrics",
      values = c("Portfolio Median" = "blue", "Actual Study Data" = "black"),
      breaks = c("Portfolio Median", "Actual Study Data")
    ) +
    # Use clean theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11, color = "gray40"),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )
  
  return(p)
}

