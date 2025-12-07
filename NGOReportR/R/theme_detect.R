
#' Detect Funding Themes Using qdap
#'
#' Uses rule-based keyword dictionaries to classify grant purposes
#' into interpretable usual NGO themes.
#'
#' @param data A dataset containing a purpose text column
#' @param purpose_column Name of the Purpose column (string)
#' @return Data with added `theme` column
#' @importFrom qdap freq_terms polarity
#' @export


detect_funding_themes_qdap <- function(data, purpose_column = "purpose") {

  if (!purpose_column %in% names(data)) {
    stop("The specified text column does not exist in the dataset.")
  }

  cli::cli_h1("Detecting Funding Themes Using qdap")
  text_vec <- tolower(data[[text_col]]) #Lowers all of the text to make matching easier

  theme_dict <- list(
    Education = c("school", "education", "student", "teacher", "learning", "literacy"),
    Health = c("health", "hospital", "clinic", "cancer", "mental", "medical"),
    Women = c("women", "girl", "maternal", "female", "domestic"),
    Climate = c("climate", "flood", "environment", "carbon", "pollution"),
    Poverty = c("poverty", "food", "hunger", "shelter", "relief"),
    Technology = c("technology", "internet", "AI", "computer", "digital")
  )


  counts <- lapply(theme_dict, function(words) {
    qdap::term_count(text_vec, words)
  })

  count_tbl <- as.data.frame(counts)


  theme_assignment <- apply(count_tbl, 1, function(row) {
    if (all(row == 0)) {
      return("Other")
    } else {
      return(names(which.max(row)))
    }
  })

  data$theme_qdap <- theme_assignment

  cli::cli_alert_success("qdap theme detection complete!")

  return(data)
}

#' Visualize the Detected Funding Themes
#' ggplot bar chart of funding themes
#' @param data A dataset with detected themes
#' @param theme_column Name of the theme column (string)
#' @return A ggplot object
#' @export
ggplot_funding_themes <- function(data, theme_column = "theme_qdap") {
  if (!theme_column %in% names(data)) {
    stop("The specified theme column does not exist in the dataset.")
  }

  cli::cli_h1("Visualizing Funding Themes")

  theme_summary <- data %>%
    dplyr::group_by(.data[[theme_column]]) %>%
    dplyr::summarise(
      count = dplyr::n(),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(
    theme_summary,
    ggplot2::aes(x = .data[[theme_column]], y = count)
  ) +
    ggplot2::geom_col(fill = "red") +
    ggplot2::labs(
      title = "Detected Funding Themes",
      x = "Funding Theme",
      y = "Count of Grants"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  cli::cli_alert_success("Visualization complete!")

  return(p)
}






