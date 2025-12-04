#' Summarize Average Amount by Year
#'
#' @param data A dataset
#' @param x Name of year column (string)
#' @param y Name of amount column (string)
#' @return A ggplot object
#' @export
summarize_amount_avg_year <- function(data, x = NULL, y = NULL) {
  if (is.null(x) || is.null(y)) {
    stop("Please provide column names.")
  }

  summary_tbl_yr_amount <- data %>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::summarise(
      avg_value = mean(.data[[y]], na.rm = TRUE),
      .groups = "drop"
    )

  p1 <- ggplot2::ggplot(
    summary_tbl_yr_amount,
    ggplot2::aes(x = .data[[x]], y = avg_value)
  ) +
    ggplot2::geom_line(linewidth = 1.2, color = "red") +
    ggplot2::geom_point(size = 2, color = "red") +
    ggplot2::labs(
      title = paste("Average", y, "per", x),
      x = x,
      y = paste("Average", y)
    ) +
    ggplot2::theme_minimal()

  return(p1)
}

#' Summarize Location Count
#'
#' @param data A dataset
#' @param x Name of location column (string)
#' @return A ggplot object
#' @export
summarize_location_count <- function(data, x = NULL) {
  if (is.null(x)) {
    stop("Please provide a column name.")
  }

  summary_tbl_location <- data %>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::summarise(
      count = dplyr::n(),
      .groups = "drop"
    )

  p2 <- ggplot2::ggplot(
    summary_tbl_location,
    ggplot2::aes(x = .data[[x]], y = count)
  ) +
    ggplot2::geom_col(fill = "blue") +
    ggplot2::labs(
      title = paste("Count of Grants in", x),
      x = x,
      y = "Count of Grants"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(p2)
}

#' Organization Size vs Amount
#'
#' @param data A dataset
#' @param x Name of org size column (string)
#' @param y Name of amount column (string)
#' @return A ggplot object
#' @export
summarize_orgsize_amount <- function(data, x = NULL, y = NULL) {
  if (is.null(x) || is.null(y)) {
    stop("Please provide column names.")
  }

  p3 <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]])
  ) +
    ggplot2::geom_point(color = "green", alpha = 0.6) +
    ggplot2::labs(
      title = paste("Relationship between", x, "and", y),
      x = x,
      y = y
    ) +
    ggplot2::theme_minimal()

  return(p3)
}

#' Time Series of Amount Over Year
#'
#' @param data A dataset
#' @param x Name of year column (string)
#' @param y Name of amount column (string)
#' @return A ggplot object
#' @export
time_series_amount_year <- function(data, x = "year", y = "amount") {

  p4 <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]])
  ) +
    ggplot2::geom_line(color = "purple", linewidth = 1) +
    ggplot2::geom_point(color = "purple", size = 2) +
    ggplot2::labs(
      title = paste("Time Series of", y, "over", x),
      x = x,
      y = y
    ) +
    ggplot2::theme_minimal()

  return(p4)
}

#' Combine All Automatic Plots
#'
#' @param data A dataset
#' @return A combined patchwork plot
#' @export
combine_auto_plots <- function(data) {

  p1 <- summarize_amount_avg_year(data, x = "year", y = "amount")
  p2 <- summarize_location_count(data, x = "location")
  p3 <- summarize_orgsize_amount(data, x = "org_size", y = "amount")
  p4 <- time_series_amount_year(data, x = "year", y = "amount")

  combined_plot <- p1 / p2 / p3 / p4 +
    patchwork::plot_layout(ncol = 1)

  return(combined_plot)
}

  #' Print all of the plots as a PDF
  #'
  #' @param data A dataset
  #' @param filename Name of the output PDF file
  #' @export
  export_auto_plots_pdf <- function(data, filename = "auto_plots.pdf") {

    combined_plot <- combine_auto_plots(data)

    ggplot2::ggsave(
      filename,
      plot = combined_plot,
      device = "pdf",
      width = 8,
      height = 12
    )

    cli::cli_alert_success(paste("Automatic plots saved to", filename))

  }



