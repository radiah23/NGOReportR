#' Visualize the data with automatic plots
#'
#' @param data A data frame containing the uploaded dataset.
#' @param x Column name for the x-axis (default is NULL).
#' @param y Column name for the y-axis (default is NULL).
#' @return A ggplot object containing the generated plots.
#' @export
#' #Numerical data (Year, Amount: This histogram will have the average amount per year with a trend line)
#' x axis = Year
#' y axis = Amount

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

  return(plot = p1
  )
}



#' #Categorical data (Location: This bar plot will show the count of records per location)
#' x axis = Location
#' y axis = Count of grants
summarize_location_count <- function(data, x = NULL) {
  if (is.null(x)) {
    stop("Please provide a column name.")
  }
  summary_tbl_location <- data %>%
    dplyr::group_by(.data[[x]]) %>%
    dplyr::summarise(
      count = n(),
      .groups = "drop"
    )
  p2 <- ggplot2::ggplot(
    summary_tbl_location,
    ggplot2::aes(x = .data[[x]], y = count)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "blue") +
    ggplot2::labs(
      title = paste("Count of Grants In", x),
      x = x,
      y = "Count of Grants"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(plot = p2
  )
}


#' #Organization Size and Amount: This scatter plot will show the relationship between organization size and grant amount
#' x axis = Organization Size
#' y axis = Amount
#'
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

  return(plot = p3
  )
}









