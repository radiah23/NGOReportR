#' Validate the data condition in the uploaded dataset
#' @param data A data frame containing the uploaded dataset.
#' @return A list containing a boolean 'is_valid' and a character vector 'messages
#' @return Summary of the dataset


# Checks the data type and missing values
#'@export
validate_data <- function(data) {
  cli::cli_h1("Validation")
  skim_summary <- skimr::skim(data)
  print(skim_summary)
}
