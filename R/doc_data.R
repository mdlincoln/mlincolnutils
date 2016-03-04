#' Document data frames
#'
#' GivenFrom a data.frame, this function generates an atomic character vector
#' with skeleton documentation as per roxygen2's syntax.
#'
#' @param d A data.frame
#'
#' @return An atomic character vector.
#'
#' @examples
#' m <- data.frame(a = c(1, 2), b = c("n", "m"), stringsAsFactors = FALSE)
#' doc_data(m)
#'
#' @export
doc_data <- function(d) {
  # Get column names and types
  vartype <- vapply(d, typeof, FUN.VALUE = character(1))

  # Write individual item description templates
  items <- paste0("#\'   \\item{\\code{", names(vartype), "}}{", vartype, ". ###}", collapse = "\n")

  # Return the full documentation template
  paste0("#\' DATASET TITLE
#\'
#\' DATASET DETAILS
#\'
#\' @format A data frame with ", nrow(d), " rows and ", length(vartype), " variables:
#\' \\describe{
", items, "
#\' }
\"", deparse(substitute(d)), "\"")
}
