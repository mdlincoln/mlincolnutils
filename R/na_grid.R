#' Visualize missing values in a data frame
#'
#' Checks for NAs in every column of a dataframe, visualizing them using ggplot's raster mode. Exclusively NA columns will be rendered translucent.
#'
#' @param d A data frame
#' @export
na_grid <- function(d) {
  var_order <- rev(names(d))
  shaped <- dplyr::mutate_each(d, dplyr::funs(is.na), everything())
  shaped$row_number <- 1:nrow(shaped)
  shaped <- tidyr::gather(shaped, variable, is_missing, -row_number)
  shaped$variable <- factor(shaped$variable, levels = var_order, ordered = TRUE)
  shaped <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(shaped, variable), entirely_missing = all(is_missing)))
  ggplot2::ggplot(shaped, ggplot2::aes(x = row_number, y = variable, fill = !is_missing, alpha = !entirely_missing)) +
    ggplot2::geom_raster()
}