#' Save all ggplot objects
#'
#' Saves all ggplot objects in the current environment to disk using \link[ggplot2]{ggsave}.
#'
#' @param dir Directory path to save files.
#' @param devices A character vector of devices to use. Defaults to .pdf, but
#'   can take any of the devices currently supported by \link[ggplot2]{ggsave}.
#'   Specify multiple values to save versions of every plot using each device.
#' @param env The environment for scan for ggplot objects.
#' @param ... Arguments passed to \link[ggplot2]{ggsave}.
#' @export
ggsave_all <- function(dir = ".", devices = c("pdf", "png"), env = .GlobalEnv, ...) {
  # Find all ggplot objects in the specifed environment
  plots <- purrr::keep(ls(env), function(x) ggplot2::is.ggplot(get(x, envir = env)))
  purrr::walk(plots, function(x) {
    p <- get(x, envir = env)
    # Compute paths for saving plots
    plot_paths <- paste0(dir, "/", x, ".", devices)
    purrr::walk(plot_paths, function(y) ggplot2::ggsave(filename = y, plot = p, ...))
  })
}
