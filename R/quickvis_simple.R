#' Useful to create a simple plot quickly. Uses ggplot to create the plot. Alternative to just using ggplot if you want to create a simple plot to visualise data quicker with less complex code.
#'
#' @param data Default dataset to use for plot.
#' @param x Variable which will be mapped to the x-axis of the plot.
#' @param y Variable which will be mapped to the y-axis of the plot.
#' @param geom Decides geom used. No input will result in the usage of [ggplot2::geom_point()].
#' @param theme Lets you decide which theme you want to use. No theme will be applied if left empty.
#' @param facet_wrap Decides which variable you want to use for a facet_wrap. When left empty there will be no facet wrap.
#' @param col Decides variable of dataset which will color the geom. Not colors will be applied if left empty.
#'
#' @returns ggplot
#' @export
#'
#' @examples quickvis_simple(mtcars, "hp", "cyl")
#' @examples quickvis_simple(mtcars, "hp", geom = "bar")
#' @examples quickvis_simple(mtcars, "mpg", "cyl", "line", "bw")
#'
#' # using facet_wrap argument
#' @examples quickvis_simple(mtcars, "cyl", "hp", facet_wrap = "gear")
#'
#' # using col argument
#' @examples quickvis_simple(mtcars, "gear", "hp", col = "vs")
#'
#' # U can also add other ggplot2 functions after the function to further enhance the plot
#' @examples quickvis_simple(mtcars, "wt", "cyl", "point", "minimal") +
#' geom_line() +
#' labs(title = "Title", x = "X", y = "Y")
#'
#'
quickvis_simple <- function(data = NULL, x, y, geom = "point", theme = "none", facet_wrap = "F", col = NULL) {
  # Create plot
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x, y,col = col))


  # Select geom based on input argument
  plot <- plot + (get(paste0("geom_", geom), envir = asNamespace("ggplot2")))()


  # Select facet wrap based on input
  if (facet_wrap != "F") {
    plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", facet_wrap)))
  }

  # Select theme based on input arguments
  if (theme != "none"){
    plot <- plot + (get(paste0("theme_", theme), envir = asNamespace("ggplot2")))()
  }

  # Return created plot
  return(plot)
}
