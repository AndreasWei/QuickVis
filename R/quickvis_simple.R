#' Create point plot quickly.
#'
#' @param data dataset
#' @param x x-axis parameter
#' @param y y-axis parameter
#' @param theme decide theme
#' @param facet_wrap decide if facetwrap should be enabled
#' @param col lets you adjust color of geom based on variable in the dataset
#'
#' @returns ggplot
#' @export
#'
#' @examples quickvis_simple(mtcars, "mpg", "cyl")
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
