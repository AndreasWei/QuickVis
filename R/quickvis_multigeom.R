#' Allows to use multiple different geoms with different aesthetic mapping. Intended to just give an first impression how the different geoms would look as a plot. Lacks functionality compared to using the normal ggplot function. For a bit more functionality use [QuickVis::quickvis_simple()] and add further geoms after the function.
#'
#' @param data Default dataset to use for plot.
#' @param geom1 First of 3 geoms available to add. Plot as follows "c ("Mapping for x-axis", "Mapping for y- Axis", "geom").
#' @param geom2 Plot as follows "c ("Mapping for x-axis", "Mapping for y- Axis", "geom").
#' @param geom3 Plot as follows "c ("Mapping for x-axis", "Mapping for y- Axis", "geom").
#'
#' @returns ggplot
#' @export
#'
#' @examples quickvis_mulitgeom(mtcars, c ("hp", "cyl", "point"), c ("hp", "cyl", "line"))
quickvis_mulitgeom <- function (data = NULL, geom1 = c("x", "y", "geom"), geom2 = c("x", "y", "geom"), geom3 = c("x", "y", "geom")) {

  # Create plot
  plot <- ggplot2::ggplot(data)


  # Create geom1
  if (all(geom1 != c("x", "y", "geom"))) {
    plot <- plot + (get(paste0("geom_", geom1[3]), envir = asNamespace("ggplot2")))(ggplot2::aes_string(
      x = geom1[1],
      y = geom1[2]
    ))
  }
  # Create geom2
  if (all(geom2 != c("x", "y", "geom"))) {
    plot <- plot + (get(paste0("geom_", geom2[3]), envir = asNamespace("ggplot2")))(ggplot2::aes_string(
      x = geom2[1],
      y = geom2[2]
    ))
  }
  # Create geom3
  if (all(geom3 != c("x", "y", "geom"))) {
    plot <- plot + (get(paste0("geom_", geom3[3]), envir = asNamespace("ggplot2")))(ggplot2::aes_string(
      x = geom3[1],
      y = geom3[2]
    ))
  }
  return(plot)
}

