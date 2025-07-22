#' Allows to use up to three different geoms with different aesthetic mapping. Intended to just give an first impression how the different geoms would look as a plot. Lacks functionality compared to using the normal ggplot function. For a bit more functionality use [QuickVis::quickvis_simple()] and add further geoms after the function.
#' Works with geoms starting with "geom_", doesn't work for inputs: abline, hline, vline, contour, contour_filled, errorbar, errorbarh, hex, crossbar, errorbar, linerange, pointrange, map, path, line, qq_line, qq, quantile, ribbon, segmend, spoke, label, text, rect, sf, sf_label, sf_text.
#'
#' @param data Default dataset to use for plot.
#' @param geom1 A List specifying the mapping for x-axis, y-axis, the geom and a specific color.
#' @param geom2 A List specifying the mapping for x-axis, y-axis, the geom and a specific color.
#' @param geom3 A List specifying the mapping for x-axis, y-axis, the geom and a specific color.
#'
#' @examples
#'
#' # Can be used for one geom
#' quickvis_mulitgeom(mtcars, list("hp", NULL, "density", "blue"))
#' quickvis_mulitgeom(mtcars, list("hp", "wt", "col", "blue"))
#'
#' # For two geoms
#' quickvis_mulitgeom(mtcars, list("hp", "cyl", "point", NULL), list("hp", "cyl", "line", NULL))
#' quickvis_mulitgeom(mtcars, list("hp", "cyl", "point", NULL), list("hp", "gear", "line", "red"))
#' quickvis_mulitgeom(mtcars, list("gear", "hp", "smooth", "purple"), list("gear", "hp", "point", "red"))
#'
#' # Or for three geoms
#' quickvis_mulitgeom(mtcars, list("hp", "wt", "violin", "blue"), list("hp", "wt", "jitter", NULL), list("hp", "wt", "boxplot", "green"))
#'
#' # Use ggplot2 function to further enhance the plot
#' quickvis_mulitgeom(mtcars, list("hp", "cyl", "density_2d_filled", NULL))
#' +labs(title = "ExamplePlot", y = "Cylinder", x = "Horsepower", fill = "Level")
#'
#' @export
quickvis_mulitgeom <- function (data = NULL, geom1 = list(NULL, NULL, NULL, NULL), geom2 = list(NULL, NULL, NULL, NULL), geom3 = list(NULL, NULL, NULL, NULL)) {

  # Check if input list is able to be processed
  validate_input_geom <- function(geom) {

    # True if default list
    if(all(sapply(geom, is.null))){
      return(TRUE)
    }
    # Check if length 4
    if(length(geom) != 4) {
      stop("Each geom List must be of length 4, if you dont want to use certain aesthetic set input NULL")
    }
    if(geom[[3]] %in% c("dotplot", "bar") && !is.null(geom[[1]]) && !is.null(geom[[2]])) {
      stop("geom_bar() only requires one aesthetic either x or y , geom_dotplot() only works with x aesthetic. Replace one aesthetic with NULL depending on your needs. quickvis_multigeom() always uses the deufault stat.")
    }

    return(TRUE)
  }

  # apply validate function
  validate_input_geom(geom1)
  validate_input_geom(geom2)
  validate_input_geom(geom3)


  # Create plot
  plot <- ggplot2::ggplot(data)


  # add geom function
  add_geom <- function(geom, plot) {
    # Filter for non default geoms
    if(!all(sapply(geom, is.null))){

      # Extract geom function from character input
      geom_function <- get(paste0("geom_", geom[[3]]), envir = asNamespace("ggplot2"))


      # Make density, histogram and freqpoly work
      if(geom[[3]] %in% c("density", "histogram", "freqpoly")){

        # replace Null Value with ""..density..""
        if(is.null(geom[[1]])){
          geom[[1]] = "..density.."
        } else if (is.null(geom[[2]])) {
          geom[[2]] = "..density.."
        }
      }




      # check if color is NULL to avoid Problems with "aes_string()"
      if(!is.null(geom[[4]])){

        # Make bar and dotplot work by only applying x or y just setting one null doesnt work
        # Filter bar, col, dotplots
        if(geom[[3]] %in% c("bar", "dotplot")) {
          #Only apply one aesthetic
          if(is.null(geom[[2]])){
            l <- geom_function(ggplot2::aes_string(x = geom[[1]]), color = geom[[4]])
          } else {
            l <- geom_function(ggplot2::aes_string(y = geom[[2]]), color = geom[[4]])
          }
        } else {
          # Normal case for most geoms when color is set
          l <- geom_function(ggplot2::aes_string(x = geom[[1]], y = geom[[2]]), color = geom[[4]])
        }


        #Non set color case
      } else {
        # Filter bar, dotplots for non set color case
        if(geom[[3]] %in% c("bar", "dotplot")) {
          #Only apply one aesthetic
          if(is.null(geom[[2]])){
            l <- geom_function(ggplot2::aes_string(x = geom[[1]]))
          } else {
            l <- geom_function(ggplot2::aes_string(y = geom[[2]]))
          }
        } else{
          # Normal case for most geoms when color is not set
          l <- geom_function(ggplot2::aes_string(x = geom[[1]], y = geom[[2]]))
        }
      }



      # Add the geom layer to the plot
      plot <- plot + l
    }


    return(plot)
  }

  # use add_geom function on our input vectors
  plot <- add_geom(geom1, plot)
  plot <- add_geom(geom2, plot)
  plot <- add_geom(geom3, plot)


  return(plot)

}


