quickvis_mulitgeom <- function (data = NULL, geom1 = c("", "", ""), geom2 = c("", "", ""), geom3 = c("", "", "")) {

  # Create plot
  plot <- ggplot2::ggplot(data)


  # Create geom1
  if (all(geom1 != c("", "", ""))) {
    plot <- plot + (get(paste0("geom_", geom1[3]), envir = asNamespace("ggplot2")))(ggplot2::aes_string(
      x = geom1[1],
      y = geom1[2]
    ))
  }
  # Create geom2
  if (all(geom2 != c("", "", ""))) {
    plot <- plot + (get(paste0("geom_", geom2[3]), envir = asNamespace("ggplot2")))(ggplot2::aes_string(
      x = geom2[1],
      y = geom2[2]
    ))
  }
  # Create geom3
  if (all(geom3 != c("", "", ""))) {
    plot <- plot + (get(paste0("geom_", geom3[3]), envir = asNamespace("ggplot2")))(ggplot2::aes_string(
      x = geom3[1],
      y = geom3[2]
    ))
  }
  return(plot)
}

