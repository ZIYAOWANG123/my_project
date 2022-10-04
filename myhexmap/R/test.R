library(sf)
library(ggplot2)

square <- matrix(c(0, 0,
                   0, 1,
                   1, 1,
                   1, 0,
                   0, 0),
                 ncol = 2,
                 byrow = TRUE) %>%
  list() %>%
  st_polygon()

plot_hex_tile <- function(cellsize = NULL, n = NULL) {
  title <- ifelse(is.null(cellsize),
                  paste0("n = ", n),
                  paste0("Cell size = ", cellsize))
  if(is.null(cellsize)) {
    title <- paste0("n = ", n)
    grid <- st_make_grid(square, n = n, square = FALSE)
  } else {
    title <- paste0("Cell size = ", cellsize)
    grid <- st_make_grid(square,
                         cellsize = cellsize,
                         square = FALSE)
  }

  ggplot(grid) +
    geom_sf(fill = "transparent") +
    geom_sf(data = square, fill = "transparent", color = "red") +
    ggtitle(title)
}

plot_hex_tile(0.25)
