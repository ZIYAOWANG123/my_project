#' Function `calculate_grid`
#'
#' This function provides the number of hexagons for the geographical area.
#'
#' @param data
#' This is input data which is a `sf` object (i.e. has geometry features).
#' @param weight
#' The variable to adjust the weight while assigning statistical information to the geographic hexagonal map.
#'
#' @return A geographic hexagonal map.
#'
#'
#'
#' @export calculate_grid



library(sf)
library(dplyr)
library(ozmaps)

calculate_grid <- function(data, weights = NULL){

  # Check if the input data is in SF class
  if(inherits(data, "sf")) {data = data}
  else {
    data <- st_sf(data)
  }

  # Check if weights variable is in numeric, if not return an error
  if(is.numeric(weights)) {}
  else {
    stop("Error: The weight variable is not numeric!")
  }

  # If weights = NULL, treat it as an equal weight for each obs
  if(is.null(weights)) {data %>% mutate(weights = rep(1))}

  # function to obtain proper cellsize for creating the hexagon grid
  # tot_area = total area of AU; land = total land area of AU; nhex = number of hexagons wanted by user
  cellsize <- function(data, offset = st_bbox(data)[c("xmin", "ymin")]){


  }

    # construct the calculation of hexagon grid
  hex_grid <- data %>%
    # create an id for later filtering intersection area
    mutate(hex_id = seq_along(data)) %>%
    # ensure data sf properties correct
    st_make_valid(data) %>%
    # create hexagons based on input data geometry: hex_grid = Total AU area; data = Total AU land area
    st_make_grid(x = data,  cellsize = 2.3, square = FALSE)

  # obtain the best number of hexagons
  hex_best <- st_intersection(hex_grid, data) %>%
    group_by(hex_id) %>%
    # acquire the sum of intersection areas
    summarise(area = sum(st_area(hex_grid))) %>%
    # pick the number of hexagons wanted (based on weights variable) by max. intersection area
    top_n(sum(weights), wt = area) %>%
    # get the id of hexagons maximized intersection area
    pull(hex_id)

  # filter number of hexagons maximized the intersection area by hex_id
  hex_grid <- filter(hex_grid, hex_id %in% !!hex_best)

  # output the map
  hex_map <- plot(st_union(data, hex_grid$hex_grid), pal = sf.colors(alpha = 0))

  return(hex_map)
}

# Test and debug function
# hexgrid(data = ozmap_states, weight = "")

# data(ozmap_states)
#
# coord <- st_coordinates(ozmap_states) %>% as_tibble() %>% select(c(X, Y))


