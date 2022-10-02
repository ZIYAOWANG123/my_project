library(sf)
library(dplyr)
library(ozmaps)

# x should be a "sf" object
hexgrid <- function(data, weights = NULL){
  
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
  cellsize <- function(data, offset){
    
    offset = st_bbox(data)[c("xmin", "ymin")]
    
  }

  ## 1. how to link the "offset" in both functions? 2. how to replace "dx" and "dy" with no "cellsize" in them?
  make_hex_grid <- function (data, pt, dx) 
  {
    dx = cellsize[1]/sqrt(3)
    dy = sqrt(3) * dx/2
    pt = offset
    bb = st_bbox(data)
    xlim = bb[c("xmin", "xmax")]
    ylim = bb[c("ymin", "ymax")]
    
    offset = c(x = (offset[1] - xlim[1])%%dx, y = (pt[2] - ylim[1])%%(2 * dy))
    
    # using formula to calculate the proper cellsize based on the # of hexagons
    ## total area and land area of the data
    area = (xlim[2] - xlim[1]) * (ylim[2] - ylim[1])
    land <- sum(st_area(data))
    ## land-area ratio
    p = land / area
    
    ## number of hexagons for each col and row
    nx = floor((bb[3] - offset[1])/cellsize[1])
    ny = floor((bb[4] - offset[2])/cellsize[2])
    n = nx * ny * p
    
    # formula
    cellsize = sqrt((area * p * 2 * sqrt(3))/ n )
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


