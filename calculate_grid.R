library(sf)
library(dplyr)
library(ozmaps)

is_sf <- function(x) inherits(x, "sf")


# x should be a "sf" object
hexgrid <- function(data, id){
  
  if(inherits(data, "sf")) {data = data}
  else {
    data <- st_sf(data)
  }

  if(id %in% colnames(data)) {cat("Yep, it's in this data!\n")}
  else {
    cat("Id variable is not in the provided data!\n")
  }
  
  hex_grid <- data %>% 
    mutate(hex_id = seq_along(data)) %>% 
    st_make_valid(data) %>% 
    st_make_grid(x = data,  cellsize = 2.3, square = FALSE)
  
  hex_best <- st_intersection(hex_grid, data) %>% 
    group_by(hex_id) %>% 
    summarise(area = sum(st_area(hex_grid))) %>% 
    top_n(length(id), wt = area) %>% 
    pull(hex_id)
  
  hex_grid <- filter(hex_grid, hex_id %in% !!hex_best)
  
  hex_map <- plot(st_union(data, hex_grid$hex_grid), pal = sf.colors(alpha = 0))

  return(hex_map)
}

# Test and debug function
ozmap_states %>% mutate(id = seq_along(NAME))
hexgrid(data = ozmap_states, id = id)



