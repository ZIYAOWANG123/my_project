
# code and data from https://github.com/emitanaka/ozhexmap

library(sf)
library(tidyverse)
library(gganimate)
ozmap <- readRDS("present/ozced.rds") %>% 
  as_tibble() %>% 
  left_join(hexmap::aec2022, by = c("name" = "division"))

# note: pivot_longer has error: 
# ! Can't combine `hex` <sfc_GEOMETRY> and `geometry` <sfc_GEOMETRY>.
# so use approach below
hexmap <- ozmap %>% 
  select(-geometry.x, -geometry.y) %>% 
  rename(geometry = hex) %>% 
  mutate(type = "hex")
longmap <- ozmap %>% 
  select(-hex, geometry = geometry.y) %>% 
  mutate(type = "choropleth") %>% 
  bind_rows(hexmap)

g <- ggplot(longmap) + 
  geom_sf(aes(geometry = geometry, fill = winner),
          color = "black") + 
  #facet_wrap(~type) +
  theme_void() + 
  labs(fill = "Winner") +
  scale_fill_manual(values = c("#e11f30", "#0952bf", "green4", "black"),
                    limits = c("ALP", "LNP", "GRN", "Other")) +
  transition_states(type)

#g

gganimate::anim_save(filename = "present/ozhexmap.gif", g,
                     height = 600, width = 800)
