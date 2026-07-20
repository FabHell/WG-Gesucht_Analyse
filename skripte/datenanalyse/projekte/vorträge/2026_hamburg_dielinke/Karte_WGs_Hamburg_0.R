

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ABB. POLYGON-RASTER        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


stadt <- "Hamburg"

cell_size <- 200


library(sf)
library(ggplot2)
library(dplyr)
library(glue)
library(DBI)
library(tidyverse)
library(ggfx)



# Geodaten Stadtteilgrenzen ----------------------------------------------------


grenzen_stadtteile <- read_sf("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/daten/geodaten/Hamburg/Geo_Stadtteile/Stadtteile_Hamburg.shp") %>%
  select(stadtteil = stadtteil_) %>%
  filter(stadtteil != "Neuwerk") %>%
  mutate(stadt = "Hamburg")

grenzen_stadt <- grenzen_stadtteile %>%
  st_make_valid() %>%
  group_by(stadt) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()



# Abbildung erstellen ----------------------------------------------------------

Karte_Hexagon <- ggplot() + 
  with_outer_glow(
    geom_sf(data = grenzen_stadt, linewidth = 0.1,
            fill = "transparent", color = "white"),
    colour = "red", sigma  = 5, expand = 7.5
  ) +
  geom_sf(data = grenzen_stadtteile, linewidth = 0.5,
          fill = "gray85", color = "gray40") +
  theme_minimal() +
  labs(
    title    = NULL,
    subtitle = NULL,
    caption  = NULL
  ) +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    plot.background  = element_rect(fill =  "#1c202a", color = "transparent"),
    plot.margin      = margin(r=220, l=-150, t=25, b =25),
  )



file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/Karte_WGs_Hamburg_0.png.png"
ggsave(filename = file_save, plot = Karte_Hexagon, 
       width = 16, height = 9, units = "in", dpi = 300)

system(paste("xdg-open", shQuote(file_save)))
