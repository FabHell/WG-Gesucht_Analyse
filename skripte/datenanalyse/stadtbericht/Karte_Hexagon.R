

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ABB. POLYGON-RASTER        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


stadt <- "Rostock"

cell_size <- 250


library(sf)
library(ggplot2)
library(dplyr)
library(glue)
library(DBI)
library(tidyverse)


# Anfang -----------------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")


# Geodaten Stadtteilgrenzen ----------------------------------------------------

query_geo_stadtteile <- glue_sql("
  SELECT stadt, stadtteil, geom.STAsText() AS wkt
  FROM geodaten_stadtteile
    WHERE stadt = {stadt}
      AND stadtteil IS NOT NULL
", .con = con_lokal)

grenzen_stadtteile <- dbGetQuery(con_lokal, query_geo_stadtteile) %>%
  st_as_sf(wkt = "wkt", crs = 4326) %>%
  st_transform(crs = 25832) 

grenzen_stadt <- grenzen_stadtteile %>%
  st_make_valid() %>%
  group_by(stadt) %>%
  summarise(geometry = st_union(wkt)) %>%
  ungroup()


# WGdaten laden ----------------------------------------------------------------

query_wgdaten <- glue_sql("
  SELECT stadt, stadtteil_geocoding, geolocation
  FROM analysedaten
    WHERE stadt = {stadt}
      AND geolocation IS NOT NULL
", .con = con_lokal)

punkte_wohnungen <- dbGetQuery(con_lokal, query_wgdaten) 

punkte_wohnungen <- punkte_wohnungen %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  st_transform(crs = 25832)
  


# Polygonraster anlegen --------------------------------------------------------

grid <- st_make_grid(
  grenzen_stadtteile, 
  cellsize = cell_size,
  square = FALSE,
  what = "polygons") %>%
  st_sf(grid_id = 1:length(.)) %>%
  st_intersection(grenzen_stadt) %>%
  select(grid_id)


# Punkte dem Polygonraster zuordnen --------------------------------------------

punkte_in_zellen <- st_join(punkte_wohnungen, grid) %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(count = n()) %>%
  ungroup()

grid_mit_punkten <- grid %>%
  left_join(punkte_in_zellen, by = "grid_id") %>%
  mutate(count_decile = ntile(count, 10)) %>%
  mutate(count_decile = replace_na(count_decile, 0),
         count_decile = as.character(count_decile))


# Abbildung erstellen ----------------------------------------------------------

Karte_Hexagon <- ggplot() + 
  geom_sf(data = grid_mit_punkten, aes(fill = count_decile), 
          color = "gray70", size = 0.1) +
  geom_sf(data = grenzen_stadtteile, linewidth = 0.1,
          fill = "transparent", color = "gray40") +
  scale_fill_discrete(type = c("0" = "gray80",
                               "1" = "#edf4ff", 
                               "2" = "#dfeeff", 
                               "3" = "#c9e2fe", 
                               "4" = "#b0d2fd", 
                               "5" = "#9fc1fb", 
                               "6" = "#92b1f5", 
                               "7" = "#8ea7ec", 
                               "8" = "#8fa0d7", 
                               "9" = "#8f99c5", 
                               "10" = "#8b92ba")) +
  theme_minimal() +
  labs(
    title = paste("Raster-Analyse", stadt),
    subtitle = paste("Zellendurchmesser:", cell_size, "qm"),
    caption = paste("Gesamt:", sum(punkte_in_zellen$count), "Punkte")
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.subtitle = element_text(margin = margin(l=15), face = "italic"),
    plot.background = element_rect(fill = "gray99", color = "transparent"),
    legend.position = "none"
  )

file_save <- glue("Abbildungen/Karte_Hexagon/Karte_Hexagon_{stadt}.png")
ggsave(filename = file_save, plot = Karte_Hexagon, 
       width = 7, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

