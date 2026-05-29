

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ABB. POLYGON-RASTER        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


stadt <- "Kassel"

cell_size <- 200


library(sf)
library(ggplot2)
library(dplyr)
library(glue)
library(DBI)
library(tidyverse)
library(ggfx)


source("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Skripte\\Sonstiges\\laden_Fonts.R")


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
  SELECT stadt, datum_scraping, stadtteil_geocoding, geolocation
  FROM analysedaten
    WHERE stadt = {stadt}
      AND geolocation IS NOT NULL
", .con = con_lokal)

punkte_wohnungen <- dbGetQuery(con_lokal, query_wgdaten) 

punkte_wohnungen <- punkte_wohnungen %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(datum_scraping >= "2025-10-01" & datum_scraping <= "2026-03-31") %>%
  select(-datum_scraping) %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  st_transform(crs = 25832)


# Uni-Geodaten -----------------------------------------------------------------

Uni_Kassel <- tibble(
  Ort = c("Hauptcampus", "Kunstuni", "Murhadstraße", "AVZ"),
  lon = c(9.505947531511303, 9.487369376868523, 9.473807178256493, 9.448924309219835),
  lat = c(51.3219196627738, 51.304459942946025, 51.31134133430143, 51.28174419310388))%>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(grenzen_stadtteile))


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
         count_decile = factor(count_decile,
                               levels = c("0","1","2","3","4","5",
                                          "6","7","8","9","10")))


# Abbildung erstellen ----------------------------------------------------------

Karte_Hexagon <- ggplot() + 
  with_outer_glow(
    geom_sf(data = grenzen_stadt, linewidth = 0.1,
            fill = "transparent", color = "white"),
    colour = "gray65", sigma  = 5, expand = 7.5
  ) +
  geom_sf(data = grid_mit_punkten, aes(fill = count_decile), 
          color = "gray70", size = 0.25) +
  geom_sf(data = grenzen_stadtteile, linewidth = 0.25,
          fill = "transparent", color = "gray40") +
  geom_sf(data = Uni_Kassel, colour = "gray15", fill = "red", size = 4.5,
          shape = 21) +
  scale_fill_discrete(
    type = c("0" = "gray90", "1" = "#edf4ff", "2" = "#dfeeff",
             "3" = "#c9e2fe", "4" = "#b0d2fd", "5" = "#9fc1fb",
             "6" = "#92b1f5", "7" = "#8ea7ec", "8" = "#8fa0d7",
             "9" = "#8f99c5", "10" = "#8b92ba"),
    labels = c("0" = "*keine WGs*", "1" = "1 *- wenig WGs*", "2" = "2", "3" = "3", "4" = "4",
               "5" = "5", "6" = "6", "7" = "7", "8" = "8",
               "9" = "9", "10" = "10 *- viele WGs*")
  ) +
  guides(fill = guide_legend(
    reverse        = TRUE,
    label.position = "right",
    keywidth       = unit(2, "lines"),
    keyheight      = unit(2.3, "lines"),
  )) +
  theme_minimal() +
  labs(
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    fill     = NULL
  ) +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    plot.subtitle    = element_text(margin = margin(l = 15), face = "italic"),
    plot.background  = element_rect(fill = "#1c202a", color = "transparent"),
    panel.background  = element_rect(fill = "#1c202a", color = "transparent"),
    legend.position  = "right",
    legend.key.spacing.y = unit(c(rep(0.25, 9), 4,0), "pt"),
    legend.text      = element_markdown(family = "domine", size = 18,
                                        margin = margin(l=15), color = "gray40"),
    legend.box.margin = margin(l=30)
  )

file_save <- "Abbildungen/1_Präsentation_Linke_Kassel/Karte_Hexagon_Kassel_4.png"
ggsave(filename = file_save, plot = Karte_Hexagon, 
       width = 16, height = 9, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))


