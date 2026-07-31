


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ABB. POLYGON-RASTER        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


stadt <- "Hamburg"

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

cell_size <- 500


library(sf)
library(ggplot2)
library(dplyr)
library(glue)
library(DBI)
library(tidyverse)
library(ggfx)
library(ggtext)

source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")


# Anfang -----------------------------------------------------------------------

con_lokal <- dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("DATABASE_PG"),
                       host = Sys.getenv("SERVER_PG"),
                       port = Sys.getenv("PORT_PG"),
                       user = Sys.getenv("UID_PG"),
                       password = Sys.getenv("PWD_PG"))


# Geodaten Stadtteilgrenzen ----------------------------------------------------

query <- glue_sql("
   SELECT *
     FROM geodaten_stadtteile
     WHERE stadt = {stadt}
   ", .con = con_lokal)

grenzen_stadtteile <- st_read(con_lokal, query = query) %>%
  st_transform(crs = 25832) 

grenzen_stadt <- grenzen_stadtteile %>%
  st_make_valid() %>%
  st_union()


# WGdaten laden ----------------------------------------------------------------

query_wgdaten <- glue_sql("
  SELECT stadt, datum_scraping, stadtteil_geocoding, geolocation
  FROM analysedaten
  WHERE stadt = {stadt}
    AND geolocation IS NOT NULL
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND link IN (
      SELECT link
      FROM analysedaten
      GROUP BY link
      HAVING COUNT(*) <= 2
    )
", .con = con_lokal)

punkte_wohnungen <- st_read(con_lokal, query = query_wgdaten) %>%
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
    legend.box.margin = margin(l=30),
    plot.margin = margin(t=25, b=25)
  )

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/Karte_WGs_Hamburg_3.png"
ggsave(filename = file_save, plot = Karte_Hexagon, 
       width = 16, height = 9, units = "in", dpi = 300)

add_logo_präsi(
  image_path = file_save,
  opacity = 0.25,
  logo_path = "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/daten/logos/DG_Logo_schwarzweiß.png"
)

system(paste("xdg-open", shQuote(file_save)))


