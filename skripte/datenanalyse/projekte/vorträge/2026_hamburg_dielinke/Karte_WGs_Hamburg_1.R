


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

cell_size <- 200


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


# WG-Daten ---------------------------------------------------------------------

query_wgdaten <- glue_sql("
  SELECT stadtteil_geocoding
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

stadteile_wohnungen <- dbGetQuery(con_lokal, query_wgdaten) 

stadteile_wohnungen_aufb <- stadteile_wohnungen %>%
  group_by(stadtteil_geocoding) %>%
  summarise(Anzahl_Wohnungen = n()) %>%
  rename(stadtteil = stadtteil_geocoding)

# Geodaten Stadtteilgrenzen ----------------------------------------------------

query <- glue_sql("
   SELECT *
     FROM geodaten_stadtteile
     WHERE stadt = {stadt}
   ", .con = con_lokal)

grenzen_stadtteile <- st_read(con_lokal, query = query) %>%
  st_transform(crs = 25832) %>%
  left_join(stadteile_wohnungen_aufb, by = "stadtteil") 


# Abbildung erstellen ----------------------------------------------------------

Karte_Hexagon <- ggplot() + 
  with_outer_glow(
    geom_sf(data = grenzen_stadtteile, linewidth = 0.5,
            aes(fill = Anzahl_Wohnungen), color = "gray40"),
    colour = "gray75", sigma  = 5, expand = 5
  ) +
  scale_fill_gradient(
    low   = "#dfeeff", 
    high = "#8b92ba",
    na.value = "gray85",
    guide = guide_colorbar(
      barwidth  = unit(1.25, "cm"),  
      barheight = unit(5, "cm")
    )
  ) +
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
    axis.ticks = element_blank(),
    plot.background  = element_rect(fill =  "#1c202a", color = "transparent"),
    panel.background  = element_rect(fill =  "#1c202a", color = "transparent"),
    legend.background = element_rect(fill =  "#1c202a", color = "transparent"),
    legend.position  = "right",
    legend.box.margin = margin(l = 30),
    legend.ticks = element_blank(),
    legend.text      = element_markdown(family = "domine", size = 16,
                                        margin = margin(l=10), color = "gray40"),
    plot.margin      = margin(r=90, t=25, b =25),
  )


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/Karte_WGs_Hamburg_1.png"
ggsave(filename = file_save, plot = Karte_Hexagon, 
       width = 16, height = 9, units = "in", dpi = 300)

add_logo_präsi(
  image_path = file_save,
  opacity = 0.25,
  logo_path = "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/daten/logos/DG_Logo_schwarzweiß.png"
)

system(paste("xdg-open", shQuote(file_save)))



