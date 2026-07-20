

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
", .con = con_lokal)

stadteile_wohnungen <- dbGetQuery(con_lokal, query_wgdaten) 

stadteile_wohnungen_aufb <- stadteile_wohnungen %>%
  group_by(stadtteil_geocoding) %>%
  summarise(Anzahl_Wohnungen = n()) %>%
  rename(stadtteil = stadtteil_geocoding)

# Geodaten Stadtteilgrenzen ----------------------------------------------------

grenzen_stadtteile <- read_sf("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/daten/geodaten/Hamburg/Geo_Stadtteile/Stadtteile_Hamburg.shp") %>%
  select(stadtteil = stadtteil_) %>%
  filter(stadtteil != "Neuwerk") %>%
  mutate(stadt = "Hamburg") %>%
  left_join(stadteile_wohnungen_aufb, by = "stadtteil") %>%
  st_transform(crs = 4326)

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
    colour = "gray65", sigma  = 5, expand = 7.5
  ) +
  geom_sf(data = grenzen_stadtteile, linewidth = 0.5,
          aes(fill = Anzahl_Wohnungen), color = "gray40") +
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
    plot.margin      = margin(r=95, t=5, b =5),
  )


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/Karte_WGs_Hamburg_1.png"
ggsave(filename = file_save, plot = Karte_Hexagon, 
       width = 16, height = 9, units = "in", dpi = 300)

system(paste("xdg-open", shQuote(file_save)))



