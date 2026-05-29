

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
library(ggtext)

source("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Skripte\\Sonstiges\\laden_Fonts.R")


# Anfang -----------------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")



# WG-Daten ---------------------------------------------------------------------

query_wgdaten <- glue_sql("
  SELECT datum_scraping, stadtteil_geocoding
  FROM analysedaten
    WHERE stadt = {stadt}
      AND geolocation IS NOT NULL
", .con = con_lokal)

stadteile_wohnungen <- dbGetQuery(con_lokal, query_wgdaten) 

stadteile_wohnungen_aufb <- stadteile_wohnungen %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(datum_scraping >= "2025-10-01" & datum_scraping <= "2026-03-31") %>%
  select(-datum_scraping) %>%
  group_by(stadtteil_geocoding) %>%
  summarise(Anzahl_Wohnungen = n()) %>%
  rename(stadtteil = stadtteil_geocoding)

# Geodaten Stadtteilgrenzen ----------------------------------------------------

query_geo_stadtteile <- glue_sql("
  SELECT stadt, stadtteil, geom.STAsText() AS wkt
  FROM geodaten_stadtteile
    WHERE stadt = {stadt}
      AND stadtteil IS NOT NULL
", .con = con_lokal)

grenzen_stadtteile <- dbGetQuery(con_lokal, query_geo_stadtteile) %>%
  st_as_sf(wkt = "wkt", crs = 4326) %>%
  st_transform(crs = 25832) %>%
  left_join(stadteile_wohnungen_aufb, by = "stadtteil")

grenzen_stadt <- grenzen_stadtteile %>%
  st_make_valid() %>%
  group_by(stadt) %>%
  summarise(geometry = st_union(wkt)) %>%
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

file_save <- "Abbildungen/1_Präsentation_Linke_Kassel/Karte_Hexagon_Kassel_1.png"
ggsave(filename = file_save, plot = Karte_Hexagon, 
       width = 16, height = 9, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))


