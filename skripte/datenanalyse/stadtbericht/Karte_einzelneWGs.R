


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          KARTE EINZELNE WGS         #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


stadt <- "Chemnitz"

library(tidyverse)
library(sf)
library(DBI)
library(glue)
library(tidygeocoder)
library(showtext)
library(ggtext)

font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()


# Datenbankverbindung herstellen -----------------------------------------------

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


## WG-Daten --------------------------------------------------------------------

query_wgdaten <- glue_sql("
  SELECT stadtteil_geocoding, geolocation
  FROM analysedaten
    WHERE stadt = {stadt}
      AND geolocation != 'POINT EMPTY'
", .con = con_lokal)

wgdaten <- dbGetQuery(con_lokal, query_wgdaten) %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  st_transform(crs = 25832) %>%
  st_join(grenzen_stadt %>% rename(stadt_filter = stadt)) %>%
  filter(!is.na(stadt_filter))


## Erstellen der Karte ---------------------------------------------------------

auswahlliste <- wgdaten %>%
  tibble() %>%
  group_by(stadtteil_geocoding) %>%
  summarise(fallzahl = n()) %>%
  arrange(desc(fallzahl)) %>%
  head(5) 

auswahlliste_namen <- auswahlliste %>%
  pull(stadtteil_geocoding)

auswahlliste_anteil <-  round(sum(auswahlliste$fallzahl) / nrow(wgdaten),3)*100

grenzen_stadtteile <- grenzen_stadtteile %>%
  mutate(top5 = stadtteil %in% auswahlliste_namen)


farbe_grün <- "darkgreen" 
farbe_rot <- "darkred"

Karte_einzelneWGs <- ggplot() +
  geom_sf(data = grenzen_stadtteile, colour = "darkgray",
          fill = "gray95") +
  geom_sf(data = grenzen_stadtteile %>% filter(top5 == TRUE), colour = "darkgray",
          fill = "gray85", linewidth = 0.5) +
  geom_sf(data = wgdaten, colour = farbe_grün, fill = farbe_grün, 
          size = 0.05, shape = 1, linewidth= 0.1, alpha = 0.75) +

  labs(title = glue("Lage der Wohngemeinschaften in {stadt}"),
       subtitle = glue("Die <b><span style='color:{farbe_grün}'>Lage der Wohngemeinschaften</span></b> konzentriert sich auf die fünf beliebsteten Bezirke <b><span style='color:gray50'>({auswahlliste_namen[1]}, {auswahlliste_namen[2]}, {auswahlliste_namen[3]}, {auswahlliste_namen[4]}, {auswahlliste_namen[5]})</span></b>. In diesen Stadtteilen bedfinden sich **{auswahlliste_anteil}%** der WG's"),
       caption = glue("Gesamt: {nrow(wgdaten)} Punkte")) +
  theme_void() +
  theme(plot.title = element_text(size = 14, family = "domine", colour = "black",
                                  margin = margin(l = -4, t = 5)),
        plot.subtitle = element_textbox_simple(family = "franklin", size = 8.5, colour = "black",
                                         margin = margin(l = 5, b = 5, t = 6),
                                         lineheight = 1.2),
        plot.caption = element_text(family = "franklin"))

file_save <- glue("Abbildungen/Karte_einzelneWGs/Karte_einzelneWGs_{stadt}.png")
ggsave(filename = file_save, plot = Karte_einzelneWGs, 
       dpi = 300)

shell.exec(normalizePath(file_save))

