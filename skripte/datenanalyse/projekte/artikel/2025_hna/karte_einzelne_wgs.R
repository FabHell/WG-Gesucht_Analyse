


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######      KARTE EINZELNE WGS KASSEL      #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(sf)
library(DBI)
library(glue)
library(tidygeocoder)
library(showtext)
library(ggtext)


source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")
 

## Laden Geo-Daten -------------------------------------------------------------

Wasser_Kassel <- st_read("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\daten\\geodaten\\Wasser\\DLM250_Gewässerfläche.shp") %>%
  filter(Objekt_ID %in% c("DEBKGDL20000BZ37", "DEBKGDL20000BZEH",
                          "DEBKGDL200009ZUM", "DEBKGDL200009YW8")) 

Stadtteile_Kassel <- st_read("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\daten\\geodaten\\Kassel\\Stadtteile\\Geo_Stadtteile_Kassel.shp") %>%
  st_transform(crs = 25832) 

Uni_Kassel <- tibble(
  Ort = c("Hauptcampus", "Kunstuni", "Murhadstraße", "AVZ"),
  lon = c(9.505947531511303, 9.487369376868523, 9.473807178256493, 9.448924309219835),
  lat = c(51.3219196627738, 51.304459942946025, 51.31134133430143, 51.28174419310388))%>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(Stadtteile_Kassel))



## Laden WG-Daten --------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

St_Teile <- as.data.frame(Stadtteile_Kassel) %>%
  select(stadtteil) %>% pull()

sql <- glue_sql("
  SELECT *
  FROM analysedaten
  WHERE stadt = 'Kassel'
    AND (gesamtmiete >= 200 AND gesamtmiete <= 1300)
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
 ", .con = con_lokal)

WGdaten_Kassel <- dbGetQuery(con_lokal, sql) %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  st_transform(crs = 25832) %>%
  st_join(Stadtteile_Kassel) %>%
  filter(!is.na(stadtteil)) %>%
  select(-stadtteil)

## Erstellen der Karte ---------------------------------------------------------

Auswahlliste_Fallzahl <- WGdaten_Kassel %>%
  tibble() %>%
  group_by(stadtteil_geocoding) %>%
  summarise(fallzahl = n()) %>%
  arrange(desc(fallzahl)) 

Stadtteile_Kassel <- Stadtteile_Kassel %>%
  mutate(top5 = stadtteil %in% Auswahlliste_Fallzahl[1:5, 1, drop = TRUE])

farbe_grün <- "darkgreen" 
farbe_rot <- "darkred"

Karte_einzelneWGs <- ggplot() +
  geom_sf(data = Stadtteile_Kassel %>% filter(top5 == FALSE), colour = "darkgray",
          fill = "gray95") +
  geom_sf(data = Stadtteile_Kassel %>% filter(top5 == TRUE), colour = "darkgray",
          fill = "gray85", linewidth = 0.5) +
  geom_sf(data = Wasser_Kassel, colour = "darkblue", fill = "darkblue") +
  geom_sf(data = Uni_Kassel, colour = farbe_rot, fill = farbe_rot, size = 2,
          shape = 23) +
  geom_sf(data = WGdaten_Kassel, colour = farbe_grün, fill = farbe_grün, 
          size = 0.5, shape = 21, alpha = 0.75) +
  labs(title = "Wohngemeinschaften in Kassel",
       subtitle = glue("Die <b><span style='color:{farbe_grün}'>Lage der Wohngemeinschaften</span></b> konzentriert sich vor allem um die drei zentralen <b><span style='color:{farbe_rot}'>Standorte der Universität Kassel</span></b>. In den fünf beliebsteten Vierteln <b><span style='color:gray50'>({Auswahlliste_Fallzahl[1,1]}, {Auswahlliste_Fallzahl[2,1]}, {Auswahlliste_Fallzahl[3,1]}, {Auswahlliste_Fallzahl[4,1]}, {Auswahlliste_Fallzahl[5,1]})</span></b> befinden sich **{round(sum(Auswahlliste_Fallzahl[1:5,]$fallzahl)/sum(Auswahlliste_Fallzahl$fallzahl),3)*100}%** der WG's.")) +
  theme_dunkel() +
  theme(plot.title = element_text(size = 12, family = plot_title_family, colour = plot_title_color,
                                  margin = margin(l = -4, b = 10)),
        plot.subtitle = element_textbox_simple(family = "domine", size = 8, colour = axis_title_color,
                                         margin = margin(l = 8, b = 5, t = 6),
                                         lineheight = 1.2),
        panel.grid.major.x = element_line(color = panel_background_color),
        panel.grid.major.y = element_line(color = panel_background_color),
        panel.grid.minor.x = element_line(color = panel_background_color),
        panel.grid.minor.y = element_line(color = panel_background_color),
        axis.line = element_blank(),
        axis.text = element_blank())

file_save <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\artikel\\2025_hna\\Karte_einzelneWGs_Kassel.png"
ggsave(filename = file_save, plot = Karte_einzelneWGs, 
       width = 7, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

