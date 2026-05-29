


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

font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()


## Laden Geo-Daten -------------------------------------------------------------

Wasser_Kassel <- st_read("Daten/Geodaten/Kassel/Wasser/DLM250_Gewässerfläche.shp") %>%
  filter(Objekt_ID %in% c("DEBKGDL20000BZ37", "DEBKGDL20000BZEH",
                          "DEBKGDL200009ZUM", "DEBKGDL200009YW8")) 

Stadtteile_Kassel <- st_read("Daten/Geodaten/Kassel/Stadtteile/Geo_Stadtteile_Kassel.shp")

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
    AND stadtteil IN ({teile*})
 ", teile = St_Teile, .con = con_lokal)

WGdaten_Kassel <- dbGetQuery(con_lokal, sql)



## Geocoding WG-Daten ----------------------------------------------------------

WGdaten_Kassel_geo <- WGdaten_Kassel %>%
  geocode(method = "osm", country = land, city = stadt,
          postalcode = postleitzahl, street = straße) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, na.fail = FALSE) %>%
  st_transform(crs = st_crs(Stadtteile_Kassel)) 

WGdaten_Kassel_geo2 <- WGdaten_Kassel_geo %>%
  st_join(Stadtteile_Kassel %>% select(filter = stadtteil)) %>%
  filter(!is.na(filter))



## Erstellen der Karte ---------------------------------------------------------

Auswahlliste_Fallzahl <- WGdaten_Kassel_geo2 %>%
  tibble() %>%
  group_by(stadtteil) %>%
  summarise(fallzahl = n()) %>%
  arrange(desc(fallzahl)) 

Stadtteile_Kassel <- Stadtteile_Kassel %>%
  mutate(top5 = stadtteil %in% Auswahlliste_Fallzahl[1:5, 1, drop = TRUE])

farbe_grün <- "darkgreen" 
farbe_rot <- "darkred"


Karte_einzelneWGs_Broschüre <- ggplot() +
  geom_sf(data = Stadtteile_Kassel , colour = "gray35",
          fill = "gray65", linewidth = 0.5) +
  geom_sf(data = Stadtteile_Kassel, colour = "gray35",
          fill = "gray85") +
  geom_sf(data = Wasser_Kassel, colour = "darkblue", fill = "darkblue") +
  geom_sf(data = Uni_Kassel, colour = farbe_rot, fill = farbe_rot, size = 2,
          shape = 23) +
  geom_sf(data = WGdaten_Kassel_geo2, colour = farbe_grün, fill = farbe_grün, 
          size = 1, shape = 1, alpha = 0.75) +
  theme_void() +
  theme(plot.title = element_text(size = 12, family = "domine", colour = "black",
                                  margin = margin(l = -4, t = 5)),
        plot.subtitle = element_markdown(family = "franklin", size = 8, colour = "black",
                                         margin = margin(l = 8, b = 5, t = 6),
                                         lineheight = 1.2))

file_save <- "Abbildungen/Karte_einzelneWGs_Broschüre.png"
ggsave(filename = file_save, plot = Karte_einzelneWGs_Broschüre, 
       width = 7, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

