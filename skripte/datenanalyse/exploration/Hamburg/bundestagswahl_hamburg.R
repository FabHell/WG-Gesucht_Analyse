


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######           WAHLDATEN HAMBURG         #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(sf)
library(DBI)
library(glue)



## Wahlbezogene Daten laden ----------------------------------------------------


wahlergebnisse <- read.csv("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Kontextdaten_Städte\\Hamburg\\ergebnis-download.csv", sep = ";") 

wahlergebnisse_aufb <- wahlergebnisse %>%
  filter(Erfassungsgebietsart == "STIMMBEZIRK") %>%
  filter(Direktstimmen.gueltige..D. > 0) %>%
  mutate(
    Perc_SPD = F1 / Listenstimmen.gueltige..F. * 100,
    Perc_Grüne = F2 / Listenstimmen.gueltige..F. * 100,
    Perc_CDU = F3 / Listenstimmen.gueltige..F. * 100,
    Perc_FDP = F4 / Listenstimmen.gueltige..F. * 100,
    Perc_Linke = F5 / Listenstimmen.gueltige..F. * 100,
    Perc_AFD = F6 / Listenstimmen.gueltige..F. * 100,
    Perc_BSW = F13 / Listenstimmen.gueltige..F. * 100
  ) %>%
  mutate(Meiste_Zweitstimmen = case_when(
    Perc_SPD > Perc_Grüne & Perc_SPD > Perc_CDU & Perc_SPD > Perc_FDP & Perc_SPD > Perc_Linke & Perc_SPD > Perc_AFD & Perc_SPD > Perc_BSW ~ "SPD",
    Perc_Grüne > Perc_SPD & Perc_Grüne > Perc_CDU & Perc_Grüne > Perc_FDP & Perc_Grüne > Perc_Linke & Perc_Grüne > Perc_AFD & Perc_Grüne > Perc_BSW ~ "Grüne",
    Perc_CDU > Perc_SPD & Perc_CDU > Perc_Grüne & Perc_CDU > Perc_FDP & Perc_CDU > Perc_Linke & Perc_CDU > Perc_AFD & Perc_CDU > Perc_BSW ~ "CDU",
    Perc_FDP > Perc_SPD & Perc_FDP > Perc_Grüne & Perc_FDP > Perc_CDU & Perc_FDP > Perc_Linke & Perc_FDP > Perc_AFD & Perc_FDP > Perc_BSW ~ "FDP",
    Perc_Linke > Perc_SPD & Perc_Linke > Perc_Grüne & Perc_Linke > Perc_CDU & Perc_Linke > Perc_FDP & Perc_Linke > Perc_AFD & Perc_Linke > Perc_BSW ~ "Linke",
    Perc_AFD > Perc_SPD & Perc_AFD > Perc_Grüne & Perc_AFD > Perc_CDU & Perc_AFD > Perc_FDP & Perc_AFD > Perc_Linke & Perc_AFD > Perc_BSW ~ "AFD",
    Perc_BSW > Perc_SPD & Perc_BSW > Perc_Grüne & Perc_BSW > Perc_CDU & Perc_BSW > Perc_FDP & Perc_BSW > Perc_Linke & Perc_BSW > Perc_AFD ~ "BSW",
    TRUE ~ "uneindeutig"
  )) %>%
  select(Stadtteil, Erfassungsgebietsnummer, Meiste_Zweitstimmen,
         starts_with("Perc")) %>%
  mutate(Erfassungsgebietsnummer = as.character(Erfassungsgebietsnummer)) 


wahlbezirke_geo <- read_sf("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Geodaten\\Hamburg\\Geo_Wahlbezirke\\de_hh_up_wahlbezirke_EPSG_4326.json") %>%
  st_transform(crs = 25832) %>%
  select(Erfassungsgebietsnummer = wahlbezirksnummer) %>%
  left_join(wahlergebnisse_aufb, by = "Erfassungsgebietsnummer") %>%
  filter(Erfassungsgebietsnummer != "14201") 

wahlbezirke_geo %>%
  ggplot() +
  geom_sf(aes(fill=-Perc_AFD), show.legend = F)



# WGdaten laden ----------------------------------------------------------------


con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

stadt <- "Hamburg"

query_wgdaten <- glue_sql("
  SELECT stadtteil_geocoding, gesamtmiete, geolocation
  FROM analysedaten
    WHERE stadt = {stadt}
      AND geolocation IS NOT NULL
", .con = con_lokal)

daten_wohnungen <- dbGetQuery(con_lokal, query_wgdaten) 

daten_wohnungen_aufb <- daten_wohnungen %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  st_transform(crs = 25832) %>%
  filter(!is.na(stadtteil_geocoding)) %>%
  filter(!is.na(gesamtmiete))



# Daten kombinieren und erste Analysen -----------------------------------------


analysedaten_wahlen <- daten_wohnungen_aufb %>%
  st_join(wahlbezirke_geo)

table(analysedaten_wahlen$Meiste_Zweitstimmen)

cor(analysedaten_wahlen$gesamtmiete, analysedaten_wahlen$Perc_Grüne)
cor(analysedaten_wahlen$gesamtmiete, analysedaten_wahlen$Perc_AFD)


analysedaten_wahlen %>%
  filter(Meiste_Zweitstimmen != "uneindeutig") %>%
  st_drop_geometry() %>%
  group_by(Meiste_Zweitstimmen) %>%
  summarise(Median_Miete = median(gesamtmiete),
            Anzahl_WGs = n()) %>%
  arrange(desc(Median_Miete))
