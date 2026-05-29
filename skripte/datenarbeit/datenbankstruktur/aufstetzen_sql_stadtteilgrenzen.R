


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######           AUFSETZEN GEO-SQL         #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(DBI)
library(odbc)
library(tidyverse)



## Lokale Geodaten laden und aufbereiten ---------------------------------------

stadt <- "Hamburg"
pfad <- "Daten/Geodaten/Hamburg/Geo_Stadtteile/Stadtteile_Hamburg.shp"

Geodaten_Stadtteile <- st_read(pfad) %>%
  filter(stadtteil_ != "Neuwerk") %>%
  select(stadtteil = stadtteil_, stadtbezirk = bezirk_nam) %>%
  mutate(
    land = "Deutschland",
    stadt = stadt,
    .before = stadtteil
  ) %>%
  st_transform(4326)



## Lokaler SQL -----------------------------------------------------------------

# usethis::edit_r_environ()    

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")



## Tabelle anlegen -------------------------------------------------------------

dbExecute(con_lokal, "DROP TABLE IF EXISTS geodaten_staedte")

dbExecute(con_lokal, "
CREATE TABLE geodaten_staedte (
    land NVARCHAR(100),
    stadt NVARCHAR(100),
    stadtteil NVARCHAR(100),
    geom GEOGRAPHY
)")



## Geodaten aufbereiten --------------------------------------------------------

Geodaten_Stadtteile <- Geodaten_Stadtteile %>%
  mutate(geom = st_as_text(geometry)) %>%
  select(land, stadt, stadtteil, geom) %>%
  tibble() 

for (i in 1:nrow(Geodaten_Stadtteile)) {
  land <- Geodaten_Stadtteile$land[i]
  stadt <- Geodaten_Stadtteile$stadt[i]
  stadtteil <- Geodaten_Stadtteile$stadtteil[i]
  geom_wkt <- Geodaten_Stadtteile$geom[i]
  
  geom_wkt <- gsub("'", "''", geom_wkt)
  
  sql <- glue::glue("
    INSERT INTO geodaten_staedte (land, stadt, stadtteil, geom)
    VALUES ('{land}', '{stadt}', '{stadtteil}', geography::STGeomFromText('{geom_wkt}', 4326))
  ")
  
  dbExecute(con_lokal, sql)
}



## Geodaten aus SQL laden ------------------------------------------------------

df <- dbGetQuery(con_lokal, "
  SELECT land, stadt, stadtteil, geom.STAsText() AS wkt
  FROM geodaten_staedte
") %>%
  st_as_sf(wkt = "wkt", crs = 4326) 

df %>%
  ggplot() +
  geom_sf()



