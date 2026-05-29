

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          DATENUPDATE GEO-SQL        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(sf)
library(DBI)
library(glue)
library(futile.logger)

# usethis::edit_r_environ()      


## SQL-Verbindung herstellen ---------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

con_vpn <- dbConnect(odbc::odbc(),
                     Driver   = "SQL Server",
                     Server   = Sys.getenv("SERVER_SQL_VPN"),
                     Database = Sys.getenv("DATABASE_SQL_VPN"),
                     UID      = Sys.getenv("UID_SQL_VPN"),
                     PWD      = Sys.getenv("PWD_SQL_VPN"),
                     Encrypt  = "No")


## Tabelle anlegen -------------------------------------------------------------

# dbExecute(con_lokal, "DROP TABLE IF EXISTS geodaten_stadtteile")
# 
# dbExecute(con_lokal, "
# CREATE TABLE geodaten_stadtteile (
#     stadt NVARCHAR(100),
#     stadtteil NVARCHAR(100),
#     geom GEOGRAPHY
# )")


## Städtenamen lokale Geodaten -------------------------------------------------

query_geo_stadtteile_lokal <- glue_sql("
  SELECT stadt
  FROM geodaten_stadtteile
", .con = con_lokal)

staedtename_geo_lokal <- dbGetQuery(con_lokal, query_geo_stadtteile_lokal) %>%
  distinct() %>% pull()


## Städtenamen VPN-Geodaten ----------------------------------------------------

query_geo_stadtteile_vpn <- glue_sql("
  SELECT stadt
  FROM geodaten_stadtteile
", .con = con_vpn)

staedtename_geo_vpn <- dbGetQuery(con_vpn, query_geo_stadtteile_vpn) %>%
  distinct() %>% pull()


## Unterschied lokale und VPN-Geodaten -----------------------------------------

staedtename_neu <- setdiff(staedtename_geo_vpn, staedtename_geo_lokal)


## Neue Geodaten laden ---------------------------------------------------------

if (is.null(staedtename_neu)) {
  
  query_geo_stadtteile_neu <- glue_sql("
    SELECT 
      stadt,
      stadtteil,
      geom.STAsText() AS geom_wkt
    FROM geodaten_stadtteile
    WHERE stadt IN ({staedtename_neu*})
  ", .con = con_vpn)
  
  geodaten_neu <- dbGetQuery(con_vpn, query_geo_stadtteile_neu)
  
  geodaten_neu <- st_as_sf(geodaten_neu, wkt = "geom_wkt", crs = 4326)
  
} else {
  message("Geodatenbank bereits aktuell")
  stop()
}


# Neue Geodaten in lokaler Datenbank speichern ---------------------------------

geodaten_neu_aufb <- geodaten_neu %>%
  mutate(geom = st_as_text(geom_wkt)) %>%
  st_drop_geometry()

for (i in 1:nrow(geodaten_neu_aufb)) {
  
  message("Speicher:", geodaten_neu_aufb$stadtteil[i])
  
  stadt <- geodaten_neu_aufb$stadt[i]
  stadtteil <- geodaten_neu_aufb$stadtteil[i]
  geom_wkt <- geodaten_neu_aufb$geom[i]
  
  geom_wkt <- gsub("'", "''", geom_wkt)
  
  sql <- glue("
    INSERT INTO geodaten_stadtteile (stadt, stadtteil, geom)
    VALUES ('{stadt}', '{stadtteil}', geography::STGeomFromText('{geom_wkt}', 4326))
  ")
  
  dbExecute(con_lokal, sql)
}


# Geodaten laden und darstellen ------------------------------------------------

# lokale_geodaten <- dbGetQuery(con_lokal, "
#   SELECT stadt, stadtteil, geom.STAsText() AS wkt
#   FROM geodaten_stadtteile
# ") %>%
#   st_as_sf(wkt = "wkt", crs = 4326)
# 
# lokale_geodaten %>%
#   ggplot() +
#   geom_sf()
