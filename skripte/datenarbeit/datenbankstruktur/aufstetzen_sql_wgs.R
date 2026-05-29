


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          AUFSETZEN SQL-SERVER       #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(DBI)
library(odbc)
library(tidyverse)

# usethis::edit_r_environ()    


## Lokaler SQL -----------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")



## Tabellenstruktur Analysedaten -----------------------------------------------

# dbExecute(con_lokal, "DROP TABLE analysedaten")

dbExecute(con_lokal, "
CREATE TABLE analysedaten (
    id INT,
    land NVARCHAR(50),
    bundesland NVARCHAR(50),
    stadt NVARCHAR(50),
    titel NVARCHAR(1000),
    link NVARCHAR(1000),
    profil NVARCHAR(200),
    stadtteil_webseite NVARCHAR(100),
    stadtteil_geocoding NVARCHAR(100),
    postleitzahl FLOAT,
    straße NVARCHAR(100),
    geolocation NVARCHAR(100),
    gesamtmiete FLOAT,
    kaltmiete FLOAT,
    nebenkosten FLOAT,
    kaution FLOAT,
    sonstige_kosten FLOAT,
    ablösevereinbarung FLOAT,
    zimmergröße FLOAT,
    personenzahl FLOAT,
    wohnungsgröße FLOAT,
    bewohneralter NVARCHAR(50),
    einzugsdatum DATE,
    zusammensetzung NVARCHAR(50),
    befristung_enddatum DATE,
    befristungsdauer FLOAT,
    geschlecht_ges NVARCHAR(100),
    alter_ges NVARCHAR(50),
    wg_art NVARCHAR(1000),
    rauchen NVARCHAR(100),
    sprache NVARCHAR(500),
    angaben_zum_objekt NVARCHAR(2000),
    freitext_zimmer_1 NVARCHAR(4000),
    freitext_zimmer_2 NVARCHAR(4000),
    freitext_zimmer_3 NVARCHAR(4000),
    freitext_lage_1 NVARCHAR(4000),
    freitext_lage_2 NVARCHAR(4000),
    freitext_lage_3 NVARCHAR(4000),
    freitext_wg_leben_1 NVARCHAR(4000),
    freitext_wg_leben_2 NVARCHAR(4000),
    freitext_wg_leben_3 NVARCHAR(4000),
    freitext_sonstiges_1 NVARCHAR(4000),
    freitext_sonstiges_2 NVARCHAR(4000),
    freitext_sonstiges_3 NVARCHAR(4000),
    seite_scraping INT,
    uhrzeit_scraping NVARCHAR(10),
    datum_scraping DATE
)")



## Befehlssammlung SQL-Server --------------------------------------------------


# Tabellen anzeigen
dbListTables(con_lokal)

# Spalten einer Tabelle anzeigen
dbListFields(con_lokal, "WG-Gesucht")

# Query für Datenabfrage
Test <- dbGetQuery(con, "
  SELECT Link
  FROM Analysedaten
") %>% pull()
 

