


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######         DATENUPDATE SQL-SERVER      #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############

                 
library(DBI)
library(tidyverse)
library(futile.logger)
library(glue)

# usethis::edit_r_environ()      



## Lokale ID's laden -----------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

flog.info("Verbindung zu lokalem SQL erfolgreich")


max_id <- dbGetQuery(con_lokal, "SELECT ISNULL(MAX(id), 0) AS last_id FROM analysedaten") %>%
  pull()

flog.info("Lokale MAX-ID erfolgreich geladen")


## SQL Scraping-PC -------------------------------------------------------------

con_vpn <- dbConnect(odbc::odbc(),
                     Driver   = "SQL Server",
                     Server   = Sys.getenv("SERVER_SQL_VPN"),
                     Database = Sys.getenv("DATABASE_SQL_VPN"),
                     UID      = Sys.getenv("UID_SQL_VPN"),
                     PWD      = Sys.getenv("PWD_SQL_VPN"),
                     Encrypt  = "no")

flog.info("Verbindung zu VPN SQL erfolgreich")

new_data <- if (is.na(max_id)) {
  
  dbGetQuery(con_vpn, "
    SELECT * 
    FROM analysedaten
  ")
  
} else {
  
  sql_query <- glue_sql("
    SELECT *
    FROM analysedaten
    WHERE id > {max_id}
  ", .con = con_vpn)
  
  dbGetQuery(con_vpn, sql_query)
  
}

flog.info("Daten erfolgreich vom VPN SQL abgerufen")


## Neue Daten speichern --------------------------------------------------------

if (nrow(new_data) > 0) {
  
  con_lokal %>% dbBegin()
  
  tryCatch({
    
    dbWriteTable(con_lokal, "analysedaten", new_data,
                 append = TRUE)
    con_lokal %>% dbCommit()
    flog.info("%d neue Anzeigen in lokaler Datenbank gespeichert", nrow(new_data))
    
  }, error = function(e) {
    
    con_lokal %>% dbRollback()
    flog.error("Fehler: %s", e$message)
    
  })

} else {
  flog.info("Keine neuen Daten in Scrapingdatenbank")
}


dbDisconnect(con_lokal)
dbDisconnect(con_vpn)


