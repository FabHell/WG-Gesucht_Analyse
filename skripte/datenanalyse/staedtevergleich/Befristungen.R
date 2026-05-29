


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          PHÄNOMEN BEFRISTUNG        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(DBI)
library(glue)



## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

sql <- glue_sql("
  SELECT stadt, stadtteil, befristungsdauer
  FROM analysedaten
    WHERE stadtteil IS NOT NULL
 ", .con = con_lokal)

WGdaten_ges <- dbGetQuery(con_lokal, sql)



## Anteil Befristung von allen Anzeigen ----------------------------------------

WGdaten_ges %>%
  mutate(befristung = ifelse(is.na(befristungsdauer), F, T)) %>%
  group_by(stadt, befristung) %>%
  summarise(n = n()) %>%
  group_by(stadt) %>%
  mutate(prozent = n / sum(n) * 100) %>%
  filter(befristung == T) %>%
  arrange(desc(prozent), .by_group = F) %>%
  print(n = Inf)



## Phänomen Kurzbefristungen ---------------------------------------------------

WGdaten_ges %>%
  mutate(kurzbefristung = ifelse(!is.na(befristungsdauer) 
                                 & befristungsdauer < 30, T, F)) %>%
  group_by(stadt, kurzbefristung) %>%
  summarise(n = n()) %>%
  group_by(stadt) %>%
  mutate(prozent = n / sum(n) * 100) %>%
  filter(kurzbefristung == T) %>%
  arrange(desc(prozent), .by_group = F) %>%
  print(n = Inf)

