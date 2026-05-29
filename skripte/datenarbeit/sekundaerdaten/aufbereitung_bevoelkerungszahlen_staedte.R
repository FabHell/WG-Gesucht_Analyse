
library(tidyverse)
library(readxl)
library(glue)
library(DBI)

kontextdaten_städte <- read_excel("Daten/Kontextdaten/05-staedte.xlsx", 
                                  sheet = "Städte", skip = 1)

kontextdaten_städte_aufb <- kontextdaten_städte %>%
  select(stadt = Stadt, 
         plz = Postleitzahl,
         fläche_qm = `Fläche km² ¹⁾`,
         bev_insgesamt = `Bevölkerung auf Grundlage des ZENSUS 2022 ²⁾ insgesamt`,
         bev_männlich = `Bevölkerung auf Grundlage des ZENSUS 2022 ²⁾ männlich`,
         bev_weiblich = `Bevölkerung auf Grundlage des ZENSUS 2022 ²⁾ weiblich`,
         bev_km = `Bevölkerung auf Grundlage des ZENSUS 2022 ²⁾             je km²`) %>%
  mutate(stadt = str_remove(stadt, ", Stadt"),
         stadt = str_remove(stadt, ", Landeshauptstadt"),
         stadt = str_remove(stadt, ", Universitätsstadt"),
         stadt = str_remove(stadt, ", Universitäts- und Hansestadt"),
         stadt = str_remove(stadt, ", Hanse- und Universitätsstadt"),
         stadt = str_remove(stadt, ", Freie und Hansestadt"),
         stadt = str_remove(stadt, ", Hansestadt"),
         stadt = str_remove(stadt, ", Wissenschaftsstadt"),
         stadt = str_remove(stadt, ", documenta-Stadt"),
         stadt = str_replace(stadt, "Oldenburg (Oldenburg)", "Oldenburg"))


con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

query_wg_art <- glue_sql("
  SELECT stadt, wg_art
  FROM analysedaten
    WHERE stadtteil IS NOT NULL
      AND land = 'Deutschland'
", .con = con_lokal)

städte_wg_art <- dbGetQuery(con_lokal, query_wg_art)

städte_wg_art_aggr <- städte_wg_art %>%
  mutate(azubi = str_detect(wg_art, "Azubi-WG")) %>%
  group_by(stadt) %>%
  summarise(azubi_perc = mean(azubi, na.rm = T)) %>%
  left_join(kontextdaten_städte_aufb, by = "stadt") %>%
  filter(stadt != "Oldenburg")

cor(städte_wg_art_aggr$azubi_perc, städte_wg_art_aggr$bev_männlich)

