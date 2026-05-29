

library(tidyverse)
library(DBI)
library(glue)

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

stadt_filter <- "Hamburg"

sql <- glue_sql("
  SELECT *
  FROM analysedaten
  WHERE stadt = {stadt_filter}
 ", teile = St_Teile, .con = con_lokal)

Analysedaten <- dbGetQuery(con_lokal, sql)


Verbindungen <- Analysedaten %>%
  mutate(freitext_zimmer = paste0(freitext_zimmer_1, freitext_zimmer_2, freitext_zimmer_3),
         freitext_wg_leben = paste0(freitext_wg_leben_1, freitext_wg_leben_2, freitext_wg_leben_3),
         freitext_lage = paste0(freitext_lage_1, freitext_lage_2, freitext_lage_3),
         freitext_sonstiges = paste0(freitext_sonstiges_1, freitext_sonstiges_2, freitext_sonstiges_3)
         ) %>%
  select(geschlecht_ges, personenzahl, zusammensetzung, wg_art, 
         freitext_zimmer, freitext_lage, freitext_wg_leben, freitext_sonstiges
         ) %>%
  mutate(
    anz_weiblich = as.numeric(str_extract(zusammensetzung, "\\d+(?=w)")),
    anz_männlich = as.numeric(str_extract(zusammensetzung, "\\d+(?=m)")),
    anz_divers   = as.numeric(str_extract(zusammensetzung, "\\d+(?=d)"))
  ) %>%
  filter(geschlecht_ges == "Mann" & personenzahl >= 6 & anz_weiblich == 0 &
         anz_divers == 0 & anz_männlich != 0) 
  