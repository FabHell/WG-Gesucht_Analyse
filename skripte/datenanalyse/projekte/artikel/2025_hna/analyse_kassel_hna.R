
library(tidyverse)
library(sf)

Stadtteile_Kassel <- st_read("Daten/Geodaten/Kassel/Stadtteile/Geo_Stadtteile_Kassel.shp")

St_Teile <- as.data.frame(Stadtteile_Kassel) %>%
  select(stadtteil) %>% pull()

con_vpn <- dbConnect(odbc::odbc(),
                     Driver   = "SQL Server",
                     Server   = Sys.getenv("SERVER_SQL_VPN"),
                     Database = Sys.getenv("DATABASE_SQL_VPN"),
                     UID      = Sys.getenv("UID_SQL_VPN"),
                     PWD      = Sys.getenv("PWD_SQL_VPN"),
                     Encrypt  = "no")

sql <- glue_sql("
  SELECT *
  FROM analysedaten
  WHERE stadt = 'Kassel'
    AND (gesamtmiete >= 200 AND gesamtmiete <= 1300)
    AND stadtteil IN ({teile*})
 ", teile = St_Teile, .con = con_vpn)




WGdaten_Kassel <- dbGetQuery(con_vpn, sql)


# 

WGdaten_Kassel %>%
  mutate(grenzwert = ifelse(gesamtmiete > 380, "drüber", "drunter")) %>%
  group_by(grenzwert) %>%
  summarise(n = n(),
            median_größe = median(zimmergröße),
            max_größe = max(zimmergröße),
            min_größe = min(zimmergröße))



# Anteil Befristung ------------------------------------------------------------

WGdaten_Kassel %>%
  mutate(Befristet = ifelse(is.na(befristungsdauer),F,T)) %>%
  group_by(Befristet) %>%
  summarize(n = n()) %>%
  mutate(anteil = round(n/sum(n),3)*100)

  
# Anzahl seit 1. August --------------------------------------------------------

WGdaten_Kassel %>%
  filter(datum_scraping > "2025-08-01") %>%
  nrow()



# Häufigkeit nach Wochentag ----------------------------------------------------

WGdaten_Kassel %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(datum_scraping > "2025-05-24") %>%
  count(datum_scraping, name = "faelle_pro_tag") %>%
  mutate(
    wochentag = factor(
      format(datum_scraping, "%A"),
      levels = c("Montag", "Dienstag", "Mittwoch",
                 "Donnerstag", "Freitag", "Samstag", "Sonntag")
    )) %>%
  group_by(wochentag) %>%
  summarise(Durchschnitt = mean(faelle_pro_tag), .groups = "drop")





WGdaten_Kassel %>%
  group_by(stadtteil) %>%
  summarise(n = n(), 
            median_miete = median(gesamtmiete)) %>%
  filter(n > 10) %>%
  arrange(desc(median_miete)) %>%
  ungroup() %>%
  ggplot(aes(x=median_miete, y = reorder(stadtteil, median_miete),
             fill = median_miete)) +
  geom_col() +
  labs(x = "Durchscnittliche Miete",
       y = "Stadtteil") +
  theme_minimal() +
  theme(legend.position = "none")





Test <- WGdaten_Kassel %>%
  group_by(stadtteil) %>%
  summarise(n = n(), 
            median_miete = median(gesamtmiete)) %>%
  filter(n > 10)
  



dummy <- WGdaten_Kassel %>%
  group_by(stadtteil) %>%
  summarise(n = n(), 
            median_miete = median(gesamtmiete)) %>%
  filter(n > 10) %>%
  arrange(desc(median_miete)) %>%
  select(stadtteil) %>% pull()

Test %>%
  mutate(stadtteil = factor(stadtteil, levels = dummy)) %>%
  ggplot(aes(x=median_miete, y= stadtteil)) +
  geom_col()


WGdaten_Kassel %>%
  group_by(datum_scraping) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=datum_scraping, y=n)) +
  geom_line()


WGdaten_Kassel %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  mutate(monat =  format(datum_scraping, "%B")) %>%
  filter(monat %in% c("August", "September")) %>%
  group_by(monat) %>%
  summarise(mean_miete = mean(gesamtmiete),
            median_miete = median(gesamtmiete))
  
WGdaten_Kassel %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  mutate(monat =  format(datum_scraping, "%B")) %>%
  filter(monat %in% c("August", "September")) %>%
  summarise(n = n())

