
library(tidyverse)
library(sf)
library(glue)
library(DBI)
library(ggtext)
library(tidytext)
library(stopwords)

font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()

## Geodaten --------------------------------------------------------------------

Stadtteile_Berlin <- st_read("C:\\Users\\hellm\\Downloads\\bezirksgrenzen.shp\\bezirksgrenzen.shp")

St_Teile <- as.data.frame(Stadtteile_Berlin) %>%
  select(Gemeinde_n) %>% pull()


## WG-Daten --------------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

sql <- glue_sql("
  SELECT *
  FROM analysedaten
  WHERE stadt = 'Berlin'
      AND stadtteil IN ({teile*})
", teile = St_Teile, .con = con_lokal)

WGdaten_Berlin <- dbGetQuery(con_lokal, sql)

WGdaten_Berlin <- WGdaten_Berlin %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(month(datum_scraping) == 9)



## Parameter -------------------------------------------------------------------


## Anteil Befristung -----------------------------------------------------------

WGdaten_Berlin %>%
  mutate(grenzwert = ifelse(gesamtmiete > 380, "drüber", "drunter")) %>%
  group_by(grenzwert) %>%
  summarise(n = n(),
            median_größe = median(zimmergröße),
            max_größe = max(zimmergröße),
            min_größe = min(zimmergröße))


# Anteil Befristung ------------------------------------------------------------

WGdaten_Berlin %>%
     mutate(Befristet = ifelse(is.na(befristungsdauer),F,T)) %>%
     group_by(Befristet) %>%
     summarize(n = n()) %>%
     mutate(anteil = round(n/sum(n),3)*100)


## Beliebtester Stadtteil ------------------------------------------------------
  
WGdaten_Berlin %>%
  group_by(stadtteil) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


## Durchschnittsmiete ----------------------------------------------------------

WGdaten_Berlin %>%
  filter(befristungsdauer > 30 | is.na(befristungsdauer)) %>%
  summarise(median_miete = median(gesamtmiete, na.rm = T))


## Beliebtestes Adjektiv -------------------------------------------------------

tibble(title = WGdaten_Berlin$titel) %>%
  unnest_tokens(word, title, strip_numeric = T) %>%
  filter(nchar(word) > 2) %>%  
  count(word, sort = TRUE) %>%
  anti_join(tibble(word = stopwords("de")), by = "word") %>%
  print(n = 50)


## Histogram Infotafel ---------------------------------------------------------

Histogram_Miete <- WGdaten_Berlin %>%
  filter(befristungsdauer > 30 | is.na(befristungsdauer)) %>%
  filter(gesamtmiete >= quantile(gesamtmiete, 0.01, na.rm = TRUE),
         gesamtmiete <= quantile(gesamtmiete, 0.99, na.rm = TRUE)) %>%
  
  ggplot(aes(x=gesamtmiete)) +
  geom_histogram(fill= "gray80", colour= "gray20", binwidth = 30) +
  scale_x_continuous(breaks = c(300,450,600,750,900,1050),
                     limits = c(150,1170)) +
  
  labs(x = "Zimmermiete in Euro",
       y = "Anzahl der Inserate") +
  theme_minimal() +
  theme(
    axis.title = element_markdown(family = "franklin", size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_markdown(family = "franklin", size = 15),
    axis.line = element_line(),
    panel.grid = element_line(colour = "gray80")
  )

file_save <- "Abbildungen/Histogram_Miete_Berlin.png"
ggsave(filename = file_save, plot = Histogram_Miete, 
       width = 7, height = 4.5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))


