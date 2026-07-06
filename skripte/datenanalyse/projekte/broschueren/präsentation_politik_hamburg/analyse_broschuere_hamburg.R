
library(tidyverse)
library(sf)
library(glue)
library(DBI)
library(ggtext)
library(tidytext)
library(stopwords)

source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")


## WG-Daten --------------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

sql <- glue_sql("
  SELECT gesamtmiete, zimmergröße, befristungsdauer, stadtteil_geocoding, titel
  FROM analysedaten
  WHERE stadt = 'Hamburg'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
", .con = con_lokal)

daten_staedte_roh <- dbGetQuery(con_lokal, sql) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) 


## Parameter -------------------------------------------------------------------


## Zimmergröße -----------------------------------------------------------------

daten_staedte_roh %>%
  mutate(grenzwert = ifelse(gesamtmiete > 380, "drüber", "drunter")) %>%
  group_by(grenzwert) %>%
  summarise(n = n(),
            median_größe = median(zimmergröße),
            max_größe = max(zimmergröße),
            min_größe = min(zimmergröße))


# Anteil Befristung ------------------------------------------------------------

daten_staedte_roh %>%
  mutate(Befristet = ifelse(is.na(befristungsdauer),F,T)) %>%
  group_by(Befristet) %>%
  summarize(n = n()) %>%
  mutate(anteil = round(n/sum(n),3)*100)


## Beliebtester Stadtteil ------------------------------------------------------

daten_staedte_roh %>%
  group_by(stadtteil_geocoding) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


## Durchschnittsmiete ----------------------------------------------------------

daten_staedte_roh %>%
  summarise(median_miete = median(gesamtmiete, na.rm = T))


## Beliebtestes Adjektiv -------------------------------------------------------

tibble(title = daten_staedte_roh$titel) %>%
  unnest_tokens(word, title, strip_numeric = T) %>%
  filter(nchar(word) > 2) %>%  
  count(word, sort = TRUE) %>%
  anti_join(tibble(word = stopwords("de")), by = "word") %>%
  print(n = 50)


## Histogram Infotafel ---------------------------------------------------------

Histogram_Miete <- daten_staedte_roh %>%

  ggplot(aes(x=gesamtmiete)) +
  geom_histogram(fill= "gray80", colour= "gray20", binwidth = 50) +
  scale_x_continuous(limits = c(min(daten_staedte_roh$gesamtmiete)-50,
                                max(daten_staedte_roh$gesamtmiete)-25)) +
  
  labs(x = "Zimmermiete in Euro",
       y = "Anzahl der Inserate") +
  theme_minimal() +
  theme(
    axis.title = element_markdown(family = "franklin", size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_markdown(family = "franklin", size = 15),
    axis.line = element_line(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    panel.grid = element_line(colour = "gray80")
  )

file_save <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\broschueren\\präsentation_politik_hamburg\\Histogram_Miete_Hamburg.png"
ggsave(filename = file_save, plot = Histogram_Miete, 
       width = 7, height = 4.5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))


