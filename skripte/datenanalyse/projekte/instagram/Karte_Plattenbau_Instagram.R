


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##            INSTAGRAM - Plattenbau Ost/west           ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################



source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

library(tidyverse)
library(ggrepel)
library(glue)
library(DBI)
library(ggfx)
library(sf)

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")



## Daten laden -----------------------------------------------------------------


con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql <- glue_sql("
  SELECT bundesland, gesamtmiete, stadt, angaben_zum_objekt
  FROM analysedaten
  WHERE land = 'Deutschland'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
", .con = con_lokal)


daten_staedte_roh <- dbGetQuery(con_lokal, sql) %>%
  na.omit() %>%
  group_by(stadt) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-gesamtmiete)


geo_deutschland <- read_sf("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\daten\\geodaten\\Länder\\Grenzen_Deutschland.shp")

geo_bundesländer <- read_sf("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\daten\\geodaten\\Länder\\Grenzen_Deutschland_Länder.shp")

geo_städte <- read_sf("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\daten\\geodaten\\Städte\\Punkt_Stadt.shp") 


## Daten aufbereiten -----------------------------------------------------------


daten_staedte_aufb <- daten_staedte_roh %>%
  mutate(
    haustyp = angaben_zum_objekt %>%
      str_extract_all("Altbau|sanierter Altbau|Neubau|Reihenhaus|Doppelhaus|Einfamilienhaus|Alleinerziehende|Mehrfamilienhaus|Hochhaus|Plattenbau") %>%
      map_chr(~ if (length(.x) == 0) NA_character_ else str_c(.x, collapse = ", "))
  ) %>%
  filter(!is.na(haustyp)) %>%
  group_by(stadt, haustyp) %>%
  summarise(anzahl = n(), .groups = "drop") %>%
  complete(stadt, haustyp, fill = list(anzahl = 0)) %>%
  group_by(stadt) %>%
  mutate(anteil = anzahl/sum(anzahl)*100) %>%
  filter(haustyp == "Plattenbau") %>%
  ungroup() %>%
  mutate(über_mean = ifelse(anteil > mean(anteil), "über", "unter")) %>%
  arrange(desc(anteil))


geo_städte_analyse <- geo_städte %>%
  left_join(daten_staedte_aufb, by = "stadt") %>%
  na.omit()


geo_deutschland_clean <- geo_deutschland %>%
  st_cast("POLYGON") %>%         
  mutate(area = st_area(.)) %>%   
  filter(area > units::set_units(8000000, m^2)) %>%  
  summarise(geometry = st_union(geometry)) %>%
  st_cast("MULTIPOLYGON")
  
  
plot_karte <- ggplot() +
  with_outer_glow(
    geom_sf(data = geo_deutschland_clean, linewidth = 1.5, color = "gray15"),
    colour = "#3B7DC2", sigma = 8, expand = 5
  ) +
  geom_sf(data = geo_bundesländer, fill  = "gray92") +
  geom_sf(data = geo_städte_analyse, size = 4,
          shape = 21, fill = "gray33") +
  geom_sf(data = geo_städte_analyse %>% arrange(anteil) %>% tail(10),
          size = 6, shape = 21, fill = "skyblue") +
  geom_label_repel(data = geo_städte_analyse %>% arrange(anteil) %>% tail(10),
                   aes(label = glue("{stadt} / {round(anteil,1)}%"),
                       geometry = geometry),
                   family = "domine",
                   stat = "sf_coordinates",
                   nudge_y = 0.075,
                   alpha = 0.8,
                   size = 2.75,
                   seed = 66,
                   box.padding = 0.25,
                   min.segment.length = 0) +
  labs(title = "Top 10 <b><span style='color:gray90'>Städte</span></b> mit dem größten<br>Anteil an WGs im <b><span style='color:gray90'>Plattenbau</span></b>",
       caption = "Abbildung: Fabian Hellmold/Datengeschichten") +
  theme_dunkel() +
  theme(
    plot.margin = margin(t=10, l=20, b=5, r=20),
    plot.title.position = "plot",
    plot.title = element_markdown(
      color = plot_title_color,
      family = plot_title_family,
      hjust = 0.5,
      lineheight = 1.25
    ),
    plot.caption = element_text(
      color = plot_caption_color,
      family = plot_caption_family,
      face = "italic",
      size = 8,
      margin = margin(r=-10, t=5)
    ),
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
  )



## Abbildung speichern ---------------------------------------------------------

file_save_lokal <- glue("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_platte_karte_insta.png")
ggsave(filename = file_save_lokal, plot = plot_karte, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))

## Abbildung in Dropbox speichern ----------------------------------------------

file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_platte_karte_insta.png"

ggsave(filename = file_save_dropbox, plot = plot_karte,
       width = 4, height = 5, units = "in", dpi = 300)

