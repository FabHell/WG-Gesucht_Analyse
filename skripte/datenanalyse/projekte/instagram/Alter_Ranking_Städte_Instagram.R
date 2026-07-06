



################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##            INSTAGRAM - Alter Städteranking           ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


library(tidyverse)
library(ggrepel)
library(DBI)
library(glue)

source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")


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
  SELECT stadt, alter_ges, gesamtmiete
  FROM analysedaten
   WHERE land = 'Deutschland'
     AND datum_scraping >= {datum_von}
     AND datum_scraping <= {datum_bis}
     AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
 ", .con = con_lokal)

daten_staedte_roh <- dbGetQuery(con_lokal, sql) %>%
  filter(!is.na(gesamtmiete)) %>%
  group_by(stadt) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  ungroup() %>% select(-gesamtmiete)



## Daten aufbereiten -----------------------------------------------------------


daten_staedte <- daten_staedte_roh %>%
  mutate(alter_ges = ifelse(is.na(alter_ges), "16 und 99", alter_ges)) %>%
  mutate(alter_seq = map(alter_ges, ~ {
    teile <- str_extract_all(.x, "\\d+")[[1]] %>% as.integer()
    seq(teile[1], teile[2])
  }))


gesamt_staedte <- daten_staedte %>%
  count(stadt, name = "gesamt")

daten_anteil <- daten_staedte %>%
  unnest(alter_seq) %>%
  count(stadt, alter_seq, name = "haeufigkeit") %>%
  left_join(gesamt_staedte, by = "stadt") %>%
  filter(alter_seq >= 18 & alter_seq <= 40) %>%
  mutate(anteil = haeufigkeit / gesamt * 100) %>%
  filter(alter_seq == 19) %>%
  mutate(stadt = str_replace(stadt, "Freiburg im Breisgau", "Freiburg i. B."),
         label = glue("{stadt} / {round(anteil,1)}%"))



## Abbildung erstellen ---------------------------------------------------------


tick_breite <- 0.3  

plot_alter_rang <- ggplot() +
  geom_vline(xintercept = 0, color = panel_grid_color) +
  geom_segment(data = daten_anteil,
               aes(x = -tick_breite, xend = tick_breite, y = anteil, yend = anteil),
               color = "gray60", linewidth = 0.3) +
  geom_segment(data = daten_anteil %>% slice_max(anteil, n = 5, with_ties = FALSE),
               aes(x = -tick_breite, xend = tick_breite, y = anteil, yend = anteil),
               color = "#4C9F70", linewidth = 0.9) +
  geom_segment(data = daten_anteil %>% slice_min(anteil, n = 5, with_ties = FALSE),
               aes(x = -tick_breite, xend = tick_breite, y = anteil, yend = anteil),
               color = "#C2543B", linewidth = 0.9) +
  geom_text_repel(data = daten_anteil %>% slice_max(anteil, n = 5, with_ties = FALSE),
                  aes(x = tick_breite, y = anteil, label = label),
                  hjust = 0, direction = "y",
                  segment.color = "#4C9F70", segment.size = 0.3,
                  min.segment.length = 0,
                  xlim = c(tick_breite + 0.15, NA),
                  size = 3.2, color = "#4C9F70",
                  family = "domine") +
  geom_text_repel(data = daten_anteil %>% slice_min(anteil, n = 5, with_ties = FALSE),
                  aes(x = -tick_breite, y = anteil, label = label),
                  hjust = 1, direction = "y",
                  segment.color = "#C2543B", segment.size = 0.3,
                  min.segment.length = 0,
                  xlim = c(NA, -tick_breite - 0.15),
                  size = 3.2, color = "#C2543B",
                  family = "domine") +
  annotate("text",
           x = -3, 
           y = c(41.2,61.2,81.2),
           label = c("40%","60%","80%"),
           color = axis_title_color, family = axis_title_family,
           hjust = 0.75, size = 2.5) +
  scale_y_continuous(limits = c(40, 90), breaks = seq(40, 80, 20)) +
  scale_x_continuous(limits = c(-3, 3)) +
  labs(
    y = "Anteil der WGs, die 18-Jährige aufnehmen",
    caption = "Abbildung: Fabian Hellmold/Datengeschichten",
    title = "Nicht überall willkommen: Wie offen<br><b><span style='color:gray90'>Städte</span></b> für <b><span style='color:gray90'>Studiumsanfänger</span></b> sind",
  ) +
  theme_dunkel() +
  theme(axis.text.x  = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r=15),
                                    size = 10),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(margin = margin(b=15)))



## Abbildung speichern ---------------------------------------------------------


file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_alter_rang.png"
ggsave(filename = file_save_lokal, plot = plot_alter_rang, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))


file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_alter_rang.png"

ggsave(filename = file_save_dropbox, plot = plot_alter_rang,
      width = 4, height = 5, units = "in", dpi = 300)
