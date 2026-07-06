


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##                  INSTAGRAM - Sprachen               ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

library(tidyverse)
library(glue)
library(DBI)


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
  SELECT bundesland, stadt, gesamtmiete, sprache
  FROM analysedaten
  WHERE land = 'Deutschland'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
", .con = con_lokal)


Daten_Staedte_roh <- dbGetQuery(con_lokal, sql)



## Daten aufbereiten -----------------------------------------------------------
  

Daten_aufb_Flensb <- Daten_Staedte_roh %>%
  na.omit() %>%
  group_by(stadt) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-gesamtmiete) %>%
  mutate(sprache_filter = ifelse(str_detect(sprache, "Dänisch"), "dänisch", "kein_dänisch"),
         stadt_filter = ifelse(stadt == "Flensburg", "flensburg", "rest_flensb")) %>%
  group_by(stadt_filter, sprache_filter) %>%
  summarise(anzahl = n()) %>%
  group_by(stadt_filter) %>%
  mutate(anteil = anzahl / sum(anzahl)*100) %>%
  filter(sprache_filter == "dänisch")


Daten_aufb_Saarb <- Daten_Staedte_roh %>%
  na.omit() %>%
  group_by(stadt) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-gesamtmiete) %>%
  mutate(sprache_filter = ifelse(str_detect(sprache, "Französisch"), "französisch", "kein_französisch"),
         stadt_filter = ifelse(stadt == "Saarbrücken", "saarbrücken", "rest_saarb")) %>%
  group_by(stadt_filter, sprache_filter) %>%
  summarise(anzahl = n()) %>%
  group_by(stadt_filter) %>%
  mutate(anteil = anzahl / sum(anzahl)*100) %>%
  filter(sprache_filter == "französisch")


Daten_Sprache_ges <- rbind(Daten_aufb_Flensb, Daten_aufb_Saarb) %>%
  mutate(stadt_filter = factor(stadt_filter, levels = c(
    "rest_flensb", "flensburg",  "rest_saarb", "saarbrücken"
  ))) %>%
  mutate(label_pos_x = ifelse(anteil > 5, anteil - 0.75, anteil + 0.75),
         color = ifelse(anteil > 5, "gray5", "gray75"),
         hjust = ifelse(anteil > 5, 1,0))



Plot_sprachen_grenze <- Daten_Sprache_ges %>%
  ggplot(aes(x = anteil, y = stadt_filter, fill = sprache_filter)) +
  geom_col(show.legend = F, color = "gray25") +
  geom_text(aes(x = label_pos_x, label = glue("{round(anteil,1)}%"),
                color = color, hjust = hjust),
            family = "franklin", fontface = "italic", size = 2.75) +
  facet_wrap(~sprache_filter, ncol = 1, scales = "free_y",
             labeller = labeller(sprache_filter = c(
               "dänisch"     = "WGs mit <span style='color:#c8102f'>dänischsprachigen</span> Mitbewohnern",
               "französisch" = "WGs mit <span style='color:#002654'>französischsprachigen</span> Mitbewohnern"
             ))) +
  scale_color_identity() +
  scale_fill_manual(values = c(
    "dänisch"     = "#c8102f",
    "französisch" = "#002654"
  )) +
  scale_y_discrete(
    breaks = c("flensburg", "rest_flensb", "saarbrücken", "rest_saarb"),
    labels = c("Flensburg", "andere\nStädte", "Saarbrücken", "andere\nStädte")
  ) +
  labs(title = "Wie <b><span style='color:gray90'>Grenznähe</span></b> die <b><span style='color:gray90'>Sprache</span></b> in<br>Wohngemeinschaften prägt",
       caption = "Abbildung: Fabian Hellmold/Datengeschichten",
       x = "Anteil in %",
       y = NULL) +
  theme_dunkel(gridline_y = F) +
  theme(plot.title = element_markdown(margin = margin(b=20),
                                      size = 15),
        panel.spacing = unit(0.5, "cm"),
        strip.text = element_markdown(size = 8.25, hjust = 1),
        axis.line.y = element_blank(),
        axis.text.y = element_text(lineheight = 1.05),
        axis.text.x = element_text(size = 7.5),
        axis.title.x = element_text(size = 9))



## Abbildung lokal speichern ---------------------------------------------------

file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_sprache_grenzstadt.png"
ggsave(filename = file_save_lokal, plot = Plot_sprachen_grenze, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))


## Abbildung in Dropbox speichern ----------------------------------------------

file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_sprache_grenzstadt.png"

ggsave(filename = file_save_dropbox, plot = Plot_sprachen_grenze,
       width = 4, height = 5, units = "in", dpi = 300)

