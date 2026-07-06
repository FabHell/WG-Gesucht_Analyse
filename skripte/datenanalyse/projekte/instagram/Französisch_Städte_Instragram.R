


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##           INSTAGRAM - Sprachen Französisch           ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")


library(tidyverse)
library(glue)
library(DBI)
library(ggfx)


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


daten_staedte_roh <- dbGetQuery(con_lokal, sql)



## Daten aufbereiten -----------------------------------------------------------


daten_aufb <-  daten_staedte_roh %>%
  na.omit() %>%
  group_by(stadt) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-gesamtmiete) %>%
  mutate(sprache_filter = ifelse(str_detect(sprache, "Französisch"), "französisch", "kein_französisch")) %>%
  group_by(stadt, sprache_filter) %>%
  summarise(anzahl = n()) %>%
  group_by(stadt) %>%
  mutate(anteil = anzahl / sum(anzahl)*100) %>%
  filter(sprache_filter == "französisch")



plot_französisch <- daten_aufb %>%
  arrange(desc(anteil)) %>% head(5) %>%
  mutate(stadt = str_replace(stadt, "Freiburg im Breisgau", "Freiburg i. B.")) %>%
  
  ggplot(aes(x = anteil, y = reorder(stadt, anteil))) +
  geom_richtext(aes(label = paste0(round(anteil, 1), "%"),
                    x = anteil-0.9),
                hjust = 1, vjust = -0.075, size = 3, color = "gray70", family = "domine",
                label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
                label.colour  = "transparent", fill = "transparent") +
  geom_richtext(aes(label = stadt), x = 0.33,
                hjust = 0, vjust = -0.1, size = 3, color = "gray50", family = "domine",
                label.padding = unit(c(0.25, 0.05, 0.05, 0.05), "lines"),
                label.colour  = panel_background_color, fill = panel_background_color) +
  geom_segment(aes(x = 0, xend = anteil, y = stadt, yend = stadt),
               color = "gray35", linewidth = 0.75) +
  with_outer_glow(
    geom_point(size = 4.5, color = "#3B7DC2"),
    colour = "#3B7DC2", sigma = 8, expand = 5
  ) +
  geom_point(size = 1.25, color = "gray35", fill = "gray35",
             x = 0, shape = 23) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)),
                     breaks = c(5,15,25),
                     labels = ~paste0(., "%")) +
  coord_cartesian(xlim = c(0, max(daten_aufb$anteil) + 1),
                  clip = "off") +
  labs(title = "In welchen <b><span style='color:gray90'>Städten</span></b> findet man am ehesten<br>eine <b><span style='color:#3B7DC2'>französischsprachige WG</span></b>?",
       x = "Anteil französischsprachiger WGs", y = "Top 5 Städte",
       caption = "Abbildung: Fabian Hellmold/Datengeschichten") +
  theme_dunkel(gridline_y = FALSE) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(margin = margin(r=15), face = "bold"),
        axis.title.x = element_text(size = 9.5),
        plot.title = element_markdown(margin = margin(b=15)))



## Abbildung lokal speichern ---------------------------------------------------

file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_französisch_städte.png"
ggsave(filename = file_save_lokal, plot = plot_französisch, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))


## Abbildung in Dropbox speichern ----------------------------------------------

file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_französisch_städte.png"

ggsave(filename = file_save_dropbox, plot = plot_französisch,
       width = 4, height = 5, units = "in", dpi = 300)
