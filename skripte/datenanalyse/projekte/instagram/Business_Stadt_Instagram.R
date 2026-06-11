


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##                INSTAGRAM - Business-WG               ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

library(tidyverse)
library(glue)
library(DBI)
library(ggbeeswarm)

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
  SELECT stadt, gesamtmiete, wg_art
  FROM analysedaten
  WHERE stadt = 'Göttingen'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
", .con = con_lokal)


Daten_Staedte_roh <- dbGetQuery(con_lokal, sql)



## Daten aufbereiten -----------------------------------------------------------


Daten_Staedte_aufb <- Daten_Staedte_roh %>%
  na.omit() %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  mutate(business = str_detect(wg_art, "Business-WG")) 

Mittelwerte <- Daten_Staedte_aufb %>%
  summarise(mean_miete = mean(gesamtmiete)) %>%
  mutate(label = "\u00D8", y = mean_miete)

Mittelwerte_labels_1 <- Daten_Staedte_aufb %>%
  mutate(ueber_unter_mean = if_else(gesamtmiete > mean(gesamtmiete),
                                    F, T)) %>%
  filter(business == TRUE) %>%
  group_by(ueber_unter_mean) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = if_else(ueber_unter_mean == F, 
                         paste0('<span style="color:#FFD700"><b>',n,'</b> WGs</span>'),
                         paste0('<span style="color:#FFD700"><b>',n,'</b> WG</span>'))) %>%
  pull(label) %>% 
  paste(collapse = "<br>")

Mittelwerte_labels_2 <- Daten_Staedte_aufb %>%
  mutate(ueber_unter_mean = if_else(gesamtmiete > mean(gesamtmiete),
                                    F, T)) %>%
  filter(business == TRUE) %>%
  group_by(ueber_unter_mean) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = if_else(ueber_unter_mean == F, 
                         paste0('&gt; \u00D8'),
                         paste0('&lt; \u00D8'))) %>%
  pull(label) %>% 
  paste(collapse = "<br>")


Plot_business_goe <- ggplot(Daten_Staedte_aufb, 
                            aes(x = "", y = gesamtmiete, colour = business,
                                alpha = business)) +
  geom_quasirandom(data = Daten_Staedte_aufb %>% filter(business == FALSE),
                   cex = 1.5, method = "quasirandom",
                   show.legend = FALSE) +
  geom_quasirandom(data = Daten_Staedte_aufb %>% filter(business == TRUE),
                   cex = 1.5, method = "minout", width = 0.2,
                   show.legend = FALSE) +
  geom_richtext(label = Mittelwerte_labels_1, y = Mittelwerte$mean_miete, x = 1.41,
                lineheight = 2, hjust = 0, label.padding = unit(0.1, "lines"),
                color = "gray70", size = 2.5, inherit.aes = F, family = "domine",
                fill = panel_background_color, vjust = 0.535,
                label.colour = panel_background_color) +
  geom_richtext(label = Mittelwerte_labels_2, y = Mittelwerte$mean_miete, x = 1.61,
                lineheight = 2, hjust = 1, label.padding = unit(0.1, "lines"),
                color = "gray70", size = 2.5, inherit.aes = F, family = "domine",
                fill = panel_background_color, vjust = 0.535,
                label.colour = panel_background_color) +
  geom_hline(data = Mittelwerte, 
             aes(yintercept = mean_miete), 
             linetype = "22", color = "gray60") +
  geom_richtext(data = Mittelwerte, aes(label=label, y=y), x = 0.463,
                label.padding = unit(0.1, "lines"), vjust = 0.54,
                color = "gray70", size = 2.25, inherit.aes = F, family = "domine",
                fill = panel_background_color, label.colour = panel_background_color) +
  scale_color_manual(
    values = c("TRUE" = "#FFD700", "FALSE" = "#4D4D4D")) + 
  scale_alpha_manual(
    values = c("TRUE" = 0.8, "FALSE" = 0.4)) + 
  scale_y_continuous(labels = scales::label_dollar(prefix = "", suffix = " €")) +
  labs(x = NULL,
       y = 'Miete (einzelnes Zimmer)', 
       title = 'In <b><span style="color:gray90">Göttingen</span></b> zahlt man für <b><span style="color:#FFD700">"Business-WGs"</span></b><br>in der Regel überdurchschnittlich') +
  theme_dunkel(gridline_x = F) +
  theme(axis.text.y = element_text(size=8, margin = margin(r=5)),
        axis.title.y = element_markdown(margin = margin(r=10)),
        plot.title = element_markdown(margin = margin(b=20),
                                      size = 14))



## Abbildung speichern ---------------------------------------------------------

file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_business_goettingen.png"
ggsave(filename = file_save_lokal, plot = Plot_business_goe, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))



## Abbildung in Dropbox speichern ----------------------------------------------

file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_business_goettingen.png"

ggsave(filename = file_save_dropbox, plot = Plot_business_goe, 
       width = 4, height = 5, units = "in", dpi = 300)

