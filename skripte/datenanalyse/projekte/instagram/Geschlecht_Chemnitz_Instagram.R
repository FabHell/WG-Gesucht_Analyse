


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##                  INSTAGRAM - Geschlecht              ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


library(tidyverse)
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
  SELECT stadt, geschlecht_ges
  FROM analysedaten
  WHERE land = 'Deutschland'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
", .con = con_lokal)


Daten_Staedte <- dbGetQuery(con_lokal, sql)



## Daten aufbereiten -----------------------------------------------------------


Daten_Staedte_aufb <- Daten_Staedte %>%
  group_by(stadt, geschlecht_ges) %>%
  summarise(anzahl = n(), .groups = "drop") %>%
  na.omit() %>%
  group_by(stadt) %>%
  mutate(anteil = anzahl / sum(anzahl)*100,
         geschlecht_ges = factor(geschlecht_ges, 
                                 levels = c("Divers", "Mann", 
                                            "Frau", "Geschlecht egal"
                                 ))) 

Daten_Staedte_aufb %>%
  filter(geschlecht_ges == "Frau") %>%
  arrange(desc(anteil)) %>% 
  head(1)

Daten_Staedte_aufb_mean <- Daten_Staedte_aufb %>%
  group_by(geschlecht_ges) %>%
  summarise(anteil_mean = median(anteil))




## Abbildung erstellen ---------------------------------------------------------

Daten_Staedte_aufb_stadt <- Daten_Staedte_aufb %>%
  filter(stadt == "Chemnitz") %>%
  complete(geschlecht_ges, fill = list(anzahl = 0, anteil = 0))
  

Daten_Staedte_geschlecht_text <- Daten_Staedte_aufb_stadt %>%
  mutate(x_pos = ifelse(geschlecht_ges %in% c("Frau", "Mann", "Divers"), 
                        anteil/2+4, 2.5),
         label_1 = ifelse(geschlecht_ges %in% c("Frau", "Mann", "Divers"), 
                          "", glue("{geschlecht_ges} ({round(anteil,1)}%)")),
         label_2 = ifelse(geschlecht_ges %in% c("Frau", "Mann", "Divers"), 
                          glue("{geschlecht_ges} ({round(anteil,1)}%)"), ""))


Plot_geschlecht <- Daten_Staedte_aufb_stadt %>%  
  ggplot(aes(y = geschlecht_ges, x = anteil)) +
  geom_crossbar(data = Daten_Staedte_aufb_mean,
                aes(x = anteil_mean, xmin = anteil_mean, xmax = anteil_mean,
                    y = geschlecht_ges),
                inherit.aes = F,
                width = 0.95,
                color = "gray65", linewidth = 0.25) +
  geom_col(fill = "orange", color = "gray35", linewidth = 0.25, width = 0.75) +
  geom_text(
    data = Daten_Staedte_geschlecht_text,
    aes(y = geschlecht_ges, x = x_pos, label = label_1), inherit.aes = F,
    hjust = 0, family = "domine", color = "gray25") +
  geom_richtext(
    data = Daten_Staedte_geschlecht_text,
    aes(y = geschlecht_ges, x = anteil + 2, label = label_2), inherit.aes = F,
    hjust = 0, family = "domine", color = "gray75", fill = panel_background_color,
    label.colour = panel_background_color, 
    label.padding = unit(c(0.3, 0.3, 0.3, 0.3), "lines")) +
  annotate("text",
           x = 25.25, y = Inf,
           label = "deutschlandweites Mittel",
           hjust = 0, vjust = 0.2, size = 3,
           family = "franklin", color = "gray95",
           fontface = "italic") +
  annotate("curve",
           x = 69.5, y = nlevels(Daten_Staedte_aufb_stadt$geschlecht_ges)+0.8,
           xend = 75, yend = nlevels(Daten_Staedte_aufb_stadt$geschlecht_ges) + 0.58,
           arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
           curvature = -0.15, color = "gray95", linewidth = 0.33) +
  coord_cartesian(clip = "off") +
  labs(x = "Anteil in %", y = "gewünschtes Geschlecht",
       title = "Geschlecht egal? In <b><span style='color:orange'>Chemnitz</span></b> die Regel.") +
  theme_dunkel(gridline_x = TRUE, gridline_y = FALSE) +
  theme(
    plot.margin = margin(t=10, l=20, b=20, r=20),
    plot.title = element_markdown(margin = margin(b=30),
                                  size = 15),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(color = axis_title_color, family = axis_title_family,
                                size = 13, margin = margin(r=10)),
    axis.text.x = element_text(color = axis_text_color, family = axis_text_family,
                               margin = margin(t=7.5)),
    axis.title.x = element_text(color = axis_title_color, family = axis_title_family,
                                margin = margin(t=7.5))
  )




## Abbildung lokal speichern ---------------------------------------------------

file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_geschlecht_Chemnitz.png"
ggsave(filename = file_save_lokal, plot = Plot_geschlecht, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))


## Abbildung in Dropbox speichern ----------------------------------------------

file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_geschlecht_Chemnitz.png"

ggsave(filename = file_save_dropbox, plot = Plot_geschlecht, 
       width = 4, height = 5, units = "in", dpi = 300)

