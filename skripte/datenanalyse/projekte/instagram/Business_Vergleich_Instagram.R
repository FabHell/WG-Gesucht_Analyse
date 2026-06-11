


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##                INSTAGRAM - Business-WG               ##         ##
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
  SELECT stadt, gesamtmiete, wg_art
  FROM analysedaten
  WHERE land = 'Deutschland'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
", .con = con_lokal)


Daten_Staedte <- dbGetQuery(con_lokal, sql) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.01, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.99, na.rm = TRUE)) %>%
  select(-gesamtmiete)



## Daten aufbereiten -----------------------------------------------------------


Daten_Staedte_aufb <- Daten_Staedte %>%
  na.omit() %>%
  mutate(business = str_detect(wg_art, "Business-WG")) %>%
  group_by(stadt, business) %>%
  summarise(anzahl = n(), .groups = "drop") %>%
  group_by(stadt) %>%
  mutate(anteil = anzahl / sum(anzahl) * 100) %>%
  filter(business == TRUE) %>%
  arrange(desc(anteil)) %>%
  ungroup()

Daten_Staedte_aufb_2 <- bind_rows(
  slice_head(Daten_Staedte_aufb, n = 3),
  tibble(stadt = "spacer", business = NA, anzahl = NA_integer_, anteil = NA_real_),
  slice_tail(Daten_Staedte_aufb, n = 1)
  ) %>%
  mutate(stadt = str_replace(stadt, "Frankfurt am Main", "Frankfurt a. M."),
         stadt = factor(stadt, levels = c("Göttingen", "spacer", "München",
                                          "Düsseldorf", "Frankfurt a. M.")),
         label_1 = case_when(stadt == "Göttingen" ~ NA_character_,
                             stadt == "spacer" ~ NA_character_,
                             TRUE ~ as.character(stadt)),
         label_2 = case_when(stadt == "spacer" ~ NA_character_,
                             stadt == "Göttingen" ~ glue("<span style='color:gray75'>{stadt}</span> <span style='color:gray90'>({round(anteil,1)}%)</span>"),
                             TRUE ~ glue("{round(anteil,1)}%")))



## Abbildung erstellen ---------------------------------------------------------


Plot_business <- Daten_Staedte_aufb_2 %>%
  ggplot(aes(x = anteil, y = stadt)) +
  geom_col(fill = "#FFD700", color = "gray60",
           width = 0.75, linewidth = 0.25,
           na.rm = TRUE) +
  geom_richtext(aes(label = label_1, x = 0.5, y = stadt),
                inherit.aes = F, hjust = 0, vjust = 0.55, size = 3, 
                fontface = "bold", family = "domine", color = "gray50",
                fill = "#FFD700", label.colour = "#FFD700", 
                label.padding = unit(c(0.3, 0.3, 0.3, 0.3), "lines")) +
  geom_richtext(aes(label = label_2, x = anteil + 0.5),  vjust = 0.55,
                hjust = 0, size = 3, fontface = "bold", family = "domine",
                color = "gray90", fill = panel_background_color,
                label.colour = panel_background_color, 
                label.padding = unit(c(0.3, 0.3, 0.3, 0.3), "lines")) +
  geom_richtext(aes(label = glue("{nrow(Daten_Staedte_aufb)-4} weitere Städte"), 
                    y = "spacer", x = 0.25), 
                hjust = 0, family = "franklin", color = "gray30", size = 3,
                fontface = "italic",  fill = panel_background_color,
                label.colour = panel_background_color, 
                label.padding = unit(c(0.3, 0.3, 0.3, 0.3), "lines")) +
  annotate("segment",
           x = -0.5, xend = -0.5,
           y = c(0.4, 2.3), yend = c(1.7, 5.75),
           color = axis_line_color) +
  annotate("segment",
           x = -0.5, xend = 14,
           y = 0.4, yend = 0.4,
           color = axis_line_color) +
  annotate("segment",
           x = -0.8, xend = -0.2,
           y = c(1.67,2.27), yend = c(1.73,2.33),
           color = axis_line_color) +
  annotate("point",
           x = -0.5, y = c(1.85,2,2.15), 
           size = 0.33, color = axis_line_color) +
  scale_x_continuous(limits = c(-1, 14),
                     expand = c(0, 0)) +
  scale_y_discrete(labels = c("Göttingen"     = "Schlusslicht",
                              "spacer"        = "",
                              "München"       = "Dritter Platz",
                              "Düsseldorf"    = "Zweitplatziert",
                              "Frankfurt a. M." = "Spitzenreiter")) +
  labs(title = 'In welchen Städten <b><span style="color:#FFD700">"Business-WGs"</span></b><br>besonders verbreitet sind',
       x = "Anteil (%)",
       y = NULL) +
  coord_cartesian(clip = "off") +
  theme_dunkel(gridline_x = TRUE, gridline_y = FALSE) +
  theme(
    plot.margin = margin(t=10, l=20, b=20, r=20),
    plot.title = element_markdown(margin = margin(b=30),
                                  size = 15),
    axis.line = element_blank(),
    axis.text.y = element_text(margin = margin(r=2.5)),
    axis.title.y = element_text(color = axis_title_color, family = axis_title_family,
                                size = 13, margin = margin(r=10)),
    axis.text.x = element_text(color = axis_text_color, family = axis_text_family,
                               margin = margin(t=7.5)),
    axis.title.x = element_text(color = axis_title_color, family = axis_title_family,
                                margin = margin(t=7.5))
  )




## Abbildung speichern ---------------------------------------------------------


file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_business_vergleich.png"
ggsave(filename = file_save_lokal, plot = Plot_business, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))



## Abbildung in Dropbox speichern ----------------------------------------------

file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_business_vergleich.png"

ggsave(filename = file_save_dropbox, plot = Plot_business, 
       width = 4, height = 5, units = "in", dpi = 300)

