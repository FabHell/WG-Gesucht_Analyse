

source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")


datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")


## Daten laden -----------------------------------------------------------------


sql_wgs <- glue_sql("
  SELECT stadt, geschlecht_ges
  FROM analysedaten
  WHERE land = 'Deutschland'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
", .con = con_lokal)

Daten_WGs <- dbGetQuery(con_lokal, sql_wgs)


sql_studis <- "
 SELECT stadt, studierende_maennlich_perc
 FROM kontextdaten
"

Daten_Studizahlen <- dbGetQuery(con_lokal, sql_studis)




## Daten aufbereiten -----------------------------------------------------------


Daten_WGs_aufb <- Daten_WGs %>%
  group_by(stadt, geschlecht_ges) %>%
  summarise(anzahl = n(), .groups = "drop") %>%
  na.omit() %>%
  group_by(stadt) %>%
  mutate(anteil = anzahl / sum(anzahl)*100) %>%
  filter(geschlecht_ges == "Frau") %>%
  select(stadt, anteil_frauen_wg = anteil)

Daten_Studizahlen_aufb <- Daten_Studizahlen %>%
  mutate(studierende_weiblich_perc = 100 - studierende_maennlich_perc) %>%
  select(stadt, anteil_frauen_studis = studierende_weiblich_perc)

Daten_ges <- Daten_WGs_aufb %>%
  left_join(Daten_Studizahlen_aufb, by = "stadt")


corr_pearson <- cor(Daten_ges$anteil_frauen_wg, Daten_ges$anteil_frauen_studis, method = "pearson")



Plot_geschlecht_frauen <- Daten_ges %>%
  ggplot(aes(x = anteil_frauen_wg, y = anteil_frauen_studis)) +
  geom_point(shape = 21, size = 2.5,
             fill = "darkred", color = "darkgray") +
  geom_richtext(aes(label = glue("Korrelation: **{round(corr_pearson,2)}**"),
                    x=25, y= 40), family = "franklin", fill = "#f8f5f2",
                inherit.aes = F, hjust = 0.5, size = 3.5,
                label.padding = unit(c(0.3,0.25,0.25,0.3), "lines")) +
  annotate("text",
           x = 18.8, y = 64.5,
           label = "Greifswald",
           hjust = 0, vjust = 0, size = 2.5,
           family = "franklin", color = "gray90",
           fontface = "italic") +
  annotate("curve",
           x = 18.5, y = 64.75,
           xend = 17.25, yend = 63.65,
           arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
           curvature = 0.15, color = "gray90", linewidth = 0.33) +
  annotate("text",
           x = 13.0, y = 33.65,
           label = "Aachen",
           hjust = 0, vjust = 0, size = 2.5,
           family = "franklin", color = "gray90",
           fontface = "italic") +
  annotate("curve",
           x = 12.75, y = 34.05,
           xend = 11.7, yend = 34.6,
           arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
           curvature = -0.1, color = "gray90", linewidth = 0.33) +
  labs(title = "Blablabla. Diese Überschrift geht über zwei<br>Zeilen",
       caption = "Abbildung: Fabian Hellmold/Datengeschichten",
       x = "gewünschtes Geschlecht: Frau",
       y = "Frauenanteil unter Studierenden") +
  scale_x_continuous(
    labels = ~paste0(.x, "%")
  ) +
  scale_y_continuous(
    labels = ~paste0(.x, "%")
  ) +
  coord_cartesian(clip = "off") +
  theme_dunkel() +
  theme(plot.title = element_markdown(margin = margin(b=20)),
        plot.caption = element_text(margin = margin(t=15, r=-2.5),
                                    size = 7),
        axis.title = element_markdown(size = 10))



## Abbildung lokal speichern ---------------------------------------------------

file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_geschlecht_frauen_vergleich.png"
ggsave(filename = file_save_lokal, plot = Plot_geschlecht_frauen, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))


## Abbildung in Dropbox speichern ----------------------------------------------

file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_geschlecht_frauen_vergleich.png"

ggsave(filename = file_save_dropbox, plot = Plot_geschlecht_frauen, 
       width = 4, height = 5, units = "in", dpi = 300)
