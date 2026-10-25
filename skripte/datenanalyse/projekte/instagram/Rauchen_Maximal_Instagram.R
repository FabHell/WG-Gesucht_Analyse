


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##               INSTAGRAM - Rauchen Maxima             ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


library(tidyverse)
library(glue)
library(DBI)
library(colorspace)

source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")


## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("DATABASE_PG"),
                       host = Sys.getenv("SERVER_PG"),
                       port = Sys.getenv("PORT_PG"),
                       user = Sys.getenv("UID_PG"),
                       password = Sys.getenv("PWD_PG"))

sql <- glue_sql("
  SELECT stadt, gesamtmiete, rauchen
  FROM analysedaten
  WHERE stadtteil_geocoding IS NOT NULL
    AND gesamtmiete IS NOT NULL
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND land = 'Deutschland'
", .con = con_lokal)


Daten_Staedte_roh <- dbGetQuery(con_lokal, sql) %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.01),
    quantile(gesamtmiete, 0.99)
  )) %>%
  select(-gesamtmiete) %>%
  na.omit()


farben_kategorien <- c(
  "Rauchen nicht erwünscht" = "#E63946",
  "Rauchen nur auf Balkon" = "#F4A261",
  "Rauchen im Zimmer erlaubt" = "#E9C46A",
  "Rauchen überall erlaubt" = "#4C956C"
)

Daten_Staedte <- Daten_Staedte_roh %>%
  count(stadt, rauchen, name = "N") %>%
  group_by(stadt) %>%
  mutate(anteil = N / sum(N) * 100,
         rauchen = str_replace(rauchen, "Rauchen auf dem Balkon erlaubt", "Rauchen nur auf Balkon")) %>%
  select(-N) %>%
  ungroup() %>%
  mutate(
    rauchen_kategorie = factor(rauchen, levels = c(
      "Rauchen nicht erwünscht", "Rauchen nur auf Balkon", 
      "Rauchen im Zimmer erlaubt", "Rauchen überall erlaubt")),
    color = farben_kategorien[as.character(rauchen_kategorie)]
  )


stadt_filter <- subset(Daten_Staedte, rauchen_kategorie == "Rauchen nicht erwünscht") %>%
  slice_max(anteil, n = 3) %>%
  pull(stadt)

Daten_Staedte_ana <- Daten_Staedte %>%
  filter(stadt %in% stadt_filter)

daten_ges <- Daten_Staedte_ana %>%
  mutate(stadt_reinfolge = factor(stadt, levels = stadt_filter)) %>%
  mutate(stadt_position = as.numeric(factor(stadt_reinfolge)))

daten_mean <- Daten_Staedte %>%
  group_by(rauchen_kategorie) %>%
  summarise(anteil = mean(anteil)) %>%
  mutate(color = case_when(
    rauchen_kategorie == "Rauchen nicht erwünscht" ~ "#E63946",
    rauchen_kategorie == "Rauchen nur auf Balkon" ~ "#F4A261",
    rauchen_kategorie == "Rauchen im Zimmer erlaubt" ~ "#E9C46A",
    rauchen_kategorie == "Rauchen überall erlaubt" ~ "#2A9D8F",
    TRUE ~ "gray"
  )) %>%
  arrange(desc(anteil))


Plot_rauchen_min <- daten_ges %>%
  ggplot() +
  geom_segment(
    data = daten_mean,
    aes(x = 0.75, xend = 3.67,
        y = anteil, yend = anteil, color = color), alpha = 0.5, linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_segment(
    data = daten_mean,
    aes(x = 0.74, xend = 0.74,
        y = anteil-0.75, yend = anteil+0.75, color = color), alpha = 0.5, linewidth = 0.5
  ) +
  geom_point(
    data = daten_mean,
    aes(y = anteil, fill = color),
    x = 3.75, shape = 23, color = "gray15", size = 2.75, alpha = 0.5,
    show.legend = TRUE
  ) +
  geom_point(
    aes(x = stadt_position, y = anteil, fill = color),
    shape = 21, color = "gray15", size = 4,
    show.legend = FALSE, alpha = 0.8
  ) +
  annotate("segment",
           x = 3.75, xend = 3.75,
           y = as.numeric(daten_mean[1,2])+3,
           yend = as.numeric(daten_mean[1,2])+5,
           alpha = 0.25, color = "gray60") +
  annotate("segment",
           x = 3.75, xend = 3.75,
           y = as.numeric(daten_mean[1,2])-3,
           yend = as.numeric(daten_mean[2,2])+3,
           alpha = 0.25, color = "gray60") +
  annotate("segment",
           x = 3.75, xend = 3.75,
           y = as.numeric(daten_mean[2,2])-3,
           yend = as.numeric(daten_mean[3,2])+3,
           alpha = 0.25, color = "gray60") +
  annotate("text",
           x = 3.75, y = as.numeric(daten_mean[1,2])+6.5,
           label = "Deutschlandweiter\nDurchschnitt", alpha = 0.5,
           lineheight = 1, size = 2.5, vjust = 0, color = "gray66") +
  scale_fill_identity(
    guide = guide_legend(ncol = 2,
                         override.aes = list(shape = 21, alpha = 0.8)),
    breaks = daten_mean$color,
    labels = str_replace(daten_mean$rauchen_kategorie, "Rauchen ", ""),
    name = "Rauchen ..."
  ) +
  scale_color_identity() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.15)),
                     breaks = seq_along(stadt_filter),
                     labels = stadt_filter) +
  labs(
    title = 'In welchen Städten es <b><span style="color:gray90">Raucher</span></b> bei der<br>WG-Suche <b><span style="color:gray90">am schwersten</span></b> haben',
    y = "Anteil in %",
    x = NULL,
    caption = "Abbildung: Fabian Hellmold/Datengeschichten"
  ) +  coord_cartesian(clip = "off") +
  theme_dunkel() +
  theme(legend.position = "bottom",
        plot.title = element_markdown(margin = margin(b=20)),
        axis.title.y = element_text(size = 10.5),
        axis.text.y = element_text(size = 8.5),
        axis.text.x = element_text(color = axis_title_color),
        legend.title = element_text(color = axis_title_color, family = "domine",
                                    size = 8.5),
        legend.text = element_markdown(size = 7.5, family = "franklin",
                                       margin = margin(r=0, l=0)),
        legend.background = element_rect(fill = lighten(panel_background_color, amount = 0.025)),
        legend.key = element_rect(fill = lighten(panel_background_color, amount = 0.025),
                                  color = NA),
        legend.key.spacing.y = unit(-2.5, "pt"),
        legend.key.spacing.x = unit(0, "pt")
  )


## Abbildung speichern ---------------------------------------------------------

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/instagram/Rauchen_max.png"
file_dropbox <- "/home/fabian/Dropbox/Abbildungen_Instagram/Abbildungen_Instagram_Fabi/Rauchen_max.png"

ggsave(filename = file_save, plot = Plot_rauchen_min,
       width = 4, height = 5, units = "in", dpi = 300)

add_logo_insta(
  image_path = file_save,
  opacity = 0.25,
  logo_path = "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/daten/logos/DG_Logo_schwarzweiß.png"
)

system(paste("xdg-open", shQuote(file_save)))

file_copy(file_save, file_dropbox, overwrite = TRUE)


