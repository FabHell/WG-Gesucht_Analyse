
library(tidyverse)
library(waffle)
library(patchwork)
library(ggfx)
library(DBI)
library(glue)

filter_alter <- 18

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

primärfarbe <- "#3f74a6"

source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")


## Daten laden -----------------------------------------------------------------


con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql <- glue_sql("
  SELECT stadt, alter_ges, geschlecht_ges, gesamtmiete
  FROM analysedaten
  WHERE land = 'Deutschland'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
", .con = con_lokal)

daten_staedte_roh <- dbGetQuery(con_lokal, sql) %>%
  group_by(stadt) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-gesamtmiete) %>%
  ungroup()



## Daten aufbereiten -----------------------------------------------------------


daten_staedte <- daten_staedte_roh %>%
  mutate(alter_ges = ifelse(is.na(alter_ges), "16 und 99", alter_ges)) %>%
  mutate(alter_seq = map(alter_ges, ~ {
    teile <- str_extract_all(.x, "\\d+")[[1]] %>% as.integer()
    seq(teile[1], teile[2])
  }))


daten_waffle_w <- daten_staedte %>%
  mutate(gruppe_personen = ifelse(
    geschlecht_ges %in% c("Geschlecht egal", "Frau") & map_lgl(alter_seq, ~ filter_alter %in% .x), 
    "zielgruppe", "andere")) %>%
  group_by(gruppe_personen) %>%
  count() %>%
  ungroup() %>%
  mutate(n_scaled = round(n / sum(n) * 100) -100,
         n_scaled = n_scaled *(-1))


daten_waffle_m <- daten_staedte %>%
  mutate(gruppe_personen = ifelse(
    geschlecht_ges %in% c("Geschlecht egal", "Mann") & map_lgl(alter_seq, ~ filter_alter %in% .x), 
    "zielgruppe", "andere")) %>%
  group_by(gruppe_personen) %>%
  count() %>%
  ungroup() %>%
  mutate(n_scaled = round(n / sum(n) * 100) -100,
         n_scaled = n_scaled *(-1))



## Abbildungen erstellen -------------------------------------------------------

plot_waffle_w <- daten_waffle_w %>%
  ggplot(aes(fill = gruppe_personen, values = n_scaled)) +
  with_outer_glow(
    geom_waffle(n_rows = 5, size = 0.5, colour =  panel_background_color, flip = T,
                linewidth = 5),
    colour = "gray30", sigma = 8, expand = 2
  ) +
  geom_text(x=5.5, y = 21.3, label = glue("{daten_waffle_w[1,3]}%"),
            hjust = 1, family = "domine",
            color = primärfarbe, size = 4.75) +
  geom_text(x=0.5, y = 21.1, label = glue("Frau / 18 Jahre"),
            hjust = 0, size = 2.8,
            family = "domine", color = "gray85") +
  scale_fill_manual(
    values = c("zielgruppe" = "gray65", "andere" = primärfarbe)
  ) +
  coord_cartesian(expand = F, clip = "off") +
  labs(y = "Anteil der zugänglichen WG-Angebote") +
  theme_dunkel() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r=17.5),
                                    size = 9.5),
        plot.margin = margin(t=10,b=5,r=12.5,l=20)
  )



plot_waffle_m <- daten_waffle_m %>%
  ggplot(aes(fill = gruppe_personen, values = n_scaled)) +
  with_outer_glow(
    geom_waffle(n_rows = 5, size = 0.5, colour =  panel_background_color, flip = T,
                linewidth = 5),
    colour = "gray30", sigma = 8, expand = 2
  ) +
  geom_text(x=5.5, y = 21.3, label = glue("{daten_waffle_m[1,3]}%"),
            hjust = 1, family = "domine",
            color = primärfarbe, size = 4.75) +
  geom_text(x=0.5, y = 21.1, label = glue("Mann / 18 Jahre"),
            hjust = 0, size = 2.8,
            family = "domine", color = "gray85") +
  scale_fill_manual(
    values = c("zielgruppe" = "gray65", "andere" = primärfarbe)
  ) +
  coord_cartesian(expand = F, clip = "off") +
  labs(y = "Anteil der zugänglichen WG-Angebote") +
  theme_dunkel() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r=17.5),
                                    size = 9.5),
        plot.margin = margin(t=10,b=5,r=20,l=12.5)
  )




plot_gesamt <- (plot_waffle_w | plot_waffle_m) +
  plot_layout(axis_titles = "collect") +
  plot_annotation(
    title = glue("Nicht für alle gleich: Wie <b><span style='color:gray90'>Geschlecht</span></b> und<br><b><span style='color:gray90'>Alter</span></b> den <b><span style='color:{primärfarbe}'>Zugang</span></b> zu WGs prägen"),
    caption = "Abbildung: Fabian Hellmold/Datengeschichten"
  ) +
  plot_annotation(theme = theme(
    plot.background  = element_rect(fill = panel_background_color, color = "transparent"),
    panel.background = element_rect(fill = panel_background_color, color = "transparent"),
    plot.title.position = "plot",
    plot.title = element_markdown(
      color = plot_title_color,
      family = plot_title_family,
      hjust = 0.5,
      lineheight = 1.25,
      margin = margin(b = 27.5),
    ),
    plot.caption = element_text(
      color = plot_caption_color,
      family = plot_caption_family,
      face = "italic",
      margin = margin(t = 10, r = -12.5),
      size = 7
    ),
    plot.margin = margin(t = 10, b = 5, r = 7.5, l = 7.5)
  ))


## Abbildung lokal speichern ---------------------------------------------------

file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_waffle_18.png"
ggsave(filename = file_save_lokal, plot = plot_gesamt, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))


## Abbildung in Dropbox speichern ----------------------------------------------

# file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_waffle_18.png"
# 
# ggsave(filename = file_save_dropbox, plot = plot_gesamt,
#        width = 4, height = 5, units = "in", dpi = 300)
