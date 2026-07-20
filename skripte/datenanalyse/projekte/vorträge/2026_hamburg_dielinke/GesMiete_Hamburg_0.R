


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######         ALT/MIETE Hamburg - 0       #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(waffle)
library(ggrepel)
library(ggfx)
library(patchwork)
library(glue)
library(DBI)


source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

stadt <- "Hamburg"

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

filter_alter <- 19

filter_miete <- Inf

filter_profile <- tribble(
  ~profil,              ~geschlecht_filter,           ~filter_alter, ~filter_miete, ~filter_label,
  "M, 19J, Budget egal", c("Geschlecht egal", "Mann"), 19,            Inf,           T,
  "F, 19J, Budget egal", c("Geschlecht egal", "Frau"), 19,            Inf,           T
)



## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("DATABASE_PG"),
                       host = Sys.getenv("SERVER_PG"),
                       port = Sys.getenv("PORT_PG"),
                       user = Sys.getenv("UID_PG"),
                       password = Sys.getenv("PWD_PG"))

sql <- glue_sql("
  SELECT alter_ges, geschlecht_ges, gesamtmiete
  FROM analysedaten
  WHERE stadtteil_geocoding IS NOT NULL
    AND gesamtmiete IS NOT NULL
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND stadt = {stadt}
", .con = con_lokal)

daten_staedte_roh <- dbGetQuery(con_lokal, sql) 



## Daten aufbereiten -----------------------------------------------------------

daten_staedte <- daten_staedte_roh %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.01),
    quantile(gesamtmiete, 0.99)
  )) %>%
  mutate(alter_ges = ifelse(is.na(alter_ges), "16 und 99", alter_ges)) %>%
  mutate(alter_seq = map(alter_ges, ~ {
    teile <- str_extract_all(.x, "\\d+")[[1]] %>% as.integer()
    seq(teile[1], teile[2])
  }))



# Abbildung Waffleplot ---------------------------------------------------------

daten_waffle_w <- daten_staedte %>%
  mutate(gruppe_personen = ifelse(
    geschlecht_ges %in% c("Geschlecht egal", "Frau") & 
      map_lgl(alter_seq, ~ filter_alter %in% .x) &
      gesamtmiete <= filter_miete,
    "zielgruppe", "andere")) %>%
  group_by(gruppe_personen) %>%
  count() %>%
  ungroup() %>%
  mutate(n_scaled = round(n / sum(n) * 100) -100,
         n_scaled = n_scaled *(-1))

plot_waffle_w <- daten_waffle_w %>%
  ggplot(aes(fill = gruppe_personen, values = n_scaled)) +
  with_outer_glow(
    geom_waffle(n_rows = 4, size = 0.5, colour = panel_background_color, flip = T),
    colour = "gray40", sigma = 8, expand = 2
  ) +
  scale_fill_manual(
    values = c("zielgruppe" = "gray70", "andere" = "#3f74a6")
  ) +
  labs(
    title = glue("Frau, {filter_alter} Jahre<br>Budget: egal"),
    subtitle = glue("{daten_waffle_w[1,3]}%")
  ) +
  coord_cartesian(expand = F, clip = "off") +
  theme_dunkel() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        plot.margin = margin(t=0, l=100, b= 0, r = 25),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(family = "domine", margin = margin(b=-17.5), size = 11.5,
                                      vjust = 0, hjust = 0,  color = "gray80"),
        plot.subtitle = element_text(family = "domine", size = 22, hjust = 1,
                                     face = "bold", color = "gray90")
  )


daten_waffle_m <- daten_staedte %>%
  mutate(gruppe_personen = ifelse(
    geschlecht_ges %in% c("Geschlecht egal", "Mann") &
      map_lgl(alter_seq, ~ filter_alter %in% .x) &
      gesamtmiete <= filter_miete,
    "zielgruppe", "andere")) %>%
  group_by(gruppe_personen) %>%
  count() %>%
  ungroup() %>%
  mutate(n_scaled = round(n / sum(n) * 100) -100,
         n_scaled = n_scaled *(-1))

plot_waffle_m <- daten_waffle_m %>%
  ggplot(aes(fill = gruppe_personen, values = n_scaled)) +
  with_outer_glow(
    geom_waffle(n_rows = 4, size = 0.5, colour = panel_background_color, flip = T),
    colour = "gray40", sigma = 8, expand = 2
  ) +
  scale_fill_manual(
    values = c("zielgruppe" = "gray70", "andere" = "#3f74a6")
  ) +
  labs(
    title = glue("Mann, {filter_alter} Jahre<br>Budget: egal"),
    subtitle = glue("{daten_waffle_m[1,3]}%")
  ) +
  coord_cartesian(expand = F, clip = "off") +
  theme_dunkel() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        plot.margin = margin(t=0, l=25, b= 0, r = 100),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(family = "domine", margin = margin(b=-17.5), size = 11.5,
                                      vjust = 0, hjust = 0,  color = "gray80"),
        plot.subtitle = element_text(family = "domine", size = 22, hjust = 1,
                                     face = "bold", color = "gray90"))



## Abbildung Singleaxis --------------------------------------------------------


daten_profile <- filter_profile %>%
  pmap_dfr(function(profil, geschlecht_filter, filter_alter, filter_miete, filter_label) {
    daten_staedte %>%
      mutate(zielgruppe = geschlecht_ges %in% geschlecht_filter & 
               map_lgl(alter_seq, ~ filter_alter %in% .x) &
               gesamtmiete <= filter_miete) %>%
      summarise(anteil = mean(zielgruppe) * 100) %>%
      mutate(profil = profil, filter_label = filter_label, .before = 1)
  })


breite_bar <- 0.045

plot_singleaxis <- ggplot() +
  labs(y = "Spannender Achsentitel") +
  scale_y_continuous(breaks = c(25,50,75)) +
  coord_cartesian(ylim = c(0,100), xlim = c(-0.5, 0.5),
                  clip = "off", expand = F) +
  theme_dunkel(gridline_x = FALSE, gridline_y = F) +
  theme(plot.margin = margin(t=50,l=25,b=50,r=75),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14, color = panel_background_color,
                                    margin = margin(r=25)), 
        axis.line = element_blank())


## Plots zusammenfügen ---------------------------------------------------------


plot_gesamt <- (plot_waffle_w | plot_waffle_m | plot_singleaxis) +
  plot_layout(widths = c(0.30, 0.30, 1)) &
  plot_annotation(theme = theme(
    plot.background  = element_rect(fill = panel_background_color, 
                                    color = "transparent"),
    panel.background = element_rect(fill = panel_background_color, 
                                    color = "transparent"),
    plot.margin = margin(t=25,b=25,r=25,l=25)
  ))

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/GesMiete_Hamburg_0.png"
ggsave(filename = file_save, plot = plot_gesamt, 
       width = 16, height = 9, units = "in", dpi = 300)

system(paste("xdg-open", shQuote(file_save)))



