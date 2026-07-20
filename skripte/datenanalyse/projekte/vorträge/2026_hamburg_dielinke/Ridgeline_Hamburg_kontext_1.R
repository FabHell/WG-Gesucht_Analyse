


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######     RIDGELINE Hamburg Kontext - 1   #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(DBI)
library(tidyverse)
library(patchwork)
library(ggridges)
library(glue)
library(ggtext)
library(showtext)

source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

stadt <- "Hamburg"

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

con_lokal <- dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("DATABASE_PG"),
                       host = Sys.getenv("SERVER_PG"),
                       port = Sys.getenv("PORT_PG"),
                       user = Sys.getenv("UID_PG"),
                       password = Sys.getenv("PWD_PG"))



## Erstellung Abbildung 1 ------------------------------------------------------


grenzwert_1 <- 380
grenzwert_2 <- 440

farbe_unter <- "#7BAFC4"
farbe_mitte <- "#C4B8A8"
farbe_ueber <- "#D4736A"

sql <- glue_sql("
  SELECT stadt, stadtteil_geocoding, gesamtmiete
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND stadtteil_geocoding IS NOT NULL
    AND gesamtmiete IS NOT NULL
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND stadt = {stadt}
", .con = con_lokal)


WGdaten_ges <- dbGetQuery(con_lokal, sql)

stadtteil_filter <- WGdaten_ges %>%
  group_by(stadtteil_geocoding) %>%
  summarise(N =n()) %>%
  slice_max(N, n=7) %>% pull(stadtteil_geocoding)


WGdaten_trim <- WGdaten_ges %>%
  filter(stadtteil_geocoding %in% stadtteil_filter) %>%
  group_by(stadtteil_geocoding) %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.025),
    quantile(gesamtmiete, 0.975)
  )) %>%
  ungroup()

median_mieten <- WGdaten_trim %>%
  group_by(stadtteil_geocoding) %>%
  summarise(median_miete = median(gesamtmiete, na.rm = TRUE)) %>%
  arrange(median_miete)

dichte_df <- WGdaten_trim %>%
  group_by(stadtteil_geocoding) %>%
  summarise(dichte = list(density(gesamtmiete, na.rm = TRUE)), .groups = "drop") %>%
  mutate(x = map(dichte, ~.$x),
         y = map(dichte, ~.$y)) %>%
  select(-dichte) %>%
  unnest(cols = c(x, y)) %>%
  group_by(stadtteil_geocoding) %>%
  mutate(scale_y = y / max(y)) %>%
  ungroup() %>%
  mutate(bereich = case_when(x < grenzwert_1 ~ "unter", 
                             x > grenzwert_1 & x < grenzwert_2 ~ "mitte",
                             x > grenzwert_2 ~ "über"),
         bereich = factor(bereich, levels = c("unter", "mitte", "über"))) %>%
  ungroup() %>%
  mutate(stadtteil_geocoding = factor(stadtteil_geocoding, 
                                      levels = median_mieten$stadtteil_geocoding))



pct_df <- dichte_df %>%
  group_by(stadtteil_geocoding, bereich) %>%
  summarize(flaeche = sum(scale_y) * mean(diff(x)), .groups = "drop") %>%
  group_by(stadtteil_geocoding) %>%
  mutate(pct = flaeche / sum(flaeche),
         pct_label = paste0(round(pct * 100), "%")) %>%
  mutate(hjust = case_when(
    bereich == "unter" ~ 1,
    bereich == "mitte" ~ 0.5,
    bereich == "über" ~ 0
  )) %>%
  mutate(x_pos = case_when(
    bereich == "unter" ~ grenzwert_1 - 20,
    bereich == "mitte" ~ (grenzwert_1+grenzwert_2)/2,
    bereich == "über" ~ grenzwert_2 + 20
  )) %>%
  mutate(color = case_when(
    bereich == "unter" ~ "#1B4F6B",
    bereich == "mitte" ~ "#7A6E65",
    bereich == "über" ~ "#8B2500"
  )) %>%
  mutate(y_pos = case_when(
    bereich == "unter" & stadtteil_geocoding == "Barmbek-Nord" ~ -1.0,
    bereich == "unter" & stadtteil_geocoding == "Harburg" ~ -1.25,
    bereich == "unter" & stadtteil_geocoding == "Altona-Nord" ~ -0.75,
    bereich == "unter" & stadtteil_geocoding == "Eimsbüttel" ~ -0.5,
    bereich == "unter" & stadtteil_geocoding == "Ottensen" ~ -0.25,
    bereich == "unter" & stadtteil_geocoding == "St. Pauli" ~ -0.75,
    bereich == "unter" & stadtteil_geocoding == "Winterhude" ~ -0.5,
    
    bereich == "mitte" & stadtteil_geocoding == "Eimsbüttel" ~ -1.75,
    bereich == "mitte" & stadtteil_geocoding == "Ottensen" ~ -2,
    bereich == "mitte" & stadtteil_geocoding == "Winterhude" ~ -1.75,
    
    bereich == "über" & stadtteil_geocoding == "Harburg" ~ -1,
    TRUE ~ -0.1
  )) 


label_df <- tribble(
  ~x,              ~y,   ~hjust, ~color,      ~label,
  grenzwert_1-5,   Inf,  1,      farbe_unter, "Angebote unter\naktueller Pauschale",
  grenzwert_2+5,   Inf,  0,      farbe_ueber, "Angebote über\nkünftiger Pauschale"
)


dichte_stadtteile <- dichte_df %>%
  ggplot(aes(x = x, y = stadtteil_geocoding, fill = bereich, height = scale_y)) +
  geom_ridgeline(color = "gray40", linewidth = 0.5, alpha = 1, scale = 1.1) +
  # geom_vline(xintercept = c(grenzwert_1, grenzwert_2), linetype = "solid", color = "gray20", 
  #            linewidth = 0.5) +
  geom_text(data = pct_df,
            aes(x = x_pos, y = stadtteil_geocoding, label = pct_label, 
                vjust = y_pos-0.5, hjust = hjust, color = color), size = 3.5,
            inherit.aes = FALSE, fontface = "bold", family = "franklin") +
  geom_text(data = label_df,
            aes(x = x, y = y, label = label,
                hjust = hjust, color = color),
            vjust = -0.75, family = "franklin", inherit.aes = FALSE, 
            size = 4, fontface = "italic", lineheight = 1) +
  scale_fill_manual(values = c("unter" = farbe_unter, 
                               "mitte" = farbe_mitte,
                               "über" = farbe_ueber)) +
  scale_color_identity() +
  scale_size_identity() +
  scale_x_continuous(breaks = c(300,400,500,600,700,800,900,1000,1100),
                     labels = ~paste0(.,"€")) +
  scale_y_discrete(expand = c(0.1, 0.1)) +
  coord_cartesian(clip = "off") +
  labs(x = glue("Zimmermiete in {stadt}er Stadteilen"), 
       y = NULL, title = NULL) +
  theme_dunkel() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_line(color = panel_background_color),
        axis.title.x = element_text(margin = margin(t=12.5)),
        plot.margin = margin(r = 50),
        axis.text.y = element_text(face = "bold", size = 11, vjust = -1.5,
                                   colour = axis_title_color))






## Erstellen Abbildung 2 -------------------------------------------------------


farbe_stadt_einzelnd <- "#b3cde3" 
farbe_stadt_andere   <- "gray50"

auswahl_stadt <- stadt


sql <- glue_sql("
  SELECT bundesland, stadt, gesamtmiete
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND gesamtmiete IS NOT NULL
    AND land = 'Deutschland'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
", .con = con_lokal)

WGdaten_ges <- dbGetQuery(con_lokal, sql)



WGdaten_trim <- WGdaten_ges %>%
  mutate(stadt = case_when(
    stadt == "Freiburg im Breisgau" ~ "Freiburg i.B.",
    stadt == "Frankfurt am Main" ~ "Frankfurt a.M.",
    TRUE ~ stadt)) %>%
  group_by(stadt) %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.01),
    quantile(gesamtmiete, 0.99)
  )) %>%
  ungroup()

median_mieten <- WGdaten_trim %>%
  group_by(stadt) %>%
  summarise(median_miete = median(gesamtmiete, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(stadt = ifelse(stadt %in% auswahl_stadt, 
                        glue("**{stadt}**"), 
                        as.character(stadt))) %>%
  arrange(median_miete)

dichte_df <- WGdaten_trim %>%
  group_by(stadt) %>%
  summarise(dichte = list(density(gesamtmiete, na.rm = TRUE)), 
            .groups = "drop") %>%
  mutate(x = map(dichte, ~.$x),
         y = map(dichte, ~.$y)) %>%
  select(-dichte) %>%
  unnest(cols = c(x, y)) %>%
  group_by(stadt) %>%
  mutate(scale_y = y / max(y)) %>%
  ungroup() %>%
  mutate(einzelne_stadt = ifelse(stadt %in% auswahl_stadt,"1","0"),
         stadt = ifelse(stadt %in% auswahl_stadt, 
                        glue("**{stadt}**"), 
                        as.character(stadt))) %>%
  mutate(stadt = factor(stadt, levels = median_mieten$stadt))



## Label Y-Achse ---------------------------------------------------------------

label_sizes <- levels(dichte_df$stadt) %>%
  {tibble(stadt = ., position = seq_along(.))} %>%
  mutate(idx = which(stadt == glue("**{auswahl_stadt}**")),
         size = case_when(
           position == idx ~ 12,
           position %in% c(idx - 1, idx + 1) ~ 0,
           TRUE ~ 5
         )) %>%
  pull(size)

label_color <- levels(dichte_df$stadt) %>%
  {tibble(stadt = ., position = seq_along(.))} %>%
  mutate(idx = match(glue("**{auswahl_stadt}**"), stadt),
         color = case_when(
           position == idx ~ "gray70",
           TRUE ~ "gray30"
         )) %>%
  pull(color)


dichte_deutschland <- dichte_df %>%
  ggplot(aes(x = x, y = stadt, fill = einzelne_stadt, height = scale_y)) + 
  geom_ridgeline(color = panel_background_color, linewidth = 0.3, alpha = 0, scale = 2) +
  scale_fill_manual(values = c("1" = farbe_stadt_einzelnd, 
                               "0" = farbe_stadt_andere)) +
  scale_x_continuous(breaks = c(250, 500, 750, 1000, 1250)) +
  coord_cartesian(clip = "off") +
  labs(x = "Zimmermiete in €", y = NULL, title = NULL) +
  theme_dunkel() +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(color = panel_background_color),
        panel.grid.minor.x = element_line(color = panel_background_color),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_line(color = panel_background_color),
        axis.line.y = element_blank(),
        axis.title.x = element_text(color = panel_background_color),
        axis.text.x = element_text(color = panel_background_color),
        axis.text.y = element_markdown(family = axis_text_family,
                                       size = label_sizes, color = panel_background_color))



Abb_gesamt_2 <- (dichte_stadtteile | dichte_deutschland) +
  plot_layout(widths = c(1.75, 1)) +
  plot_annotation(theme = theme(
    plot.background  = element_rect(fill =  panel_background_color, 
                                    color = panel_background_color),    
    plot.margin = margin(t=100, l=75, b=50, r = 75)
  ))


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/Ridgeline_Hamburg_kontext_1.png"
ggsave(filename = file_save, plot = Abb_gesamt_2, 
       width = 16, height = 9, units = "in", dpi = 300)

system(paste("xdg-open", shQuote(file_save)))



