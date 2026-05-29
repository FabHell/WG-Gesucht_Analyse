


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######      RIDGELINEPLOT 380 €-Grenze     #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(DBI)
library(tidyverse)
library(ggridges)
library(glue)
library(ggtext)
library(showtext)

source("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Skripte\\Sonstiges\\laden_Fonts.R")

source("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Skripte\\Sonstiges\\Theme_dunkel.R")


stadt <- "Kassel"


con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")



## Erstellung Abbildung 1 ------------------------------------------------------


grenzwert_1 <- 380
grenzwert_2 <- 440

farbe_unter <- "#7BAFC4"
farbe_mitte <- "#C4B8A8"
farbe_ueber <- "#D4736A"

sql <- glue_sql("
  SELECT stadt, stadtteil_geocoding, gesamtmiete, datum_scraping
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND gesamtmiete IS NOT NULL
    AND stadt = {stadt}
", .con = con_lokal)


WGdaten_ges <- dbGetQuery(con_lokal, sql) %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(datum_scraping >= "2025-10-01" & datum_scraping <= "2026-03-31") %>%
  select(-datum_scraping)

stadtteil_filter <- WGdaten_ges %>%
  group_by(stadtteil_geocoding) %>%
  summarise(N =n()) %>%
  slice_max(N, n=5) %>% pull(stadtteil_geocoding)


WGdaten_trim <- WGdaten_ges %>%
  filter(stadtteil_geocoding %in% stadtteil_filter) %>%
  group_by(stadtteil_geocoding) %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.05),
    quantile(gesamtmiete, 0.95)
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
    bereich == "unter" ~ grenzwert_1 -10,
    bereich == "mitte" ~ (grenzwert_1+grenzwert_2)/2,
    bereich == "über" ~ grenzwert_2 + 10
  )) %>%
  mutate(color = case_when(
    bereich == "unter" ~ "#1B4F6B",
    bereich == "mitte" ~ "#7A6E65",
    bereich == "über" ~ "#8B2500"
  )) %>%
  mutate(y_pos = case_when(
    stadtteil_geocoding == "Vorderer Westen"  & bereich == "unter" ~ -2.0,
    stadtteil_geocoding == "Vorderer Westen"  & bereich == "mitte" ~ -1.6,
    stadtteil_geocoding == "Vorderer Westen"  & bereich == "über"  ~ -1.4,
    stadtteil_geocoding == "Mitte" & bereich == "unter" ~ -2.6,
    stadtteil_geocoding == "Mitte" & bereich == "mitte" ~ -1.8,
    stadtteil_geocoding == "Wesertor" & bereich == "unter" ~ -2.5,
    stadtteil_geocoding == "Wesertor" & bereich == "mitte" ~ -1.75,
    stadtteil_geocoding == "Südstadt" & bereich == "unter" ~ -2.5,
    stadtteil_geocoding == "Südstadt" & bereich == "mitte" ~ -1.75,
    TRUE ~ -1
  )) 


label_df <- tribble(
  ~x,              ~y,   ~hjust, ~color,      ~label,
  grenzwert_1-5,   Inf,  1,      farbe_unter, "Inserate unter aktueller Pauschale",
  grenzwert_2+5,   Inf,  0,      farbe_ueber, "Inserate über geplanter Pauschale"
)


dichte_stadtteile <- dichte_df %>%
  ggplot(aes(x = x, y = stadtteil_geocoding, fill = bereich, height = scale_y)) +
  geom_ridgeline(color = "gray40", linewidth = 0.5, alpha = 1, scale = 1.1) +
  geom_vline(xintercept = c(grenzwert_1, grenzwert_2), linetype = "solid", color = "gray10", 
             linewidth = 0.5) +
  geom_text(data = pct_df,
            aes(x = x_pos, y = stadtteil_geocoding, label = pct_label, 
                vjust = y_pos-0.5, hjust = hjust, color = color), size = 3.5,
            inherit.aes = FALSE, fontface = "bold", family = "franklin") +
  geom_text(data = label_df,
            aes(x = x, y = y, label = label,
                hjust = hjust, color = color),
            vjust = -1.25, family = "franklin", inherit.aes = FALSE, 
            size = 4, fontface = "italic") +
  scale_fill_manual(values = c("unter" = farbe_unter, 
                               "mitte" = farbe_mitte,
                               "über" = farbe_ueber)) +
  scale_color_identity() +
  scale_size_identity() +
  scale_x_continuous(breaks = c(150, 300, 450, 600, 750)) +
  scale_y_discrete(expand = c(0.1, 0.1)) +
  coord_cartesian(clip = "off") +
  labs(x = "Zimmermiete in €", y = NULL, title = NULL) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(),
        plot.margin = margin(r=50),
        plot.background  = element_rect(fill =  background_color, color = "transparent"),
        panel.background  = element_rect(fill =  background_color, color = "transparent"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(color = panel_grid_color),
        panel.grid.major.x = element_line(color = panel_grid_color),
        axis.line = element_line(color = axis_line_color),
        axis.line.y = element_line(color = background_color),
        axis.title.x = element_text(family = axis_title_family, margin = margin(t=10),
                                    size=14,  face = "bold", color = axis_title_color),
        axis.text.x = element_text(family = axis_text_family, size = 11, color = axis_text_color),
        axis.text.y = element_text(face = "bold", size = 14, vjust = -2, 
                                   family = axis_text_family, color = axis_title_color))





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



dichte_deutschland <- dichte_df %>%
  ggplot(aes(x = x, y = stadt, fill = einzelne_stadt, height = scale_y)) + 
  geom_ridgeline(color = background_color, linewidth = 0.3, alpha = 0, scale = 2) +
  scale_fill_manual(values = c("1" = farbe_stadt_einzelnd, 
                               "0" = farbe_stadt_andere)) +
  scale_x_continuous(breaks = c(250, 500, 750, 1000, 1250)) +
  coord_cartesian(clip = "off") +
  labs(x = "Zimmermiete in €", y = NULL, title = NULL) +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = background_color, color = background_color),
        plot.background = element_rect(fill = background_color, color = background_color),
        panel.grid.major.x = element_line(color = background_color),
        panel.grid.minor.x = element_line(color = background_color),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_line(color = background_color),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10), family = axis_title_family,
                                    color = background_color, size=14,  face = "bold"),
        axis.text.x = element_text( family = axis_text_family, color = background_color,
                                    margin = margin(t=7.5), size = 10),
        axis.text.y = element_markdown(family = axis_text_family, 
                                       size = c(rep(5, 10),0, 12, 0,rep(5, 40)),
                                       color = background_color))




Abb_gesamt_2 <- (dichte_stadtteile | dichte_deutschland) +
  plot_layout(widths = c(1.75, 1)) +
  plot_annotation(theme = theme(
    plot.background  = element_rect(fill =  background_color, color = background_color),    
    plot.margin = margin(t=100, l=75, b=50, r = 75)
  ))



file_save <- "Abbildungen/1_Präsentation_Linke_Kassel/Ridgeline_Stadtteile_1.png"
ggsave(filename = file_save, plot = Abb_gesamt_2, 
       width = 16, height = 9, units = "in", dpi = 300)
shell.exec(normalizePath(file_save))

