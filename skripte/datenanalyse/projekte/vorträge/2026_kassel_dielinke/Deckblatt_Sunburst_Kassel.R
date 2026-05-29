


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######       DECKBLATT SUNBURST KASSEL     #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


stadt <- "Kassel"

library(tidyverse)
library(showtext)
library(ggtext)
library(sf)
library(geomtextpath)
library(png)
library(DBI)
library(glue)
library(cowplot)

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

source("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Skripte\\Sonstiges\\laden_Fonts.R")

source("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Skripte\\Sonstiges\\Theme_dunkel.R")



## Daten laden -----------------------------------------------------------------


img <- readPNG(glue("Daten/Wappen/{stadt}_wappen.png"))

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql <- glue_sql("
  SELECT angaben_zum_objekt, wg_art, rauchen, sprache, geschlecht_ges, datum_scraping
  FROM analysedaten
  WHERE stadt = {stadt}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
 ", .con = con_lokal)

WGdaten_Stadt <- dbGetQuery(con_lokal, sql) %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(datum_scraping >= datum_von & datum_scraping <= datum_bis) %>%
  select(-datum_scraping)



## Erstellung der kategorialen Variablen ---------------------------------------

# HAUSTYP

data_haustyp <- WGdaten_Stadt %>%
  mutate(
    haustyp = angaben_zum_objekt %>%
      str_extract_all("Altbau|sanierter Altbau|Neubau|Reihenhaus|Doppelhaus|Einfamilienhaus|Alleinerziehende|Mehrfamilienhaus|Hochhaus|Plattenbau") %>%
      map_chr(~ if (length(.x) == 0) NA_character_ else str_c(.x, collapse = ", "))
  ) %>%
  filter(!is.na(haustyp)) %>%
  group_by(haustyp) %>%
  summarise(N = n()) %>%
  mutate(variable = "Haustyp",
         N_ges = sum(N),
         value = N/N_ges*100) %>%
  select(variable, label = haustyp, value) %>%
  rbind(tibble(
    variable = "Haustyp",
    label = rep(NA, 2),
    value = rep(NA, 2)))


# WG-Arten 

ausschluss_wgart <- c("", "keine Zweck-WG", "WG-Neugründung", "gemischte WG")

data_wgart <- WGdaten_Stadt %>%
  filter(!is.na(wg_art)) %>%
  separate_longer_delim(wg_art, delim = ", ") %>%
  count(wg_art) %>%
  arrange(desc(n)) %>%
  filter(!(wg_art %in% ausschluss_wgart)) %>%
  head(15) %>%
  mutate(variable = "WG-Art",
         n_ges = nrow(WGdaten_Stadt %>% filter(!is.na(wg_art))),
         value = n/n_ges*100) %>%
  select(variable, label = "wg_art", value) %>%
  rbind(tibble(
    variable = "wg_art",
    label = rep(NA, 2),
    value = rep(NA, 2)))


# SONSTIGES

data_sonstiges_raw <- WGdaten_Stadt %>%
  mutate(
    sonstiges = angaben_zum_objekt %>%
      str_extract_all("Waschmaschine|Spülmaschine|Terrasse|Balkon|Garten|Gartenmitbenutzung|Keller|Aufzug|Haustiere erlaubt|Fahrradkeller|Badewanne|Dusche") %>%
      map_chr(~ if (length(.x) == 0) NA_character_ else str_c(.x, collapse = ", "))
  ) %>%
  filter(!is.na(sonstiges))

data_sonstiges <- data_sonstiges_raw %>%
  mutate(
    waschmaschine = str_detect(sonstiges, "Waschmaschine"),
    spülmaschine = str_detect(sonstiges, "Spülmaschine"),
    terrasse = str_detect(sonstiges, "Terrasse"),
    balkon = str_detect(sonstiges, "Balkon"),
    garten = str_detect(sonstiges, "Garten"),
    keller = str_detect(sonstiges, "Keller"),
    aufzug = str_detect(sonstiges, "Aufzug"),
    haustiere_erlaubt = str_detect(sonstiges, "Haustiere erlaubt"),
    fahrradkeller = str_detect(sonstiges, "Fahrradkeller"),
    badewanne = str_detect(sonstiges, "Badewanne"),
    dusche = str_detect(sonstiges, "Dusche")
  ) %>%
  summarise(
    Spülmaschine = sum(spülmaschine, na.rm = TRUE),
    Waschmaschine = sum(waschmaschine, na.rm = TRUE),
    Terrasse = sum(terrasse, na.rm = TRUE),
    Balkon = sum(balkon, na.rm = TRUE),
    Garten = sum(garten, na.rm = TRUE),
    Keller = sum(keller, na.rm = TRUE),
    Aufzug = sum(aufzug, na.rm = TRUE),
    "Haustiere erlaubt" = sum(haustiere_erlaubt, na.rm = TRUE),
    Fahrradkeller = sum(fahrradkeller, na.rm = TRUE),
    Badewanne = sum(badewanne, na.rm = TRUE),
    Dusche = sum(dusche, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Sonstiges", values_to = "N") %>%
  mutate(variable = "Sonstiges",
         N_ges = nrow(data_sonstiges_raw),
         value = N/N_ges*100) %>%
  select(variable, label = Sonstiges, value) %>%
  rbind(tibble(
    variable = "Sonstiges",
    label = rep(NA, 2),
    value = rep(NA, 2)))


# RAUCHEN

data_rauchen <- WGdaten_Stadt %>%
  filter(!is.na(rauchen)) %>%
  group_by(rauchen) %>%
  summarise(N = n()) %>%
  mutate(variable = "Rauchen",
         N_ges = sum(N),
         value = N/N_ges*100) %>%
  select(variable, label = rauchen, value) %>%
  rbind(tibble(
    variable = "Rauchen",
    label = rep(NA, 2),
    value = rep(NA, 2)))


# Gesprochene Sprachen

data_sprache <- WGdaten_Stadt %>%
  filter(!is.na(sprache)) %>%
  separate_longer_delim(sprache, delim = ", ") %>%
  count(sprache) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  mutate(variable = "Sprache",
         N_ges = nrow(WGdaten_Stadt %>% filter(!is.na(sprache))),
         value = n/N_ges*100) %>%
  select(variable, label = sprache, value) %>%
  rbind(tibble(
    variable = "Sprache",
    label = rep(NA, 2),
    value = rep(NA, 2)))


# GESCHLECHT

data_geschlecht <- WGdaten_Stadt %>%
  filter(!is.na(geschlecht_ges)) %>%
  group_by(geschlecht_ges) %>%
  summarise(N = n()) %>%
  mutate(variable = "Geschlecht",
         N_ges = sum(N),
         value = N/N_ges*100) %>%
  select(variable, label = geschlecht_ges, value) %>%
  rbind(tibble(
    variable = "Geschlecht",
    label = rep(NA, 2),
    value = rep(NA, 2)))



## Daten zusammenfügen und weitere Datensätze erstellen ------------------------ 


data_ges <- rbind(tibble(variable = "Dummy",
                         label = rep(NA, 3),
                         value = rep(NA, 3)),
                  data_geschlecht, data_haustyp, data_rauchen, 
                  data_sprache, data_sonstiges, data_wgart) %>%
  group_by(variable) %>%
  arrange(value, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(id = seq(1, nrow(.)),
         label = str_remove(label, "Rauchen "))

label_data <- data_ges %>%
  mutate(angle = 90 - 360 * (id-0.5) /nrow(.),
         hjust = ifelse(angle < -90, 1, 0),
         angle = ifelse(angle < -90, angle+180, angle))

farben_kategorien <- c(
  "Geschlecht" = "#74a9cf",
  "Haustyp"    = "#41ab5d",
  "Rauchen"    = "#e34a33",
  "Sonstiges"  = "#807dba",
  "Sprache"    = "#fec44f",
  "WG-Art"     = "#2b8cbe"
)

percent_data <- tribble(
  ~variable, ~y, ~x,
  "0 %", 5, 1,
  "25 %", 30, 1,
  "50 %", 55, 1,
  "75 %", 80, 1,
  "100 %", 105, 1
) 

base_data <- data_ges %>%
  na.omit() %>%
  group_by(variable) %>% 
  summarize(start=min(id)-0.5, end=max(id)+0.5) %>% 
  rowwise() %>% 
  mutate(title = (start + end) / 2,
         colour = farben_kategorien[variable])



## Plot erstellen --------------------------------------------------------------

Sunburst <- ggplot(data_ges, aes(x=as.factor(id), y=value, fill = variable)) + 
  geom_bar(stat="identity", alpha = 0.7) +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), 
               colour = "gray25", alpha=0.8, linewidth =0.6 , inherit.aes = FALSE) +
  geom_text(data=label_data, aes(x=id, y=value+6.5, label=label, hjust=hjust), 
            color="gray50", fontface="bold",alpha=0.7, size=1.75, family="domine",
            angle= label_data$angle, inherit.aes = FALSE) +
  geom_textpath(data = percent_data, aes(x = factor(x), y = y, label = variable, 
                                         group = variable), colour = "gray", text_only = TRUE,
                size = 2.5, family = "franklin", alpha = 1, inherit.aes = FALSE) +
  geom_textpath(data = base_data, aes(x = as.numeric(title), y = -12.5,
                                      label = variable, group = variable), colour = base_data$colour, 
                text_only = TRUE, size = 2, fontface = "bold", family = "domine", 
                alpha = 1, inherit.aes = FALSE) +
  scale_y_continuous(limits = c(-75,125),
                     breaks = c(0,25,50,75,100),
                     expand = c(0, 0)) +
  scale_fill_manual(values = farben_kategorien) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#1c202a"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.1,
                                      color = c("gray25", "gray25", "gray25",
                                                "gray25", "gray25", NA)),
    plot.margin = unit(rep(-1,4), "cm")) +
  coord_polar(clip = "off")


Sunburst_Wappen <- ggdraw(Sunburst) +
  draw_image(img, x=0.43, y=0.415, width=0.145, height=0.145)


Plot_text <-  ggplot() +
  geom_text(aes(x = 1, y = 1.15, label = glue("Wohnen in {stadt}s\nWohngemeinschaften")), 
            size = 14, family = plot_title_family, color = plot_title_color,
            fontface = "bold") +
  geom_text(aes(x = 1, y = 0.85, label = "Eine Expedition durch das Datendickicht\ngemeinschaftlichen Wohnens"), 
            size = 8, family = axis_title_family, color = plot_title_color) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = background_color)
  ) +
  coord_cartesian(xlim = c(0.5, 1.5), ylim = c(0.5, 1.5))
  
  

Plot_ges <- Plot_text + Sunburst_Wappen +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = background_color, 
                                     color = background_color)
    )
  )



file_save <- "Abbildungen/1_Präsentation_Linke_Kassel/1_Titelblatt_Sunburst_Kassel.png"
ggsave(filename = file_save, plot = Plot_ges, 
       width = 16, height = 9, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

