


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######              ABB. SUNBURST          #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(showtext)
library(ggtext)
library(sf)
library(geomtextpath)
library(png)
library(DBI)
library(glue)
library(cowplot)


font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()



## Daten laden -----------------------------------------------------------------


St_Teile <-  st_read("C:\\Users\\hellm\\Downloads\\bezirksgrenzen.shp\\bezirksgrenzen.shp") %>%
  as.data.frame() %>%
  select(Gemeinde_n) %>% pull()

img <- readPNG("Daten/Wappen/wappen_berlin.png")

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql <- glue_sql("
  SELECT *
  FROM analysedaten
  WHERE stadt = 'Berlin'
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND stadtteil IN ({teile*})
 ", teile = St_Teile, .con = con_lokal)

WGdaten_Berlin <- dbGetQuery(con_lokal, sql) %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(month(datum_scraping) == 9)



## Erstellung der kategorialen Variablen ---------------------------------------

# HAUSTYP

data_haustyp <- WGdaten_Berlin %>%
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

data_wgart <- WGdaten_Berlin %>%
  filter(!is.na(wg_art)) %>%
  mutate(
    studenten_wg = str_detect(wg_art, "Studenten-WG"),
    keine_zweck_wg = str_detect(wg_art, "keine Zweck-WG"),
    männer_wg = str_detect(wg_art, "Männer-WG"),
    business_wg = str_detect(wg_art, "Business-WG"),
    wohnheim = str_detect(wg_art, "Wohnheim"),
    vegetarisch_vegan = str_detect(wg_art, "Vegetarisch/Vegan"),
    alleinerziehende = str_detect(wg_art, "Alleinerziehende"),
    funktionale_wg = str_detect(wg_art, "funktionale WG"),
    berufstätigen_wg = str_detect(wg_art, "Berufstätigen-WG"),
    gemischte_wg = str_detect(wg_art, "gemischte WG"),
    wg_mit_kindern = str_detect(wg_art, "WG mit Kindern"),
    verbindung = str_detect(wg_art, "Verbindung"),
    lgbtqia = str_detect(wg_art, "LGBTQIA+"),
    senioren_wg = str_detect(wg_art, "Senioren-WG"),
    inklusive_wg = str_detect(wg_art, "inklusive WG"),
    wg_neugründung = str_detect(wg_art, "WG-Neugründung"),
    zweck_wg = str_detect(wg_art, "Zweck-WG"),
    frauen_wg = str_detect(wg_art, "Frauen-WG"),
    plus_wg  = str_detect(wg_art, "Plus-WG"),
    mehrgenerationen = str_detect(wg_art, "Mehrgenerationen"),
    azubi_wg  = str_detect(wg_art, "Azubi-WG"),
    wohnen_für_hilfe = str_detect(wg_art, "Wohnen für Hilfe"),
    internationals_welcome = str_detect(wg_art, "Internationals welcome")
    ) %>%
  summarise(
    "Studenten-WG" = sum(studenten_wg, na.rm = TRUE),
#    "keine Zweck-WG" = sum(keine_zweck_wg, na.rm = TRUE),
    "Männer-WG" = sum(männer_wg, na.rm = TRUE),
    "Business-WG" = sum(business_wg, na.rm = TRUE),
    "Wohnheim" = sum(wohnheim, na.rm = TRUE),
    "Vegetarisch/Vegan" = sum(vegetarisch_vegan, na.rm = TRUE),
    "Alleinerziehende" = sum(alleinerziehende, na.rm = TRUE),
#    "funktionale WG" = sum(funktionale_wg, na.rm = TRUE),
    "Berufstätigen-WG" = sum(berufstätigen_wg, na.rm = TRUE),
#    "gemischte-WG" = sum(gemischte_wg, na.rm = TRUE),
    "WG mit Kindern" = sum(wg_mit_kindern, na.rm = TRUE),
    "Verbindung" = sum(verbindung, na.rm = TRUE),
    "LGBTQIA+" = sum(lgbtqia, na.rm = TRUE),
    "Senioren-WG" = sum(senioren_wg, na.rm = TRUE),
    "Inklusive-WG" = sum(gemischte_wg, na.rm = TRUE),
#    "WG-Neugründung" = sum(gemischte_wg, na.rm = TRUE),
#    "Zweck-WG" = sum(zweck_wg, na.rm = TRUE),
    "Frauen-WG" = sum(frauen_wg, na.rm = TRUE),
#    "Plus-WG" = sum(plus_wg, na.rm = TRUE),
    "Azubi-WG" = sum(azubi_wg, na.rm = TRUE),
    "Wohnen für Hilfe" = sum(wohnen_für_hilfe, na.rm = TRUE),
    "Internationals Welcome" = sum(internationals_welcome, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "WG-Art", values_to = "N") %>%
  mutate(variable = "WG-Art",
         N_ges = nrow(WGdaten_Berlin %>% filter(!is.na(wg_art))),
         value = N/N_ges*100) %>%
  select(variable, label = "WG-Art", value) %>%
  rbind(tibble(
    variable = "WG-Art",
    label = rep(NA, 2),
    value = rep(NA, 2)))
  

# SONSTIGES

data_sonstiges_raw <- WGdaten_Berlin %>%
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

data_rauchen <- WGdaten_Berlin %>%
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

# Norwegisch Gebärdensprache Griechisch
# Tadschikisch Urdu Japanisch Schwedisch
# Tagalog Koreanisch Vietnamesisch Tschechisch
# Ukrainisch Swahili Thai

data_sprache <- WGdaten_Berlin %>%
  filter(!is.na(sprache)) %>%
  mutate(
    deutsch = str_detect(sprache, "Deutsch"),
    englisch = str_detect(sprache, "Englisch"),
    französisch = str_detect(sprache, "Französisch"),
    italienisch = str_detect(sprache, "Italienisch"),
    spanisch = str_detect(sprache, "Spanisch"),
    niederländisch = str_detect(sprache, "Niederländisch"),
    portugiesisch = str_detect(sprache, "Portugiesisch"),
    hindi = str_detect(sprache, "Hindi"),
    persisch = str_detect(sprache, "Persisch"),
    russisch = str_detect(sprache, "Russisch"),
    arabisch = str_detect(sprache, "Arabisch"),
    chinesisch = str_detect(sprache, "Chinesisch"),
    türkisch = str_detect(sprache, "Türkisch"),
    gebärdensprache = str_detect(sprache, "Gebärdensprache"),
    polnisch = str_detect(sprache, "Polnisch")
    ) %>%
  summarise(
    Deutsch = sum(deutsch, na.rm = TRUE),
    Englisch = sum(englisch, na.rm = TRUE),
    Französisch = sum(französisch, na.rm = TRUE),
    Italienisch = sum(italienisch, na.rm = TRUE),
    Spanisch = sum(spanisch, na.rm = TRUE),
    Niederländisch = sum(niederländisch, na.rm = TRUE),
    Portugiesisch = sum(portugiesisch, na.rm = TRUE),
    Hindi = sum(hindi, na.rm = TRUE),
    Persisch = sum(persisch, na.rm = TRUE),
    Russisch = sum(russisch, na.rm = TRUE),
    Arabisch = sum(arabisch, na.rm = TRUE),
    Chinesisch = sum(chinesisch, na.rm = TRUE),
    Türkisch = sum(türkisch, na.rm = TRUE),
    Gebärdensprache = sum(gebärdensprache, na.rm = TRUE),
    Polnisch = sum(polnisch, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "sprache", values_to = "N") %>%
  mutate(variable = "Sprache",
         N_ges = nrow(WGdaten_Berlin %>% filter(!is.na(sprache))),
         value = N/N_ges*100) %>%
  select(variable, label = sprache, value) %>%
  rbind(tibble(
    variable = "Sprache",
    label = rep(NA, 2),
    value = rep(NA, 2)))


# GESCHLECHT

data_geschlecht <- WGdaten_Berlin %>%
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
  geom_text(data=label_data, aes(x=id, y=value+7.5, label=label, hjust=hjust), 
            color="gray50", fontface="bold",alpha=0.7, size=1.75, family="domine",
            angle= label_data$angle, inherit.aes = FALSE) +
  geom_textpath(data = percent_data, aes(x = factor(x), y = y, label = variable, 
                group = variable), colour = "gray", text_only = TRUE,
                size = 2.5, family = "franklin", alpha = 1, inherit.aes = FALSE) +
  geom_textpath(data = base_data, aes(x = as.numeric(title), y = -12.5,
                label = variable, group = variable), colour = base_data$colour, 
                text_only = TRUE, size = 2, fontface = "bold", family = "franklin", 
                alpha = 1, inherit.aes = FALSE) +
  scale_y_continuous(limits = c(-75,125),
                     breaks = c(0,25,50,75,100),
                     expand = c(0, 0)) +
  scale_fill_manual(values = farben_kategorien) +
  theme_minimal() +
  theme(
    legend.position = "none",
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
  draw_image(img, x=0.445, y=0.435, width=0.125, height=0.125)

file_save <- "Abbildungen/Sunburst_Berlin.png"
ggsave(filename = file_save, plot = Sunburst_Wappen, 
       width = 6, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

