


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######        RIDGELINEPLOT 380/440 €      #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(DBI)
library(tidyverse)
library(ggridges)
library(glue)
library(showtext)
library(ggtext)

font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()

grenzwert_1 <- 380
grenzwert_2 <- 440

farbe_unter <- "#b3cde3"
farbe_mitte <- "#74a9cf"
farbe_über  <- "#ef8a62"


## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

sql <- glue_sql("
  SELECT land, stadt, gesamtmiete
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND gesamtmiete IS NOT NULL
    AND land = 'Deutschland'
", .con = con_lokal)

WGdaten_ges <- dbGetQuery(con_lokal, sql) %>%
  select(-land)


## Daten aufbereiten -----------------------------------------------------------

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
  summarise(median_miete = median(gesamtmiete, na.rm = TRUE)) %>%
  arrange(median_miete)

dichte_df <- WGdaten_trim %>%
  group_by(stadt) %>%
  summarise(dichte = list(density(gesamtmiete, na.rm = TRUE)), .groups = "drop") %>%
  mutate(x = map(dichte, ~.$x),
         y = map(dichte, ~.$y)) %>%
  select(-dichte) %>%
  unnest(cols = c(x, y)) %>%
  group_by(stadt) %>%
  mutate(scale_y = y / max(y)) %>%
  ungroup() %>%
  group_by(stadt) %>%
  mutate(scale_y = y / max(y),
         bereich = case_when(x < grenzwert_1 ~ "unter", 
                             x > grenzwert_1 & x < grenzwert_2 ~ "mitte",
                             x > grenzwert_2 ~ "über"),
         bereich = factor(bereich, levels = c("unter", "mitte", "über"))) %>%
  ungroup() %>%
  mutate(stadt = factor(stadt, levels = median_mieten$stadt))


## Abbildung erstellen und speichern -------------------------------------------

dichte_df %>%
  ggplot(aes(x = x, y = stadt, fill = bereich, height = scale_y)) +
  geom_ridgeline(color = "#1D1E34", linewidth = 0.3, , alpha = 1, scale = 2) +
  scale_fill_manual(values = c("unter" = farbe_unter, "mitte" = farbe_mitte, 
                               "über" = farbe_über)) +  
  geom_vline(xintercept = c(grenzwert_1, grenzwert_2), linetype = "solid", 
             color = "gray30", linewidth = 0.25) +
  scale_x_continuous(breaks = c(250, 500, 750, 1000, 1250)) +
  labs(x = "Zimmermiete", y = NULL, title = NULL) +
  theme_minimal() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.9,0.1),
        axis.line = element_line(color = "gray10"),
        axis.title.x = element_text(margin = margin(t=5), family = "domine"),
        axis.text = element_text(family = "franklin"))

file_save <- "Abbildungen/Ridgeline_380_440.png"
ggsave(filename = file_save, plot = last_plot(), 
       width = 6, height = 9, units = "in", dpi = 300)
shell.exec(normalizePath(file_save))
