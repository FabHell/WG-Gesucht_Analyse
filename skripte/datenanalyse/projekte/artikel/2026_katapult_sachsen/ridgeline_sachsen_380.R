


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

grenzwert <- 380



## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

sql <- glue_sql("
  SELECT stadt, gesamtmiete, datum_scraping
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND gesamtmiete IS NOT NULL
    AND bundesland = 'Sachsen'
", .con = con_lokal)

WGdaten_ges <- dbGetQuery(con_lokal, sql) %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(datum_scraping >= "2025-10-01") %>%
  select(-datum_scraping)


## Daten aufbereiten -----------------------------------------------------------

WGdaten_trim <- WGdaten_ges %>%
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
         bereich = ifelse(x < grenzwert, "unter", "über")) %>%
  ungroup() %>%
  mutate(stadt = factor(stadt, levels = median_mieten$stadt))


## Abbildung erstellen und speichern -------------------------------------------

pct_df <- dichte_df %>%
  group_by(stadt, bereich) %>%
  summarize(flaeche = sum(scale_y) * mean(diff(x)), .groups = "drop") %>%  
  group_by(stadt) %>%
  mutate(pct = flaeche / sum(flaeche),
         pct_label = paste0(round(pct * 100), "%")) %>%
  mutate(hjust = ifelse(bereich == "unter", 1, 0)) %>%
  mutate(x_pos = ifelse(bereich == "unter", grenzwert -10, 
                                            grenzwert +10)) %>%
  mutate(color = ifelse(bereich == "unter", "#1B4F6B", "#8B2500")) %>%
  mutate(y_pos = case_when(
    stadt == "Leipzig"  & bereich == "unter" ~ -3.75,
    stadt == "Leipzig"  & bereich == "über"  ~ -3,
    stadt == "Dresden"  & bereich == "unter" ~ -2,
    stadt == "Dresden"  & bereich == "über"  ~ -2,
    stadt == "Chemnitz" & bereich == "unter" ~ -2.25,
    stadt == "Chemnitz" & bereich == "über"  ~ -2.25
  )) 


dichte_df %>%
  ggplot(aes(x = x, y = stadt, fill = bereich, height = scale_y)) +
  geom_ridgeline(color = "#1D1E34", linewidth = 0.3, alpha = 1, scale = 1.25) +
  geom_vline(xintercept = grenzwert, linetype = "solid", color = "gray30", 
             linewidth = 0.75) +
  geom_text(data = pct_df,
            aes(x = x_pos, y = stadt, label = pct_label, vjust = y_pos,
                hjust = hjust, color = color),
            inherit.aes = FALSE, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("unter" = "#7BAFC4", "über" = "#E8967A")) +
  scale_color_identity() +
  scale_x_continuous(breaks = c(150, 300, 450, 600, 750)) +
  scale_y_discrete(expand = c(0.1, 0.1)) +
  coord_cartesian(clip = "off") +
  labs(x = "Zimmermiete in €", y = NULL, title = NULL) +
  theme_minimal() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.9, 0.1),
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_text(face = "bold", size = 12, vjust = -3.5))

file_save <- "Abbildungen/Analyse_Katapult_Sachsen/Ridgeline_Sachsen_380.png"
ggsave(filename = file_save, plot = last_plot(), 
       width = 9, height = 5, units = "in", dpi = 300)
shell.exec(normalizePath(file_save))



