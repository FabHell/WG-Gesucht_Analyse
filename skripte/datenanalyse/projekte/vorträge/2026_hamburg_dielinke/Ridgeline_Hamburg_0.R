


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######       RIDGELINEPLOT Hamburg- 0      #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(DBI)
library(tidyverse)
library(ggridges)
library(glue)
library(ggtext)
library(showtext)

source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

grenzwert_1 <- 380
grenzwert_2 <- 440

farbe_unter <- "transparent"
farbe_mitte <- "transparent"
farbe_ueber <- "transparent"

stadt <- "Hamburg"


## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("DATABASE_PG"),
                       host = Sys.getenv("SERVER_PG"),
                       port = Sys.getenv("PORT_PG"),
                       user = Sys.getenv("UID_PG"),
                       password = Sys.getenv("PWD_PG"))

sql <- glue_sql("
  SELECT stadt, gesamtmiete, datum_scraping
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND gesamtmiete IS NOT NULL
    AND stadtteil_geocoding IS NOT NULL
    AND stadt = {stadt}
    AND link IN (
      SELECT link
      FROM analysedaten
      GROUP BY link
      HAVING COUNT(*) <= 2
    )
", .con = con_lokal)


WGdaten_ges <- dbGetQuery(con_lokal, sql) 



# Daten für Abbildung vorbereiten ----------------------------------------------

WGdaten_trim <- WGdaten_ges %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.01, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.99, na.rm = TRUE))


dichte_df <- with(WGdaten_trim, density(gesamtmiete, na.rm = TRUE)) %>%
  {data.frame(x = .$x, y = .$y)} %>%
  mutate(bereich = case_when(x < grenzwert_1 ~ "unter", 
                             x > grenzwert_1 & x < grenzwert_2 ~ "mitte",
                             x > grenzwert_2 ~ "über"),
         bereich = factor(bereich, levels = c("unter", "mitte", "über")))


anteile <- dichte_df %>%
  group_by(bereich) %>%
  summarize(flaeche = sum(y) * mean(diff(x)), .groups = "drop") %>%
  mutate(pct_label = paste0(round(flaeche * 100), "%")) %>%
  mutate(color = case_when(
    bereich == "unter" ~ "#1B4F6B",
    bereich == "mitte" ~ "gray20",
    bereich == "über" ~ "#8B2500"
  )) %>%
  mutate(x_pos = case_when(
    bereich == "unter" ~ grenzwert_1 -12.5,
    bereich == "mitte" ~ (grenzwert_1+grenzwert_2)/2,
    bereich == "über" ~ grenzwert_2 + 12.5
  )) %>%
  mutate(hjust = case_when(
    bereich == "unter" ~ 1,
    bereich == "mitte" ~ 0.5,
    bereich == "über" ~ 0
  )) 


# Abbildung erstellen ----------------------------------------------------------

Abb_Dichte <- ggplot(dichte_df, aes(x = x, y = y, fill = bereich)) +
  geom_area(linewidth = 0.25) +
  scale_fill_manual(values = c("unter" = farbe_unter, "mitte" = farbe_mitte, 
                               "über" = farbe_ueber)) +
  scale_color_identity() +
  labs(
    title = NULL,
    x = glue("Zimmermiete in {stadt}"),
    y = "Dichte",
  ) +
  scale_x_continuous(breaks = c(300,400,500,600,700,800,900,1000,1100),
                     labels = ~paste0(.,"€")) +
  theme_dunkel() +
  theme(legend.position = "none", 
        plot.margin = margin(t=100, l=75, b=50, r = 75),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(margin = margin(t=10), size=14,  face = "bold"),
        axis.title.y = element_text(margin = margin(r=15), size=14, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(face = "bold", size = 11, vjust = -1.5))


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/Ridgeline_Hamburg_0.png"
ggsave(filename = file_save, plot = Abb_Dichte, 
       width = 16, height = 9, units = "in", dpi = 300)

system(paste("xdg-open", shQuote(file_save)))

