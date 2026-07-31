


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######       RIDGELINEPLOT Hamburg - 5     #######    ###########            
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

farbe_unter <- "#7BAFC4"
farbe_mitte <- "#999999"
farbe_ueber <- "#E8967A"

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
                             TRUE ~ "über"),
         bereich = factor(bereich, levels = c("unter", "über")))


anteile <- dichte_df %>%
  group_by(bereich) %>%
  summarize(flaeche = sum(y) * mean(diff(x)), .groups = "drop") %>%
  mutate(pct_label = paste0(round(flaeche * 100), "%")) %>%
  mutate(color = case_when(
    bereich == "unter" ~ "#1B4F6B",
    bereich == "über" ~ "#8B2500"
  )) %>%
  mutate(x_pos = case_when(
    bereich == "unter" ~ grenzwert_1 -12.5,
    bereich == "über" ~ grenzwert_1 + 12.5
  )) %>%
  mutate(hjust = case_when(
    bereich == "unter" ~ 1,
    bereich == "über" ~ 0
  )) 


label_df <- tribble(
  ~x,              ~y,   ~hjust, ~color,      ~label,
  grenzwert_1-7.5, Inf,  1,      farbe_unter, "Angebote unter\naktueller Pauschale",
  grenzwert_1+7.5, Inf,  0,      farbe_ueber, "Angebote über\naktueller Pauschale"
)


# Abbildung erstellen ----------------------------------------------------------

Abb_Dichte <- ggplot(dichte_df, aes(x = x, y = y, fill = bereich)) +
  geom_area(color = "gray75", linewidth = 0.5) +
  geom_vline(xintercept = grenzwert_1, linetype = "solid", 
             color = "gray25", linewidth = 0.5) +
  geom_text(data = anteile,
            aes(x = x_pos, label = pct_label, hjust = hjust, color = color), 
            size = 4.75, inherit.aes = FALSE, fontface = "bold", 
            family = "franklin", y = 0.00014) +
  geom_text(data = label_df,
            aes(x = x, y = y, label = label,
                hjust = hjust, color = color),
            vjust = -0.25, family = "franklin", inherit.aes = FALSE, 
            size = 4.5, fontface = "italic", lineheight = 1) +
  scale_x_continuous(breaks = c(300,400,500,600,700,800,900,1000,1100),
                     labels = ~paste0(.,"€")) +
  scale_fill_manual(values = c("unter" = farbe_unter, "mitte" = farbe_mitte, 
                               "über" = farbe_ueber)) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  labs(
    title = NULL,
    x = glue("Zimmermiete in {stadt}"),
    y = "Dichte",
  ) +
  theme_dunkel() +
  theme(legend.position = "none", 
        plot.margin = margin(t=100, l=75, b=50, r = 75),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_line(color = panel_background_color),
        
        axis.title.x = element_text(margin = margin(t=10), size=14,  face = "bold"),
        axis.title.y = element_text(margin = margin(r=15), size=14, face = "bold",
                                    color = panel_background_color),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(face = "bold", size = 11, vjust = -1.5,
                                   colour = panel_background_color))


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/Ridgeline_Hamburg_5.png"
ggsave(filename = file_save, plot = Abb_Dichte, 
       width = 16, height = 9, units = "in", dpi = 300)

system(paste("xdg-open", shQuote(file_save)))
