


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######        RIDGELINEPLOT Kassel - 6     #######    ###########            
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


grenzwert_1 <- 380
grenzwert_2 <- 440

farbe_unter <- "#7BAFC4"
farbe_ueber <- "gray70"

stadt <- "Kassel"


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
    AND stadt = {stadt}
", .con = con_lokal)


WGdaten_ges <- dbGetQuery(con_lokal, sql) %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter(datum_scraping >= "2025-10-01" & datum_scraping <= "2026-03-31") %>%
  select(-datum_scraping)



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
    bereich == "über" ~ "transparent"
  )) %>%
  mutate(x_pos = case_when(
    bereich == "unter" ~ grenzwert_1 -12.5,
    bereich == "über" ~ grenzwert_1 + 12.5
  )) %>%
  mutate(hjust = case_when(
    bereich == "unter" ~ 1,
    bereich == "über" ~ 0
  )) 



# Abbildung erstellen ----------------------------------------------------------

Abb_Dichte <- ggplot(dichte_df, aes(x = x, y = y)) +
  geom_area(fill = "gray45", color = "gray75", linewidth = 0.5) +
  geom_vline(xintercept = grenzwert_1, 
             linetype = "solid", color = "gray35", linewidth = 0.5) +
  geom_vline(xintercept = grenzwert_2, 
             linetype = "solid", color = "gray85", linewidth = 0.5) +
  annotate(
    "text",
    x = grenzwert_1-52.5 , y = 0.005, 
    label = glue("aktuelle Pauschale ({grenzwert_1}€)"), 
    hjust = 1, color = "gray35") +
  geom_curve(x = grenzwert_1-50, 
             xend = grenzwert_1-5,  
             y = 0.005, yend = 0.00475,
             arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
             curvature = -0.15, color = "gray35", linewidth = 0.5,
             inherit.aes = F) +
  annotate(
    "text",
    x = grenzwert_2+42.5 , y = 0.00425, 
    label = glue("geplante Pauschale ({grenzwert_2}€)"), 
    hjust = 0, color = "gray75") +
  geom_curve(x = grenzwert_2+40, 
             xend = grenzwert_2+5,  
             y = 0.00425, yend = 0.004,
             arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
             curvature = 0.15, color = "gray75", linewidth = 0.5,
             inherit.aes = F) +
  
  scale_y_continuous(breaks = c(0.000, 0.001, 0.002, 0.003, 0.004, 0.005)) +
  scale_x_continuous(breaks = c(200,300,400,500,600,700,800)) +
  scale_fill_manual(values = c("unter" = farbe_unter, "mitte" = farbe_mitte, 
                               "über" = farbe_ueber)) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  labs(
    title = NULL,
    x = "Zimmermiete in €",
    y = "Dichte",
  ) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t=100, l=75, b=50, r = 75),
        plot.background  = element_rect(fill =  background_color, color = "transparent"),
        panel.background  = element_rect(fill =  background_color, color = "transparent"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(color = panel_grid_color),
        panel.grid.major.x = element_line(color = panel_grid_color),
        axis.line = element_line(color = axis_line_color),
        axis.line.y = element_line(color = background_color),
        axis.title.x = element_text(family = axis_title_family, margin = margin(t=7.5),
                                    size=14,  face = "bold", color = axis_title_color),
        axis.title.y = element_text(family = axis_title_family, margin = margin(r=15),
                                    size = 14, face = "bold", color = background_color),
        axis.text.x = element_text(family = axis_text_family, size = 11, color = axis_text_color),
        axis.text.y = element_text(face = "bold", size = 11, vjust = -1.5,
                                   family = axis_text_family, color = background_color))


file_save <- "Abbildungen/1_Präsentation_Linke_Kassel/Ridgeline_Kassel_6.png"
ggsave(filename = file_save, plot = Abb_Dichte, 
       width = 16, height = 9, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

