


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ALT/GES Hamburg - 7        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(waffle)
library(ggh4x)
library(geomtextpath)
library(patchwork)
library(glue)
library(ggtext)
library(showtext)
library(DBI)


source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

stadt <- "Hamburg"

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

filter_alter <- 19



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

Daten_Staedte_roh <- dbGetQuery(con_lokal, sql) 



## Daten aufbereiten -----------------------------------------------------------

Daten_Staedte <- Daten_Staedte_roh %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.01),
    quantile(gesamtmiete, 0.99)
  )) %>%
  mutate(alter_ges = ifelse(is.na(alter_ges), "16 und 99", alter_ges)) %>%
  mutate(alter_seq = map(alter_ges, ~ {
    teile <- str_extract_all(.x, "\\d+")[[1]] %>% as.integer()
    seq(teile[1], teile[2])
  }),
  frau_offen = geschlecht_ges %in% c("Geschlecht egal", "Frau"),
  mann_offen = geschlecht_ges %in% c("Geschlecht egal", "Mann")
  )



## Abbildung Alter -------------------------------------------------------------


Anteil_ges_egal <- nrow(subset(Daten_Staedte_roh, is.na(alter_ges))) / nrow(Daten_Staedte_roh) * 100

Daten_Staedte_alter <- Daten_Staedte %>%
  unnest(alter_seq) %>%
  count(alter_seq, name = "haeufigkeit") %>%
  filter(alter_seq >= 18 & alter_seq <= 40) %>%
  mutate(anteil = haeufigkeit / nrow(Daten_Staedte) * 100) 


interp <- approx(
  x = Daten_Staedte_alter$alter_seq,
  y = Daten_Staedte_alter$anteil,
  xout = seq(min(Daten_Staedte_alter$alter_seq), max(Daten_Staedte_alter$alter_seq), length.out = 50000)
)

data_interp <- data.frame(Alter = interp$x, Anteil = interp$y)


Plot_alter <- data_interp %>%
  ggplot(aes(x = Alter, y = 1, height = Anteil, fill = after_stat(height))) +
  geom_density_ridges_gradient(stat = "identity", scale = 1, color = "gray35") +
  geom_hline(yintercept = Anteil_ges_egal, linetype = "dotted", 
             color = "gray15") +
  annotate("text",
           x=38.25, y=Anteil_ges_egal-10, label="Alter egal",
           family = "franklin", color = "gray25", fontface = "italic",
           size = 3.5) +
  geom_curve(aes(x    = 36.9,   y =    Anteil_ges_egal-10, 
                 xend = 36.25,   yend = Anteil_ges_egal-2.5),
             arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
             curvature = -0.5, color = "gray25", linewidth = 0.4) +
  scale_fill_gradientn(colours = c("gray70", "gray50", "gray30")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(18,40)) +
  labs(x = "gewünschtes Alter in Jahren", y = "Anteil in %") +
  theme_dunkel() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10.25),
        axis.text.x = element_text(size = 10.25),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        plot.margin = margin(t=50, l=50, b=30, r = 100))



## Abbildung Geschlecht --------------------------------------------------------


Daten_Staedte_geschlecht <- Daten_Staedte %>%
  group_by(geschlecht_ges) %>%
  count(geschlecht_ges, name = "Anzahl") %>%
  na.omit() %>%
  ungroup() %>%
  mutate(anteil = Anzahl / sum(Anzahl) *100,
         geschlecht_ges = factor(geschlecht_ges, 
                                 levels = c("Divers", "Mann", "Frau", 
                                            "Geschlecht egal"))) 


Daten_Staedte_geschlecht_text <- Daten_Staedte_geschlecht %>%
  mutate(x_pos = ifelse(geschlecht_ges %in% c("Mann", "Divers"), 
                        anteil/2+4, 2.5),
         label_1 = ifelse(geschlecht_ges %in% c("Mann", "Divers"), 
                          "", as.character(geschlecht_ges)),
         label_2 = ifelse(geschlecht_ges %in% c("Mann", "Divers"), 
                          glue("{geschlecht_ges} ({round(anteil,1)}%)"), glue("{round(anteil,1)}%")))


Plot_geschlecht <- Daten_Staedte_geschlecht %>%  
  ggplot(aes(y= geschlecht_ges, x= anteil)) +
  geom_col(fill = "gray65", color = "gray35", linewidth = 0.25) +
  geom_text(
    data = Daten_Staedte_geschlecht_text,
    aes(y=geschlecht_ges, x=x_pos, label = label_1), inherit.aes = F,
    hjust = 0, family = "domine", color = "gray25") +
  geom_text(
    data = Daten_Staedte_geschlecht_text,
    aes(y=geschlecht_ges, x=anteil+2, label = label_2), inherit.aes = F,
    hjust = 0, family = "domine", color = "gray75") +
  coord_cartesian(clip = "off") +
  labs(x = "Anteil in %", y = "gewünchtes Geschlecht") +
  theme_dunkel() +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t=30, l=50, b=50, r = 100),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10.25),
        axis.title.y = element_text(size = 12, face = "bold", vjust = -2),
        axis.title.x = element_text(size = 12, face = "bold"))



# Abbildung Waffleplot ---------------------------------------------------------


alter_geschlecht <- Daten_Staedte %>%
  select(alter_seq, frau_offen, mann_offen) %>%
  unnest(alter_seq) %>%
  rename(Alter = alter_seq) %>%
  filter(Alter >= 18, Alter <= 40) %>%
  group_by(Alter) %>%
  summarise(
    n_inserate = n(),
    anteil_alter = n_inserate / nrow(Daten_Staedte) * 100,
    frau_anteil = mean(frau_offen) * 100,
    mann_anteil = mean(mann_offen) * 100,
    alter_frau_anteil = sum(frau_offen) / nrow(Daten_Staedte) * 100,
    alter_mann_anteil = sum(mann_offen) / nrow(Daten_Staedte) * 100,
    .groups = "drop"
  ) %>%
  mutate(diff = alter_frau_anteil - alter_mann_anteil)


data_label <- alter_geschlecht %>%
  filter(Alter == max(Alter)) %>%
  select(Alter, alter_frau_anteil, alter_mann_anteil) %>%
  pivot_longer(cols = c(alter_frau_anteil, alter_mann_anteil),
               names_to = "geschlecht", values_to = "anteil") %>%
  mutate(geschlecht = recode(geschlecht,
                             alter_frau_anteil = "Frau",
                             alter_mann_anteil = "Mann"),
         hjust = ifelse(geschlecht == "Frau", 0, 1))


plot_alter_geschlecht <- alter_geschlecht %>%
  ggplot(aes(x = Alter)) +
  stat_difference(aes(ymin = 0, ymax = alter_mann_anteil),
                  alpha = 0.15, fill = "gray80") +
  stat_difference(aes(ymin = alter_mann_anteil, ymax = alter_frau_anteil),
                  alpha = 0.3, fill = "#F2A65A") +
  geom_textline(aes(y = alter_frau_anteil, color = "Frau"), linewidth = 1,
                lineend = "round", label = "Frau", vjust = 1.15, hjust = 0.75,
                gap = FALSE, size = 5.5) +
  geom_textline(aes(y = alter_mann_anteil, color = "Mann"), linewidth = 1,
                lineend = "round", label = "Mann",  vjust = 0.05, hjust = 0.19,
                gap = FALSE, size = 5.5) +
  # geom_text(
  #   data = data_label,
  #   aes(x = Alter, y = anteil, label = geschlecht, color = geschlecht,
  #       hjust = hjust), vjust = -0.75, family = "domine", size = 3.25
  #   ) +
  scale_y_continuous(limits = c(0,100),
                     labels = ~paste0(.x, "%")) +
  scale_color_manual(values = c(
    "Frau" = "#F2A65A",
    "Mann" = "#5FA8D3"
  )) +
  labs(y = "WGs mit passendem Alters- und Geschlechtsprofil",
       x = "Alter der bewerbenden Person (Jahre)",
  ) +
  theme_dunkel() +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(t=82, l=-5, b=50, r = 43),
        axis.text.y = element_text(size = 10.25),
        axis.text.x = element_text(size = 10.25),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold")) +
  coord_flip(clip = "off")


## Abbildungen zusammenfügen ---------------------------------------------------


Plot_links  <- Plot_alter / Plot_geschlecht

Plot_gesamt <- (Plot_links | plot_alter_geschlecht) +
  plot_layout(widths = c(1.25, 1)) &
  plot_annotation(theme = theme(
    plot.background  = element_rect(fill = panel_background_color, 
                                    color = "transparent"),
    panel.background = element_rect(fill = panel_background_color, 
                                    color = "transparent"),
    plot.margin = margin(t=25,b=25,r=25,l=25)
  ))


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/AltGes_Hamburg_7.png"
ggsave(filename = file_save, plot = Plot_gesamt, 
       width = 16, height = 9, units = "in", dpi = 300)

system(paste("xdg-open", shQuote(file_save)))



