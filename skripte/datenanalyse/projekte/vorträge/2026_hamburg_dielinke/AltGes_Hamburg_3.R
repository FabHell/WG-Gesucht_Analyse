


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ALT/GES Hamburg - 4        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(ggridges)
library(ggfx)
library(waffle)
library(patchwork)
library(glue)
library(ggtext)
library(showtext)
library(DBI)


source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

stadt <- "Hamburg"

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")

filter_alter <- 25



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
      AND link IN (
      SELECT link
      FROM analysedaten
      GROUP BY link
      HAVING COUNT(*) <= 2
    )
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
  }))



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
  with_outer_glow(
    annotate("rect",
             xmin = filter_alter-0.5, xmax = filter_alter+0.5,
             ymin = 0, ymax = 100,
             fill = "#2171b5", alpha = 0.15, color = "gray20"),
    colour = "gray75", sigma = 8, expand = 1) +
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
  with_outer_glow(
    geom_col(data = Daten_Staedte_geschlecht %>% 
               filter(geschlecht_ges %in% c("Geschlecht egal", "Frau")),
             fill = "#b8954e", color = "gray35", alpha = 0.25, linewidth = 0.25),
    colour = "#b8954e", sigma = 8, expand = 2) +
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



Daten_waffle_w <- Daten_Staedte %>%
  mutate(gruppe_personen = ifelse(
    geschlecht_ges %in% c("Geschlecht egal", "Frau") & map_lgl(alter_seq, ~ filter_alter %in% .x), 
    "zielgruppe", "andere")) %>%
  group_by(gruppe_personen) %>%
  count() %>%
  ungroup() %>%
  mutate(n_scaled = round(n / sum(n) * 100) -100,
         n_scaled = n_scaled *(-1))



Plot_waffle_w <- Daten_waffle_w %>%
  ggplot(aes(fill = gruppe_personen, values = n_scaled)) +
  with_outer_glow(
    geom_waffle(n_rows = 4, size = 0.5, colour = panel_background_color, flip = T),
    colour = "gray40", sigma = 8, expand = 2
  ) +
  scale_fill_manual(
    values = c("zielgruppe" = "gray70", "andere" = "#3f74a6")
  ) +
  labs(
    title = glue("Frau, {filter_alter} Jahre"),
    subtitle = glue("{Daten_waffle_w[1,3]}%")
  ) +
  coord_cartesian(expand = F, clip = "off") +
  theme_dunkel() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        plot.margin = margin(t=0, l=33, b= 0, r = 50),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(family = "domine", margin = margin(b=-17.5), size = 12,
                                      vjust = 0, hjust = 0,  color = "gray80"),
        plot.subtitle = element_text(family = "domine", size = 22, hjust = 1,
                                     face = "bold", color = "gray90")
  )


Daten_waffle_m <- Daten_Staedte %>%
  mutate(gruppe_personen = ifelse(
    geschlecht_ges %in% c("Geschlecht egal", "Mann") & map_lgl(alter_seq, ~ filter_alter %in% .x), 
    "zielgruppe", "andere")) %>%
  group_by(gruppe_personen) %>%
  count() %>%
  ungroup() %>%
  mutate(n_scaled = round(n / sum(n) * 100) -100,
         n_scaled = n_scaled *(-1))

Plot_waffle_m <- Daten_waffle_m %>%
  ggplot(aes(fill = gruppe_personen, values = n_scaled)) +
  with_outer_glow(
    geom_waffle(n_rows = 4, size = 0.5, colour = panel_background_color, flip = T),
    colour = panel_background_color, sigma = 8, expand = 2
  ) +
  scale_fill_manual(
    values = c("zielgruppe" = panel_background_color, 
               "andere" = panel_background_color)
  ) +
  labs(
    title = glue("Mann, {filter_alter} Jahre"),
    subtitle = glue("{Daten_waffle_m[1,3]}%")
  ) +
  coord_cartesian(expand = F, clip = "off") +
  theme_dunkel() +
  theme(legend.position = "none",
        axis.line = element_blank(),
        plot.margin = margin(t=0, l=33, b= 0, r = 50),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(family = "domine", margin = margin(b=-17.5), size = 12,
                                      vjust = 0, hjust = 0,  color = panel_background_color),
        plot.subtitle = element_text(family = "domine", size = 22, hjust = 1,
                                     face = "bold", color = panel_background_color)
  )




## Abbildungen zusammenfügen ---------------------------------------------------


Plot_links  <- Plot_alter / Plot_geschlecht
Plot_rechts <- Plot_waffle_w | Plot_waffle_m

Plot_gesamt <- (Plot_links | Plot_rechts) +
  plot_layout(widths = c(1.25, 1)) &
  plot_annotation(theme = theme(
    plot.background  = element_rect(fill = panel_background_color, 
                                    color = "transparent"),
    panel.background = element_rect(fill = panel_background_color, 
                                    color = "transparent"),
    plot.margin = margin(t=25,b=25,r=25,l=25)
  ))


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/AltGes_Hamburg_3.png"
ggsave(filename = file_save, plot = Plot_gesamt, device = ragg::agg_png,
       width = 16, height = 9, units = "in", dpi = 300)

system(paste("xdg-open", shQuote(file_save)))



