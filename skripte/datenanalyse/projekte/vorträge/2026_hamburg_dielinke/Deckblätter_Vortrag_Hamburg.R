


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          DECKBLÄTTER VORTRAG        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(patchwork)
library(ggtext)

source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")


## Politikfeld -----------------------------------------------------------------


Plot_politikfeld <- ggplot() +
  # Hauptüberschrift
  geom_richtext(aes(x = 0.01, y = 0.925, label = 'Politikfeld "Junges Wohnen"'), 
                size = 14, family = plot_title_family, color = plot_title_color,
                fontface = "bold", hjust = 0.0,
                label.colour = panel_background_color, fill = panel_background_color) +
  # Kapitelpunkte
  geom_text(aes(x = 0.04, y = 0.75, label = "Bezahlbarer Wohnraum ist eine der\ngroßen sozialen Fragen unserer Zeit"), 
            size = 9, family = axis_title_family, color = axis_title_color,
            hjust = 0, lineheight= 0.95) +
  geom_text(aes(x = 0.04, y = 0.575, label = "Weil sich junge Menschen meist in\nAusbildung befinden, sind sie von\nsteigenden Mieten besonders betroffen"), 
            size = 9, family = axis_title_family, color = axis_title_color,
            hjust = 0, lineheight= 0.95) +
  geom_richtext(aes(x = 0.04, y = 0.4, label = "Junges Wohnen ist somit auch eine<br>Frage der **Bildungsgerechtigkeit**"), 
                size = 9, family = axis_title_family, color = axis_title_color,
                hjust = 0,  lineheight= 1.15,
                label.colour = panel_background_color, fill = panel_background_color) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/2_1_Politikfeld.png"
ggsave(filename = file_save, plot = Plot_politikfeld, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))


## Vermittlung WGs 1 -----------------------------------------------------------


Plot_WGsuche_damals <- ggplot() +
  geom_text(aes(x = 0.02, y = 0.95, label = "Vermittlung von WGs - damals"), 
            size = 14, family = plot_title_family, color = plot_title_color,
            fontface = "bold", hjust = 0) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/2_2_WGsuche_damals.png"
ggsave(filename = file_save, plot = Plot_WGsuche_damals, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))



## Vermittlung WGs 2 -----------------------------------------------------------


Plot_WGsuche_heute <- ggplot() +
  geom_text(aes(x = 0.02, y = 0.95, label = "Vermittlung von WGs - heute"), 
            size = 14, family = plot_title_family, color = plot_title_color,
            fontface = "bold", hjust = 0) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/2_3_WGsuche_heute.png"
ggsave(filename = file_save, plot = Plot_WGsuche_heute, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))



## Methodik --------------------------------------------------------------------


Plot_methodik <- ggplot() +
  # Hauptüberschrift
  geom_text(aes(x = 0.02, y = 0.925, label = "Methodische Hintergründe"), 
            size = 14, family = plot_title_family, color = plot_title_color,
            fontface = "bold", hjust = 0.0) +
  # Kapitelpunkte
  geom_text(aes(x = 0.05, y = 0.775, label = "Erheben und Auswerten von Zimmer-\nangeboten auf WG-Gesucht"), 
            size = 9, family = axis_title_family, color = axis_title_color,
            hjust = 0, lineheight= 0.95) +
  geom_text(aes(x = 0.05, y = 0.625, label = "Webscraping: computergestütztes\nAuslesen von Webseiteninhalten"), 
            size = 9, family = axis_title_family, color = axis_title_color,
            hjust = 0, lineheight= 0.95) +
  geom_text(aes(x = 0.05, y = 0.475, label = "Aufbereiten und Speichern der Daten\nin einer Datenbank (ETL-Prozess)"), 
            size = 9, family = axis_title_family, color = axis_title_color,
            hjust = 0,  lineheight= 0.95) +
  geom_curve(aes(x    = 0.035,   y =    0.625,
                 xend = 0.01,   yend = 0.325),
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             curvature = 0.4, color = "gray65", linewidth = 0.75) +
  geom_curve(aes(x    = 0.035,   y =    0.475,
                 xend = 0.01,   yend = 0.325),
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             curvature = 0.4, color = "gray65", linewidth = 0.75) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/2_4_Methodik.png"
ggsave(filename = file_save, plot = Plot_methodik, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))


## Gliederung ------------------------------------------------------------------


set.seed(121)

kapitel_daten <- data.frame(
  kategorie = rep(c("A", "B"), each = 200),
  wert = c(rnorm(200, 350, 80),
           rnorm(200, 450, 90)))


plot_visual <- ggplot(kapitel_daten, 
                      aes(x = kategorie, 
                          y = wert,
                          fill = kategorie)) +
  geom_jitter(size = 2.5, show.legend = FALSE, 
              width = 0.25, alpha = 0.4, shape = 21, color = "gray15") +
  geom_boxplot(outlier.shape = NA, coef = 0,
               alpha = 0.5, show.legend = FALSE, width = 0.4,
               color = plot_title_color) +
  scale_fill_manual(values = c("B" = "#4A90E2", "A" = "#2E5A8E")) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color, color = NA),
    plot.margin = margin(40, 10, 40, 5)
  )


Plot_text <- ggplot() +
  # Hauptüberschrift
  geom_text(aes(x = 0.025, y = 0.85, label = "Wegmarken unserer Datenreise"), 
            size = 16, family = plot_title_family, color = plot_title_color,
            fontface = "bold", hjust = 0.0) +
  # Kapitelpunkte
  geom_text(aes(x = 0.05, y = 0.65, label = "- Hotspots und leere Flecken im Stadtgebiet"), 
            size = 11, family = axis_title_family, color = axis_title_color,
            hjust = 0) +
  geom_text(aes(x = 0.05, y = 0.5, label = "- Die Suche nach idealen Mitbewohner*innen"), 
            size = 11, family = axis_title_family, color = axis_title_color,
            hjust = 0) +
  geom_text(aes(x = 0.05, y = 0.35, label = "- Was kostet gemeinschaftliches Wohnen?"), 
            size = 11, family = axis_title_family, color = axis_title_color,
            hjust = 0) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))


Plot_gliederung <- (Plot_text + plot_visual) + 
  plot_layout(widths = c(3, 0.8)) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = panel_background_color,
                                   colour = panel_background_color),
    panel.background = element_rect(fill = panel_background_color,
                                    colour = panel_background_color)
  ))


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/2_5_Gliederung.png"
ggsave(filename = file_save, plot = Plot_gliederung, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))



## Deckblatt - Karte -----------------------------------------------------------


Plot_text_1 <- ggplot() +
  geom_text(aes(x = 1, y = 1.1, label = "Hotspots und leere Flecken im Stadtgebiet"), 
            size = 14, family = plot_title_family, color = plot_title_color,
            fontface = "bold") +
  geom_text(aes(x = 1, y = 0.9, label = "Die gefragtesten Orte für Hamburgs Wohngemeinschaften"), 
            size = 9, family = axis_title_family, color = plot_title_color) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0.5, 1.5), ylim = c(0.5, 1.5))


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/3_1_Deckblatt_Karte.png"
ggsave(filename = file_save, plot = Plot_text_1, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))



## Deckblatt - Geschlecht/Alter ------------------------------------------------


Plot_text_2 <- ggplot() +
  geom_text(aes(x = 1, y = 1.1, label = "Die Suche nach idealen Mitbewohner*innen"), 
            size = 14, family = plot_title_family, color = plot_title_color,
            fontface = "bold") +
  geom_text(aes(x = 1, y = 0.9, label = "Wie Alter und Geschlecht den Pfad zur WG ebnen oder versperren"), 
            size = 9, family = axis_title_family, color = plot_title_color) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0.5, 1.5), ylim = c(0.5, 1.5))


file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/3_2_Deckblatt_Geschlecht_Alter.png"
ggsave(filename = file_save, plot = Plot_text_2, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))



## Deckblatt - Zimmerkosten ----------------------------------------------------


Plot_text_3 <- ggplot() +
  geom_text(aes(x = 1, y = 1.1, label = "Was kostet gemeinschaftliches Wohnen?"), 
            size = 14, family = plot_title_family, color = plot_title_color,
            fontface = "bold") +
  geom_text(aes(x = 1, y = 0.9, label = "Das WG-Angebot zwischen bezahlbar und belastend"), 
            size = 9, family = axis_title_family, color = plot_title_color) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0.5, 1.5), ylim = c(0.5, 1.5))

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/3_3_Deckblatt_Zimmerkosten.png"
ggsave(filename = file_save, plot = Plot_text_3, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))




## Verabschiedung --------------------------------------------------------------


Plot_verabschiedung <- ggplot() +
  # Hauptüberschrift
  geom_text(aes(x = 0.02, y = 0.9, label = "Vielen Dank für eure Aufmerksamkeit!"), 
            size = 14, family = plot_title_family, color = plot_title_color,
            fontface = "bold", hjust = 0.0) +
  # Kapitelpunkte
  geom_text(aes(x = 0.03, y = 0.65, label = "Hier könnt ihr uns erreichen:"), 
            size = 12, family = axis_title_family, color = axis_title_color,
            hjust = 0) +
  geom_text(aes(x = 0.05, y = 0.55, label = "- info@daten-geschichten.de"), 
            size = 11, family = axis_title_family, color = axis_title_color,
            hjust = 0) +
  geom_text(aes(x = 0.05, y = 0.45, label = "- fabian.hellmold@posteo.de"), 
            size = 11, family = axis_title_family, color = axis_title_color,
            hjust = 0) +
  geom_text(aes(x = 0.05, y = 0.35, label = "- Instagram"), 
            size = 11, family = axis_title_family, color = "gray55",
            hjust = 0) +
  geom_curve(aes(x    = 0.225,   y =    0.32, 
                 xend = 0.7,   yend = 0.2),
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             curvature = 0.3, color = "gray65", linewidth = 0.75) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = panel_background_color)
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/vorträge/2026_hamburg_dielinke/4_Verabschiedung.png"
ggsave(filename = file_save, plot = Plot_verabschiedung, 
       width = 16, height = 9, units = "in", dpi = 300)
system(paste("xdg-open", shQuote(file_save)))

