


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##               INSTAGRAM - Rauchen rund               ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


library(tidyverse)
library(glue)
library(DBI)

source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")


## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("DATABASE_PG"),
                       host = Sys.getenv("SERVER_PG"),
                       port = Sys.getenv("PORT_PG"),
                       user = Sys.getenv("UID_PG"),
                       password = Sys.getenv("PWD_PG"))

sql <- glue_sql("
  SELECT stadt, gesamtmiete, rauchen
  FROM analysedaten
  WHERE stadtteil_geocoding IS NOT NULL
    AND gesamtmiete IS NOT NULL
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND land = 'Deutschland'
", .con = con_lokal)


Daten_Staedte_roh <- dbGetQuery(con_lokal, sql) %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.01),
    quantile(gesamtmiete, 0.99)
    )) %>%
  select(-gesamtmiete) %>%
  na.omit()


## Daten aufbereiten -----------------------------------------------------------

farben_kategorien <- c(
  "nicht erwünscht" = "#E63946",
  "nur auf Balkon" = "#F4A261",
  "im Zimmer erlaubt" = "#E9C46A",
  "überall erlaubt" = "#4C956C"
)

Daten_Staedte <- Daten_Staedte_roh %>%
  count(stadt, rauchen, name = "N") %>%
  group_by(stadt) %>%
  mutate(anteil = N / sum(N) * 100,
         rauchen = str_remove(rauchen, "Rauchen "),
         rauchen = str_replace(rauchen, "auf dem Balkon erlaubt", "nur auf Balkon")) %>%
  select(-N) %>%
  ungroup() %>%
  mutate(
    rauchen_kategorie = factor(rauchen, levels = c(
     "überall erlaubt", "im Zimmer erlaubt", "nur auf Balkon", "nicht erwünscht")),
    color = farben_kategorien[as.character(rauchen_kategorie)]
  )

daten_mean <- Daten_Staedte %>%
  pivot_wider(id_cols = everything(), names_from = rauchen, 
              values_from = anteil) %>%
  summarise(nicht_mean = mean(`nicht erwünscht`, na.rm = T),
            balkon_mean = mean(`nur auf Balkon`, na.rm = T),
            zimmer_mean = mean(`im Zimmer erlaubt`, na.rm = T),
            ueberall_mean = mean(`überall erlaubt`, na.rm = T)) %>%
  pivot_longer(cols = everything(), names_to = "rauchen_kategorie",
               values_to = "anteil") %>%
  mutate(color_segment = case_when(
    rauchen_kategorie == "nicht_mean" ~ "#E63946",
    rauchen_kategorie == "balkon_mean" ~ "#F4A261",
    rauchen_kategorie == "zimmer_mean" ~ "#E9C46A",
    rauchen_kategorie == "ueberall_mean" ~ "#4C956C"
  ))



prozent_beschriftung <- tibble(
  prozent_position = c(0, 100 / 3, 66, 100),
  label = paste0(round(prozent_position, 0), "%"),
  vjust = c(2.25, 0.5, -1.25, 0.5),
  hjust = c(0.5, 1.5, 0.5, -0.25),
  color_label = c("gray50", "gray30", "gray30", "gray50")
)

daten_numerisch <- Daten_Staedte |>
  mutate(stadt_position = as.numeric(factor(stadt)))

breite_strich <- 0.35

länge_radius <- nrow(Daten_Staedte %>% select(stadt) %>% distinct())


Plot_rauchen <- ggplot(daten_numerisch) +
  geom_segment(
    data = distinct(daten_numerisch, stadt, stadt_position),
    aes(y = stadt_position, yend = stadt_position, x = 0, xend = 100),
    color = panel_grid_color,
    linewidth = 0.33) +
  geom_vline(xintercept = c(100 / 3, 100 / 1.5), color = panel_grid_color) +
  geom_vline(xintercept = c(0, 100), color = "gray50") +
  geom_segment(
    aes(y = stadt_position - breite_strich - 0.015,
        yend = stadt_position + breite_strich + 0.015,
        x = anteil, xend = anteil),
    linewidth = 2, color = "gray65") +
  geom_segment(
    data = daten_mean,
    aes(y = 0.5, yend = länge_radius+3.5,
        x = anteil, xend = anteil, colour = color_segment),
    linewidth = 0.75, alpha = 0.5, lineend = "round", show.legend = F) +
  geom_richtext(
    data = daten_mean,
    aes(x = anteil, y = länge_radius+c(8,9,6.25,6.25), 
        label = glue("{round(anteil,0)}%"), colour = color_segment), fill = "gray99",
    family = "domine", size = 3.25, label.padding = margin(2.5,2.5,1.25,2.5),
    show.legend = F) +
  geom_segment(
    aes(y = stadt_position - breite_strich, 
        yend = stadt_position + breite_strich,
        x = anteil, xend = anteil, color = color),
    linewidth = 1.5) +
  annotate("text",
           x = 0, y = länge_radius/2,
           label = "Anteile in Städten",
           angle = 90, vjust = -1.2, colour = "gray50", 
           size = 2.75, family = "domine") +
  geom_richtext(
    data = prozent_beschriftung,
    aes(x = prozent_position, y = 1,
        label = label, hjust = hjust, vjust = vjust, color = color_label),
    size = 2.25, label.colour = "transparent", fill = panel_background_color,
    family = "domine", label.padding = margin(2.5,1.5,1.5,1.5), 
    show.legend = FALSE) +
  labs(
    title = 'Verbot, Balkon oder alles erlaubt? Wie<br>schwer es <b><span style="color:gray90">Raucher</span></b> in <b><span style="color:gray90">WGs</span></b> haben',
    x = "Anteil (%)",
    caption = "Abbildung: Fabian Hellmold/Datengeschichten"
  ) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 1 / 3))) +
  scale_y_discrete(expand = expansion(mult = c(1 / 3, 0.05))) +
  scale_color_identity(
    guide = guide_legend(ncol = 1, reverse = T),
    breaks = farben_kategorien,
    labels = names(farben_kategorien),
    name = NULL
  ) +
  coord_radial(theta = "x", clip = "off", rlim = c(NA,49)) +
  theme_dunkel(gridline_x = F, gridline_y = F) +
  theme(
    plot.margin = margin(0, 0, 0, 10),
    plot.caption = element_text(margin = margin(15,5,-10,0)),
    plot.title = element_markdown(margin = margin(-5,0,10,0)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 6,
                               margin = margin(0,0,1,2.5)),
    legend.margin = margin(t=-20),
    legend.position = "inside",
    legend.position.inside = c(0.22, 0.57),
    legend.key.spacing.y = unit(2.75, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.key.width = unit(10, "pt")
  )


## Abbildung speichern ---------------------------------------------------------

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/instagram/Rauchen_rund.png"
file_dropbox <- "/home/fabian/Dropbox/Abbildungen_Instagram/Abbildungen_Instagram_Fabi/Rauchen_rund.png"

ggsave(filename = file_save, plot = Plot_rauchen,
       width = 4, height = 5, units = "in", dpi = 300)

add_logo_insta(
  image_path = file_save,
  opacity = 0.25,
  logo_path = "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/daten/logos/DG_Logo_schwarzweiß.png"
)

system(paste("xdg-open", shQuote(file_save)))

file_copy(file_save, file_dropbox, overwrite = TRUE)


