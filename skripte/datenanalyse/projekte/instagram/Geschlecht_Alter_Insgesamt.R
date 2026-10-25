


################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##           INSTAGRAM - Alter Geschlecht gesamt        ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


library(tidyverse)
library(glue)
library(DBI)
library(ggh4x)
library(geomtextpath)

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
  SELECT stadt, alter_ges, geschlecht_ges, gesamtmiete
  FROM analysedaten
  WHERE land = 'Deutschland'
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
", .con = con_lokal)


daten_staedte_roh <- dbGetQuery(con_lokal, sql) %>%
  group_by(stadt) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-gesamtmiete) %>%
  ungroup()



## Daten aufbereiten -----------------------------------------------------------


wg_aufbereitet <- daten_staedte_roh %>%
  mutate(
    alter_ges = ifelse(is.na(alter_ges), "16 und 99", alter_ges),
    alter_seq = map(alter_ges, ~ {
      teile <- str_extract_all(.x, "\\d+")[[1]] %>% as.integer()
      seq(teile[1], teile[2])
    }),
    frau_offen = geschlecht_ges %in% c("Geschlecht egal", "Frau"),
    mann_offen = geschlecht_ges %in% c("Geschlecht egal", "Mann")
  )


alter_geschlecht <- wg_aufbereitet %>%
  select(alter_seq, frau_offen, mann_offen) %>%
  unnest(alter_seq) %>%
  rename(Alter = alter_seq) %>%
  filter(Alter >= 18, Alter <= 40) %>%
  group_by(Alter) %>%
  summarise(
    n_inserate = n(),
    anteil_alter = n_inserate / nrow(wg_aufbereitet) * 100,
    frau_anteil = mean(frau_offen) * 100,
    mann_anteil = mean(mann_offen) * 100,
    alter_frau_anteil = sum(frau_offen) / nrow(wg_aufbereitet) * 100,
    alter_mann_anteil = sum(mann_offen) / nrow(wg_aufbereitet) * 100,
    .groups = "drop"
  ) %>%
  mutate(diff = alter_frau_anteil - alter_mann_anteil)
    


## Abbildung erstellen ---------------------------------------------------------


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
                  alpha = 0.3, fill = "#c8b078") +
  geom_textline(aes(y = alter_frau_anteil, color = "Frau"), linewidth = 1,
            lineend = "round", label = "Frau", vjust = 1.2, hjust = 0.602,
            gap = FALSE, size = 3.25) +
  geom_textline(aes(y = alter_mann_anteil, color = "Mann"), linewidth = 1,
            lineend = "round", label = "Mann",  vjust = 0.1, hjust = 0.19,
            gap = FALSE, size = 3.25) +
  annotate("text",
           x = 38.5, y = 67,
           label = "Geschlechter-\nunterschiede",
           color = "#c8b078", size = 2.75, hjust = 0, fontface = "italic",
           lineheight = 0.9, alpha = 0.6) +
  annotate("curve",
           x = 38.5, xend = 37, 
           y = 65, yend = 50.75,
           curvature = -0.3, arrow = arrow(length = unit(0.15, "cm")),
           color = "#c8b078", linewidth = 0.5, alpha = 0.6) +
  scale_y_continuous(limits = c(0,100),
                     labels = ~paste0(.x, "%")) +
  scale_color_manual(values = c(
    "Frau" = "#c8b078",
    "Mann" = "#8fa8c8"
  )) +
  labs(y = "Anteil der WGs mit passendem Alters-\nund Geschlechtsprofil",
       x = "Alter der bewerbenden Person",
       caption = "Abbildung: Fabian Hellmold/Datengeschichten",
       title = "Altersspezifische <b><span style='color:#c8b078'>Geschlechterunterschiede</span></b>. Wie<br>stark das Geschlecht zählt, ist eine Frage des <b><span style='color:gray90'>Alters</span></b>"
       ) +
  theme_dunkel() +
  theme(
    plot.margin = margin(t=10, l=27.5, b=5, r=27.5),
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size  = 8.75),
    axis.title.y = element_text(size  = 10),
    plot.title = element_markdown(margin = margin(b=15),
                                  size = 10.5)
  ) +
  coord_flip(clip = "off", xlim = c(18,40))




## Abbildung speichern ---------------------------------------------------------

file_save <- "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/instagram/Geschlecht_Alter_Insgesamt.png"
file_dropbox <- "/home/fabian/Dropbox/Abbildungen_Instagram/Abbildungen_Instagram_Fabi/Geschlecht_Alter_Insgesamt.png"

ggsave(filename = file_save, plot = plot_alter_geschlecht, 
       width = 4, height = 5, units = "in", dpi = 300)

add_logo_insta(
  image_path = file_save,
  opacity = 0.4,
  logo_path = "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/daten/logos/DG_Logo_schwarzweiß.png"
)

system(paste("xdg-open", shQuote(file_save)))

file_copy(file_save, file_dropbox, overwrite = TRUE)




