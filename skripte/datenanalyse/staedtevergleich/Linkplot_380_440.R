

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######    Dichteverteilung 380 €-Grenze    #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(glue)
library(ggforce)
library(showtext)
library(ggtext)

font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()

grenzwert_1 <- 380
grenzwert_2 <- 440

farbe_unter <- "#9ecae1"
farbe_mitte <- "#2171b5"

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
    AND land = 'Deutschland'
", .con = con_lokal)

WGdaten_ges <- dbGetQuery(con_lokal, sql) %>%
  select(-land)

WGdaten_trim <- WGdaten_ges %>%
  mutate(stadt = case_when(
    stadt == "Freiburg im Breisgau" ~ "Freiburg i.B.",
    stadt == "Frankfurt am Main" ~ "Frankfurt a.M.",
    TRUE ~ stadt)) %>%
  group_by(stadt) %>%
  filter(
    gesamtmiete > quantile(gesamtmiete, 0.01, na.rm = TRUE),
    gesamtmiete < quantile(gesamtmiete, 0.99, na.rm = TRUE)
  )

median_mieten <- WGdaten_trim %>%
  group_by(stadt) %>%
  summarise(median_miete = median(gesamtmiete, na.rm = TRUE)) %>%
  arrange(median_miete)

margins <- WGdaten_trim %>%
  group_by(stadt) %>%
  summarise(
    anteil_380 = mean(gesamtmiete < 380, na.rm = TRUE) * 100,
    anteil_400 = mean(gesamtmiete < 440, na.rm = TRUE) * 100
  ) %>%
  mutate(differenz = round(anteil_400 - anteil_380, 1)) %>%
  ungroup() %>%
  arrange(anteil_400) %>% 
  mutate(stadt = factor(stadt, levels = median_mieten$stadt))



margins %>%
  pivot_longer(cols = c(anteil_380, anteil_400)) %>%
  ggplot(aes(x = value, y = stadt, colour = name, fill = name)) +
  geom_link(
    data = margins,
    mapping = aes(
      x = anteil_380, xend = anteil_400, 
      y = stadt, yend = stadt, 
      colour = after_stat(index)
    ),
    inherit.aes = FALSE, linewidth = 3.25, n = 1000
  ) +
  geom_point(shape = 21, size = 3.5, colour = "gray") +
  geom_text(
    data = margins,
    aes(x = anteil_400 + 3, y = stadt, label = glue("+{differenz}%")),
    inherit.aes = FALSE, vjust = 0.5, hjust = 0, family = "franklin",
    size = 2.25
  ) +
  scale_x_continuous(limits = c(0, 105)) +
  scale_color_gradient(low = farbe_unter,
                       high = farbe_mitte) +
  scale_fill_manual(breaks = c("anteil_380", "anteil_400"),
                     values = c(farbe_unter, farbe_mitte)) +
  labs(x = "Anteil (%)", y = NULL, title = "Vergleich Pauschale 380 vs 440") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(family = "domine", size = 14, margin = margin(b=10)),
    plot.title.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    axis.text = element_markdown(family = "franklin", size = 8),
    axis.text.y = element_markdown(margin = margin(l = 4.5)),
    axis.title.x = element_markdown(family = "domine", size = 10, 
                                  margin = margin(t=10)),
    axis.line = element_line(color = "gray35")
    )



file_save <- "Abbildungen/Linkplot_380_440.png"
ggsave(filename = file_save, plot = last_plot(), 
       width = 6, height = 9, units = "in", dpi = 300)
shell.exec(normalizePath(file_save))
