


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          SCRAPING-EINZUG-PLOT       #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(ggnewscale)
library(patchwork)
library(glue)
library(ggtext)
library(showtext)
library(DBI)

font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()

stadt_filter <- "Berlin"



## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql <- glue_sql("
  SELECT stadt, datum_scraping, einzugsdatum
  FROM analysedaten
 ", .con = con_lokal)

Daten_Staedte <- dbGetQuery(con_lokal, sql) %>%
  mutate(jahr_scraping = format(datum_scraping, "%Y"),
         jahr_einzug = format(einzugsdatum, "%Y")) %>%
  filter(jahr_scraping == "2026" | jahr_einzug == "2026") 



## Daten aufbereiten -----------------------------------------------------------


data_scraping <- Daten_Staedte %>%
  filter(stadt == stadt_filter & jahr_scraping == "2026") %>%
  mutate(woche = as.numeric(format(datum_scraping, "%V")),
         tag = factor(format(datum_scraping, "%a"),
                      levels = c("So", "Sa", "Fr", "Do", "Mi", "Di", "Mo")),
         monat = format(datum_scraping, "%m"),
         tag_im_monat = as.numeric(format(datum_scraping, "%d"))) %>%
  group_by(woche, tag) %>%
  summarise(N = n(), 
            .groups = "drop") %>%
  mutate(erster_tag = case_when(
    woche == 1  & tag == "Do" ~ T, woche == 5  & tag == "So" ~ T,
    woche == 9  & tag == "So" ~ T, woche == 14 & tag == "Mi" ~ T,
    woche == 18 & tag == "Fr" ~ T, woche == 23 & tag == "Mo" ~ T,
    woche == 27 & tag == "Mi" ~ T, woche == 36 & tag == "Mo" ~ T,
    woche == 40 & tag == "Mi" ~ T, woche == 44 & tag == "Sa" ~ T,
    woche == 49 & tag == "Mo" ~ T, TRUE ~ F))

data_einzug <- Daten_Staedte %>%
  filter(stadt == stadt_filter & jahr_einzug == "2026") %>%
  filter(!is.na(einzugsdatum)) %>%
  mutate(woche = as.numeric(format(einzugsdatum, "%V")),
         tag = factor(format(einzugsdatum, "%a"),
                      levels = c("So", "Sa", "Fr", "Do", "Mi", "Di", "Mo")),
         monat = format(einzugsdatum, "%m")) %>%
  group_by(woche, tag) %>%
  summarise(N = n(), 
            .groups = "drop") %>%
  mutate(erster_tag = case_when(
    woche == 1  & tag == "Do" ~ T, woche == 5  & tag == "So" ~ T,
    woche == 9  & tag == "So" ~ T, woche == 14 & tag == "Mi" ~ T,
    woche == 18 & tag == "Fr" ~ T, woche == 23 & tag == "Mo" ~ T,
    woche == 27 & tag == "Mi" ~ T, woche == 36 & tag == "Mo" ~ T,
    woche == 40 & tag == "Mi" ~ T, woche == 44 & tag == "Sa" ~ T,
    woche == 49 & tag == "Mo" ~ T, TRUE ~ F))


woche_min <- min(c(data_scraping$woche, data_einzug$woche), na.rm = TRUE)
woche_max <- max(c(data_scraping$woche, data_einzug$woche), na.rm = TRUE)



## Labelwerte ------------------------------------------------------------------


median_stadt <- Daten_Staedte %>%
  filter(stadt == stadt_filter) %>%
  mutate(differenz = einzugsdatum-datum_scraping,
         differenz = as.numeric(differenz)) %>%
  summarise(median_differenz = median(differenz, na.rm = T)) %>%
  pull()

median_staedte <- Daten_Staedte %>%
  mutate(differenz = einzugsdatum-datum_scraping,
         differenz = as.numeric(differenz)) %>%
  group_by(stadt) %>%
  summarise(median_differenz = median(differenz, na.rm = T)) %>%
  ungroup() %>%
  summarise(median_differenz = median(median_differenz, na.rm = T)) %>%
  pull()

perc_sub7 <- Daten_Staedte %>%
  filter(stadt == stadt_filter) %>%
  mutate(differenz = einzugsdatum-datum_scraping,
         differenz = as.numeric(differenz),
         sub7 = differenz <= 7) %>%
  group_by(sub7) %>%
  summarise(anteil_sub7 = n()) %>%
  reframe(perc_sub7 = round(anteil_sub7/sum(anteil_sub7)*100,1)) 

## Plot erstellen --------------------------------------------------------------


plot_scraping <- data_scraping %>%
  ggplot(aes(x = woche, y = tag, fill = N)) +
  geom_tile(aes(color = erster_tag), width = 0.9, height = 0.9, linewidth = 0.3) +
  scale_fill_gradient(low = "#FEE0D2", high = "#DE2D26", name = "") +
  scale_x_continuous(breaks = seq(woche_min, woche_max, by = 1),
                     labels = seq(woche_min, woche_max, by = 1),
                     expand = c(0.025,0.025)) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "transparent")) +
  annotate("richtext", x = woche_max+5.75, y = "Di", 
           label = glue("Ø = {median(data_scraping$N)}"), family = "domine",
           hjust = 0.5, size = 2, fill = "white", color = "black",
           label.color = "gray15", label.padding = unit(c(0.25, 0.25, 0.15, 0.25), "lines")) +
  guides(color = "none") +
  coord_cartesian(xlim = c(woche_max, woche_min), expand = T, clip = "off") +
  labs(title = NULL,
       x = NULL, 
       y = "Scraping-Datum") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "#f8f3f2"),
        legend.position = "right",
        legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(0.35, "cm"),
        legend.text = element_text(family = "domine", size = 4.5),
        legend.margin = margin(t = 25, b = 0, l = 0, r = 0),
        axis.line.y = element_line(color = "gray20", linewidth = 0.25),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1, size = 4.5, family = "domine"),
        axis.title.y = element_text(size = 8, family = "domine", face = "bold",
                                    margin = margin(r=7.5)))

plot_einzug <- data_einzug %>%
  ggplot(aes(x = woche, y = tag, fill = N)) +
  geom_tile(aes(color = erster_tag), width = 0.9, height = 0.9, linewidth = 0.3) +
  scale_fill_gradient(low = "#DEEBF7", high = "#3182BD", name = "") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "transparent")) +
  scale_x_continuous(breaks = seq(woche_min, woche_max, by = 2),
                     labels = seq(woche_min, woche_max, by = 2),
                     expand = c(0.025,0.025)) +
  guides(color = "none") +
  annotate("richtext", x = woche_max+5.75, y = "Di", 
           label = glue("Max = {max(data_einzug$N)}"), family = "domine",
           hjust = 0.5, size = 2, fill = "white", color = "black",
           label.color = "gray15", label.padding = unit(c(0.25, 0.25, 0.15, 0.25), "lines")) +
  coord_cartesian(xlim = c(woche_max, woche_min), expand = T,  clip = "off") +
  labs(title = NULL,
       x = "Kalenderwochen", 
       y = "Einzugsdatum") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "#f8f3f2"),
        legend.position = "right",
        legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(0.35, "cm"),
        legend.text = element_text(family = "domine", size = 5),
        legend.margin = margin(t = 25, b = 0, l = 0, r = 0),
        axis.text.x = element_text(size = 5, family = "domine"),
        axis.line = element_line(color = "gray20", linewidth = 0.25),
        axis.text.y = element_text(hjust = 1, size = 4.5, family = "domine"),
        axis.title.y = element_text(size = 8, family = "domine", face = "bold",
                                    margin = margin(r=7.5)),
        axis.title.x = element_text(size = 7, family = "domine", face = "bold",
                                    margin = margin(t=7.5)))


plot_datum_vergleich <- plot_scraping / plot_einzug +
  plot_annotation(
    title = glue("Spontaner Einzug gefällig?"),
    subtitle = glue("In {stadt_filter} liegt die Differenz zwischen Upload und Einzugsdatum einer Anzeige auf WG-Gesucht bei durchschnittlich **{median_stadt}** Tagen (Durchschnitt aller Städte {median_staedte} Tage). **{perc_sub7[2,]}%** der Anzeigen werden 7 Tage oder weniger vor dem Einzugsdatum eingestellt."),
    caption = glue("Datengrundlage sind {Daten_Staedte %>% filter(stadt == stadt_filter) %>% nrow()} Anzeigen die zwischen dem {format(min(Daten_Staedte$datum_scraping), '%d.%m.%y')} und dem {format(max(Daten_Staedte$datum_scraping), '%d.%m.%y')} erhoben wurden"),
    theme = theme(
      plot.title = element_text(size = 12, face = "bold", family = "domine",
                                hjust = 0, margin = margin(b=7.5,t=5)),
      plot.subtitle = element_textbox_simple(size = 5.5, color = "grey20",
                                             family = "domine", lineheight = 1.25,
                                             margin = margin(b=5)),
      plot.caption = element_text(size = 4.5, face = "italic", family = "franklin",
                                  margin = margin(b=5,l=0,t=5, r=-10)),
      plot.background = element_rect(fill = "#f8f3f2"),
      plot.margin = margin(r=10)
    ))



file_save <- glue("Abbildungen/Scraping_Einzug/{stadt_filter}_Scraping_Einzug.png")
ggsave(filename = file_save, plot = plot_datum_vergleich, 
       width = 6, height = 3.75, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

