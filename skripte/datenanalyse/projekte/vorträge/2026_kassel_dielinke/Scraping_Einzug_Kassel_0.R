


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


source("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Skripte\\Sonstiges\\laden_Fonts.R")

source("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Skripte\\Sonstiges\\Theme_dunkel.R")


stadt_filter <- "Kassel"

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")


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
   WHERE stadt = {stadt_filter}
 ", .con = con_lokal)

Daten_Staedte_roh <- dbGetQuery(con_lokal, sql) 

Daten_Staedte <- Daten_Staedte_roh %>%
  mutate(datum_scraping = as.Date(datum_scraping)) %>%
  filter((datum_scraping >= datum_von & datum_scraping <= datum_bis) | (einzugsdatum >= datum_von & einzugsdatum <= datum_bis))



## Daten aufbereiten -----------------------------------------------------------


data_scraping <- Daten_Staedte %>%
  filter(!is.na(datum_scraping)) %>%
  filter(datum_scraping >= datum_von & datum_scraping <= datum_bis) %>%
  mutate(woche = as.numeric(format(datum_scraping, "%V")),
         order_woche = factor(woche, levels = c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
                                                0, 1, 2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14)),
         tag = factor(format(datum_scraping, "%a"),
                      levels = c("So", "Sa", "Fr", "Do", "Mi", "Di", "Mo"))) %>%
  group_by(order_woche, tag) %>%
  summarise(N = n(), 
            .groups = "drop") %>%
  complete(order_woche, tag, fill = list(N = NA)) %>%
  mutate(erster_tag = case_when(
    order_woche == 1  & tag == "Do" ~ T, order_woche == 5  & tag == "So" ~ T,
    order_woche == 9  & tag == "So" ~ T, 
    order_woche == 40 & tag == "Mi" ~ T, order_woche == 44 & tag == "Sa" ~ T,
    order_woche == 49 & tag == "Mo" ~ T, TRUE ~ F))

data_einzug <- Daten_Staedte %>%
  filter(!is.na(einzugsdatum)) %>%
  filter(einzugsdatum >= datum_von & einzugsdatum <= datum_bis) %>%
  mutate(woche = as.numeric(format(einzugsdatum, "%V")),
         order_woche = factor(woche, levels = c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
                                                0, 1, 2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14)),
         tag = factor(format(einzugsdatum, "%a"),
                      levels = c("So", "Sa", "Fr", "Do", "Mi", "Di", "Mo")),
         monat = format(einzugsdatum, "%m")) %>%
  group_by(order_woche, tag) %>%
  summarise(N = n(), 
            .groups = "drop") %>%
  complete(order_woche, tag, fill = list(N = NA)) %>%
  mutate(erster_tag = case_when(
    order_woche == 1  & tag == "Do" ~ T, order_woche == 5  & tag == "So" ~ T,
    order_woche == 9  & tag == "So" ~ T, 
    order_woche == 40 & tag == "Mi" ~ T, order_woche == 44 & tag == "Sa" ~ T,
    order_woche == 49 & tag == "Mo" ~ T, TRUE ~ F))



## Plot erstellen --------------------------------------------------------------


plot_scraping <- data_scraping %>%
  ggplot(aes(x = order_woche, y = tag, fill = N)) +
  geom_tile(aes(color = erster_tag), width = 0.9, height = 0.9, linewidth = 0.5) +
  geom_vline(xintercept = "0", color = "gray15", linewidth = 1) +
  geom_label(x="0", y= "Do", label = "Jahreswechsel", angle = 90, size = 5, 
             color = "gray25", fill = "#1c202a", border.color =  "#1c202a",
             label.padding = unit(0.33, "lines"), family = "domine", fontface = "bold") +
  scale_fill_gradient(low = "#1c202a", high =  "#1c202a", name = "",
                      na.value = "transparent") +
  scale_color_manual(values = c("TRUE" =  "#1c202a", "FALSE" = "transparent")) +
  scale_x_discrete(
    labels = c("40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52",
               "", "1", "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "10", "11", "12", "13", "14", "15"),
    expand = c(0.025,0.025)
  ) +
  guides(color = "none") +
  coord_cartesian(expand = T,  clip = "off") +
  labs(title = NULL,
       x = "Kalenderwochen", 
       y = "Erhebungstag") +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = background_color, 
                                       color = background_color),
        panel.background = element_rect(fill = background_color),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "right",
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(family = legend_text_family, size = 10, 
                                   color = background_color, margin = margin(l=7.5)),
        legend.ticks = element_blank(),
        legend.margin = margin(t = 0, b = 0, l = 30, r = 0),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, family = axis_text_family,
                                   color = axis_text_color, margin = margin(t=7.5)),
        axis.line = element_line(color = axis_line_color, linewidth = 0.25),
        axis.text.y = element_text(hjust = 1, size = 11, family = axis_text_family,
                                   color = axis_text_color,  margin = margin(r=7.5)),
        axis.title.y = element_text(size = 17, family = axis_title_family, face = "bold",
                                    margin = margin(r=20, l=40), color = axis_title_color),
        axis.title.x = element_text(size = 16, family = axis_title_family, face = "bold",
                                    margin = margin(t=15), color = axis_title_color))


plot_einzug <- data_einzug %>%
  ggplot(aes(x = order_woche, y = tag, fill = N)) +
  geom_tile(aes(color = erster_tag), width = 0.9, height = 0.9, linewidth = 0.5) +
  geom_vline(xintercept = "0", color =  "#1c202a", linewidth = 1) +
  scale_fill_gradient(low =  "#1c202a", high =  "#1c202a", name = "",
                      na.value = "transparent") +
  scale_color_manual(values = c("TRUE" =  "#1c202a", "FALSE" = "transparent")) +
  scale_x_discrete(
    labels = c("40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52",
               "", "1", "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "10", "11", "12", "13", "14", "15"),
    expand = c(0.025,0.025)
  ) +
  guides(color = "none") +
  coord_cartesian(expand = T,  clip = "off") +
  labs(title = NULL,
       x = NULL, 
       y = "Einzugstag") +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = background_color, 
                                       color = background_color),
        panel.background = element_rect(fill = "transparent"),
        legend.position = "right",
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(family = legend_text_family, size = 10, 
                                   color = background_color, margin = margin(l=7.5)),
        legend.ticks = element_blank(),
        legend.margin = margin(t = 0, b = 0, l = 30, r = 0),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_blank(),
        axis.line.y = element_line(color = background_color, linewidth = 0.25),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1, size = 11, family = axis_text_family,
                                   color = background_color,  margin = margin(r=7.5)),
        axis.title.y = element_text(size = 17, family = axis_title_family, face = "bold",
                                    margin = margin(r=20, l=40), color = background_color))



plot_datum_vergleich <- plot_einzug / plot_scraping +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = background_color, 
                                     color = background_color),
      plot.margin = margin(t=75, l=75, b=75, r = 75)
    ))



file_save <- "Abbildungen/1_Präsentation_Linke_Kassel/Scraping_Einzug_Kassel_0.png"
ggsave(filename = file_save, plot = plot_datum_vergleich, 
       width = 16, height = 9, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))



## Kontextdaten für Präsentation -----------------------------------------------


median_stadt <- Daten_Staedte %>%
  filter(stadt == stadt_filter) %>%
  mutate(differenz = einzugsdatum-datum_scraping,
         differenz = as.numeric(differenz)) %>%
  summarise(median_differenz = median(differenz, na.rm = T)) %>%
  pull()

print(glue("Durchschnitt Differenz Upload/Einzugsdatum: {median_stadt}"))


perc_sub7 <- Daten_Staedte %>%
  filter(stadt == stadt_filter) %>%
  mutate(differenz = einzugsdatum-datum_scraping,
         differenz = as.numeric(differenz),
         sub7 = differenz <= 7) %>%
  group_by(sub7) %>%
  summarise(anteil_sub7 = n()) %>%
  reframe(perc_sub7 = round(anteil_sub7/sum(anteil_sub7)*100,1)) 

print(glue("Anteil Differenz 7 Tage oder weniger: {perc_sub7[2,]}%"))


erster_letzter <- Daten_Staedte %>%
  filter(!is.na(einzugsdatum)) %>%
  filter(einzugsdatum >= datum_von & einzugsdatum <= datum_bis) %>%
  mutate(erster_letzter = ifelse(einzugsdatum == "2025-10-01" | einzugsdatum == "2026-04-01", T,F)) %>%
  group_by(erster_letzter) %>%
  summarise(anteil_erster_letzter = n()) %>%
  reframe(perc__erster_letzter = round(anteil_erster_letzter/sum(anteil_erster_letzter)*100,1)) 

print(glue("Anteil Inserate an beiden Semesterstarts: {erster_letzter[2,]}%"))


erster_monat <- data_einzug %>%
  group_by(erster_tag) %>%
  summarise(N_erster = sum(N, na.rm = T)) %>%
  reframe(perc_N_erster = round(N_erster/sum(N_erster)*100,1)) 

print(glue("Anteil Inserate an beiden erster Monat: {erster_monat[2,]}%"))

