



################################################################################
#####   ################################################################   #####
###       ####                                                    ####       ###
##         ##            INSTAGRAM - Alter 18-40 Jährige           ##         ##
###       ####                                                    ####       ###
#####   ################################################################   #####
################################################################################


library(tidyverse)
library(ggridges)
library(DBI)
library(glue)


source("C:/Users/hellm/Desktop/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")


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
  SELECT stadt, alter_ges, gesamtmiete
  FROM analysedaten
   WHERE land = 'Deutschland'
     AND datum_scraping >= {datum_von}
     AND datum_scraping <= {datum_bis}
     AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
 ", .con = con_lokal)

daten_staedte_roh <- dbGetQuery(con_lokal, sql) %>%
  filter(!is.na(gesamtmiete)) %>%
  group_by(stadt) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.025, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.975, na.rm = TRUE)) %>%
  ungroup() %>% select(-gesamtmiete)



## Daten aufbereiten -----------------------------------------------------------


daten_staedte <- daten_staedte_roh %>%
  mutate(alter_ges = ifelse(is.na(alter_ges), "16 und 99", alter_ges)) %>%
  mutate(alter_seq = map(alter_ges, ~ {
    teile <- str_extract_all(.x, "\\d+")[[1]] %>% as.integer()
    seq(teile[1], teile[2])
  }))



## Abbildung Alter -------------------------------------------------------------


anteil_ges_egal <- nrow(subset(daten_staedte_roh, is.na(alter_ges))) / nrow(daten_staedte_roh) * 100


daten_staedte_alter <- daten_staedte %>%
  unnest(alter_seq) %>%
  count(alter_seq, name = "haeufigkeit") %>%
  filter(alter_seq >= 18 & alter_seq <= 40) %>%
  mutate(anteil = haeufigkeit / nrow(daten_staedte) * 100) 


interp <- approx(
  x = daten_staedte_alter$alter_seq,
  y = daten_staedte_alter$anteil,
  xout = seq(min(daten_staedte_alter$alter_seq), max(daten_staedte_alter$alter_seq), length.out = 50000)
)

data_interp <- data.frame(Alter = interp$x, Anteil = interp$y)


plot_alter <- data_interp %>%
  ggplot() +
  geom_density_ridges_gradient(
     aes(x = Alter, y = 1, height = Anteil, fill = after_stat(height)),
     stat = "identity", scale = 1, color = "gray35") +
  geom_hline(yintercept = anteil_ges_egal, linetype = "dotted", 
             color = "gray15") +
  annotate("text",
           x=38.5, y=anteil_ges_egal-18.5, label="Alter egal",
           family = "franklin", color = "gray25", fontface = "bold",
           size = 2.25) +
  annotate(geom = "curve",
           x    = 38.1,   y    = anteil_ges_egal - 10, 
           xend = 37.2,   yend = anteil_ges_egal - 2,
           arrow = arrow(length = unit(0.08, "cm"), type = "closed"),
           curvature = -0.33, color = "gray25", linewidth = 0.33) +
  scale_fill_gradientn(colours = c("#6baed6", "#2171b5", "#08306b")) +
  scale_y_continuous(limits = c(0,100),
                     labels = ~paste0(.x, "%")) +
  scale_x_continuous(limits = c(18,40)) +
  labs(x = "Alter bei der Bewerbung (Jahre)", y = "Anteil der WGs mit passender Altersgrenze",
       caption = "Abbildung: Fabian Hellmold/Datengeschichten",
       title = "Zu jung oder zu alt? <b><span style='color:#2171b5'>Alter</span></b> als<br><b><span style='color:gray90'>Ausschlusskriterium</span></b> auf dem WG-Markt"
  ) +
  coord_flip() +
  theme_dunkel() +
  theme(legend.position = "none",
        plot.title = element_markdown(margin = margin(b=15)),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(size = 8.5))





## Abbildung lokal speichern ---------------------------------------------------

file_save_lokal <- "C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\abbildungen\\projekte\\instagram\\Plot_alter_18_40.png"
ggsave(filename = file_save_lokal, plot = plot_alter, 
       width = 4, height = 5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save_lokal))


## Abbildung in Dropbox speichern ----------------------------------------------

file_save_dropbox <- "C:\\Users\\hellm\\Dropbox\\Abbildungen_Instagram\\Plot_alter_18_40.png"

ggsave(filename = file_save_dropbox, plot = plot_alter, 
       width = 4, height = 5, units = "in", dpi = 300)  


  

