


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######    Dichteverteilung 380 €-Grenze    #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(ggtext)
library(DBI)
library(glue)



# Stadt festlegen --------------------------------------------------------------

stadt <- "Leipzig"

grenzwert_1 <- 380
grenzwert_2 <- 440

farbe_unter <- "#9ecae1"
farbe_mitte <- "#2171b5"
farbe_über <- "#ef8a62"

# Daten laden und trimmen ------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

sql <- glue_sql("
  SELECT gesamtmiete
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND stadt = {stadt}
 ", .con = con_lokal)

WGdaten_trim <- dbGetQuery(con_lokal, sql) %>%
  filter(gesamtmiete > quantile(gesamtmiete, 0.01, na.rm = TRUE),
         gesamtmiete < quantile(gesamtmiete, 0.99, na.rm = TRUE))



# Daten für Abbildung vorbereiten ----------------------------------------------

dichte_df <- with(WGdaten_trim, density(gesamtmiete, na.rm = TRUE)) %>%
  {data.frame(x = .$x, y = .$y)} %>%
  mutate(bereich = case_when(x < grenzwert_1 ~ "unter", 
                             x > grenzwert_1 & x < grenzwert_2 ~ "mitte",
                             x > grenzwert_2 ~ "über"),
         bereich = factor(bereich, levels = c("unter", "mitte", "über")))

anteile <- WGdaten_trim %>%
  summarise(links = mean(gesamtmiete <= grenzwert_1, na.rm = TRUE) * 100,
            mitte = mean(gesamtmiete > grenzwert_1 & gesamtmiete < grenzwert_2, na.rm = TRUE) * 100,
            rechts = mean(gesamtmiete >= grenzwert_2, na.rm = TRUE) * 100)



# Abbildung erstellen ----------------------------------------------------------

Abb_Dichte <- ggplot(dichte_df, aes(x = x, y = y, fill = bereich)) +
  geom_area(alpha = 0.6) +
  geom_vline(xintercept = grenzwert_1, linetype = "solid", color = "black", 
             linewidth = 1) +
  geom_vline(xintercept = grenzwert_2, linetype = "solid", color = "black", 
             linewidth = 1) +
  scale_fill_manual(values = c("unter" = farbe_unter, "mitte" = farbe_mitte, 
                               "über" = farbe_über)) +
  labs(
    title = glue("Einfluss der Erhöhung der Wohnkostenpauschale in {stadt} (1%-Trimmed)"),
    x = "Zimmermiete in €",
    y = "Dichte",
    fill = "Bereich"
  ) +
  annotate(
    "label",
    x = grenzwert_1 - diff(range(WGdaten_trim$gesamtmiete)) * 0.15,
    y = max(dichte_df$y) * 0.8, label = paste0(round(anteile$links, 1), "%"),
    fill = farbe_unter, color = "gray95", alpha = 0.8, size = 4, hjust = 0) +
  annotate(
    "label",
    x = (grenzwert_1 + grenzwert_2)/2 ,
    y = max(dichte_df$y) * 0.8, label = paste0(round(anteile$mitte, 1), "%"),
    fill = farbe_mitte, color = "gray95", alpha = 0.8, size = 4, hjust = 0.5) +
  annotate(
    "label",
    x = grenzwert_2 + diff(range(WGdaten_trim$gesamtmiete)) * 0.15,
    y = max(dichte_df$y) * 0.8, label = paste0(round(anteile$rechts, 1), "%"),
    fill = farbe_über, color = "gray95", alpha = 0.8, size = 4, hjust = 1) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.background = element_rect(fill = "gray99", color = "transparent")
  )


file_save <- glue("Abbildungen/Dichte_380_440/Dichte_380_440_{stadt}.png")
ggsave(filename = file_save, plot = Abb_Dichte, 
       width = 10, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

