


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

stadt <- "Dresden"

grenzwert <- 380


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
  mutate(bereich = ifelse(x < grenzwert, "unter", "über"))

anteile <- WGdaten_trim %>%
  summarise(links = mean(gesamtmiete < grenzwert, na.rm = TRUE) * 100,
            rechts = mean(gesamtmiete >= grenzwert, na.rm = TRUE) * 100)



# Abbildung erstellen ----------------------------------------------------------

Abb_Dichte <- ggplot(dichte_df, aes(x = x, y = y, fill = bereich)) +
  geom_area(alpha = 0.6) +
  geom_vline(xintercept = grenzwert, linetype = "solid", color = "black", linewidth = 1) +
  scale_fill_manual(values = c("unter" = "steelblue", "über" = "tomato")) +
  labs(
    title = glue("Dichteverteilung in {stadt} unter bzw. über {grenzwert} €. Die Medianmiete liegt bei {median(WGdaten_trim$gesamtmiete)} €"),
    x = "Zimmermiete in €",
    y = "Dichte",
    fill = "Bereich"
  ) +
  annotate(
    "label",
    x = grenzwert - diff(range(WGdaten_trim$gesamtmiete)) * 0.15,
    y = max(dichte_df$y) * 0.8,
    label = paste0(round(anteile$links, 1), "%"),
    fill = "steelblue", color = "gray95", alpha = 0.8, size = 4,
    hjust = 0
  ) +
  annotate(
    "label",
    x = grenzwert + diff(range(WGdaten_trim$gesamtmiete)) * 0.15,
    y = max(dichte_df$y) * 0.8,
    label = paste0(round(anteile$rechts, 1), "%"),
    fill = "tomato", color = "gray95", alpha = 0.8, size = 4,
    hjust = 1
  ) +
  theme_minimal()


file_save <- glue("Abbildungen/Dichte_380/Dichte_380_{stadt}.png")
ggsave(filename = file_save, plot = Abb_Dichte, 
       width = 10, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

