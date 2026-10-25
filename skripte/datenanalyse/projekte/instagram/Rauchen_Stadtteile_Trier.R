


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ABB. POLYGON-RASTER        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


stadt <- "Trier"

primary_color <- "#4c956c"

datum_von <- as.Date("2025-10-01")
datum_bis <- as.Date("2026-03-31")


library(sf)
library(ggplot2)
library(dplyr)
library(glue)
library(DBI)
library(tidyverse)
library(ggfx)
library(ggtext)

source("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/skripte/hilfsfunktionen/hilfsfunktionen_design.R")


# Anfang -----------------------------------------------------------------------

con_lokal <- dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("DATABASE_PG"),
                       host = Sys.getenv("SERVER_PG"),
                       port = Sys.getenv("PORT_PG"),
                       user = Sys.getenv("UID_PG"),
                       password = Sys.getenv("PWD_PG"))


# WG-Daten ---------------------------------------------------------------------

query_wgdaten <- glue_sql("
  SELECT stadtteil_geocoding, rauchen
  FROM analysedaten
  WHERE stadt = {stadt}
    AND geolocation IS NOT NULL
    AND datum_scraping >= {datum_von}
    AND datum_scraping <= {datum_bis}
    AND link IN (
      SELECT link
      FROM analysedaten
      GROUP BY link
      HAVING COUNT(*) <= 2
    )
", .con = con_lokal)

stadteile_rauchen <- dbGetQuery(con_lokal, query_wgdaten) %>%
  na.omit() 

stadteile_rauchen_aufb <- stadteile_rauchen %>%
  count(stadtteil_geocoding, rauchen, name = "N") %>%
  group_by(stadtteil_geocoding) %>%
  mutate(anzahl_stadtteil = sum(N),
         perc = N/anzahl_stadtteil*100) %>%
  filter(anzahl_stadtteil >= 30 & rauchen == "Rauchen überall erlaubt") %>%
  arrange(perc) %>%
  rename(stadtteil = stadtteil_geocoding) %>%
  tail(1)


# Geodaten Stadtteilgrenzen ----------------------------------------------------

query <- glue_sql("
   SELECT *
     FROM geodaten_stadtteile
     WHERE stadt = {stadt}
   ", .con = con_lokal)

grenzen_stadtteile <- st_read(con_lokal, query = query) %>%
  st_transform(crs = 25832) 

grenzen_stadtteile_ana <- grenzen_stadtteile %>%
  left_join(stadteile_rauchen_aufb, by = "stadtteil") %>%
  filter(!is.na(perc))


# Abbildung erstellen ----------------------------------------------------------

zentrum_stadtteil <- grenzen_stadtteile_ana %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(x = X, y = Y)

infobox_label <- grenzen_stadtteile_ana %>%
  st_drop_geometry() %>%
  glue_data(
    "
    <span style='font-size:10pt;color:{primary_color};'><b>{stadtteil}</b></span><br>
    <span style='font-size:7pt;color:gray80;'>Rauchen überall erlaubt: </span><span style='font-size:8pt;color:{primary_color};'><b>{round(perc, 1)}%</b></span>
    "
  )


Karte_Rauchen <- ggplot() + 
  geom_sf(data = grenzen_stadtteile, linewidth = 0.75,
          color = "gray55", fill = "gray40") +
  geom_sf(data = grenzen_stadtteile, linewidth = 0.1,
          color = "gray45", fill = "gray35") +
  with_outer_glow(
    geom_sf(data = grenzen_stadtteile_ana, linewidth = 0.5,
            fill = primary_color, color = "gray80", show.legend = F, alpha = 0.5),
    colour = "gray75", sigma  = 5, expand = 5
  ) +
  annotate(
    "curve",
    x = zentrum_stadtteil$x+1325, y = zentrum_stadtteil$y+3000,
    xend = zentrum_stadtteil$x, yend = zentrum_stadtteil$y,
    curvature = 0.2,
    color = "gray80",
    linewidth = 0.4,
    alpha = 0.9,
    arrow = arrow(length = unit(3, "pt"), type = "closed")
  ) +
  annotate(
    "richtext",
    x = zentrum_stadtteil$x+1325, 
    y = zentrum_stadtteil$y+3000,
    label = infobox_label,
    fill = "gray20",
    text.colour = "gray95",
    label.colour = "grey80",
    label.padding = unit(c(4, 4, 4, 4), "pt"),
    label.r = unit(6, "pt"),
    hjust = 0, vjust = 0.5,
    lineheight = -2,
    size = 5
  ) +
  labs(
    title = glue('Zigaretten erwünscht! Wo der <b><span style="color:gray90">raucher-<br>freundlichste</span></b> Stadtteil in <b><span style="color:gray90">{stadt}</span></b> liegt'),
    x = glue("Stadtteilgrenzen von {stadt}"),
    caption = "Abbildung: Fabian Hellmold/Datengeschichten"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill =  panel_background_color, color = "transparent"),
    panel.background = element_rect(fill =  panel_background_color, color = "transparent"),
    plot.margin = margin(r=10, t=20, b =20, l = 20),
    plot.title.position = "plot",
    plot.title = element_markdown(color = plot_title_color,
                                  family = plot_title_family,
                                  hjust = 0.5,
                                  lineheight = 1.25,
                                  margin = margin(b= 15, t = -5)),
    axis.title.x = element_text(color = "gray60",
                                family = axis_title_family,
                                size = 8.5),
    plot.caption = element_text(color = plot_caption_color,
                                family = plot_caption_family,
                                face = "italic",
                                margin = margin(r=-30, b = -15, t =25),
                                size = 7))



## Abbildung speichern ---------------------------------------------------------

file_save <- glue("/home/fabian/Schreibtisch/WG-Gesucht_Analyse/abbildungen/projekte/instagram/Rauchen_Stadtteil_{stadt}.png")
file_dropbox <- glue("/home/fabian/Dropbox/Abbildungen_Instagram/Abbildungen_Instagram_Fabi/Rauchen_Stadtteil_{stadt}.png")

ggsave(filename = file_save, plot = Karte_Rauchen,
       width = 4, height = 5, units = "in", dpi = 300)

add_logo_insta(
  image_path = file_save,
  opacity = 0.25,
  logo_path = "/home/fabian/Schreibtisch/WG-Gesucht_Analyse/daten/logos/DG_Logo_schwarzweiß.png"
)

system(paste("xdg-open", shQuote(file_save)))

file_copy(file_save, file_dropbox, overwrite = TRUE)



