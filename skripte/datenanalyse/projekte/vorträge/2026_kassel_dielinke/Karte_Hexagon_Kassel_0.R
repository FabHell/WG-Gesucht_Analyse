

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ABB. POLYGON-RASTER        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


stadt <- "Kassel"

cell_size <- 200


library(sf)
library(ggplot2)
library(dplyr)
library(glue)
library(DBI)
library(tidyverse)
library(ggfx)


# Anfang -----------------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")


# Geodaten Stadtteilgrenzen ----------------------------------------------------

query_geo_stadtteile <- glue_sql("
  SELECT stadt, stadtteil, geom.STAsText() AS wkt
  FROM geodaten_stadtteile
    WHERE stadt = {stadt}
      AND stadtteil IS NOT NULL
", .con = con_lokal)

grenzen_stadtteile <- dbGetQuery(con_lokal, query_geo_stadtteile) %>%
  st_as_sf(wkt = "wkt", crs = 4326) %>%
  st_transform(crs = 25832) 

grenzen_stadt <- grenzen_stadtteile %>%
  st_make_valid() %>%
  group_by(stadt) %>%
  summarise(geometry = st_union(wkt)) %>%
  ungroup()



# Abbildung erstellen ----------------------------------------------------------

Karte_Hexagon <- ggplot() + 
  with_outer_glow(
    geom_sf(data = grenzen_stadt, linewidth = 0.1,
            fill = "transparent", color = "white"),
    colour = "gray65", sigma  = 5, expand = 7.5
  ) +
  geom_sf(data = grenzen_stadtteile, linewidth = 0.5,
          fill = "gray85", color = "gray40") +
  theme_minimal() +
  labs(
    title    = NULL,
    subtitle = NULL,
    caption  = NULL
  ) +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    plot.background  = element_rect(fill =  "#1c202a", color = "transparent"),
    plot.margin      = margin(r=220, t=5, b =5),
  )


file_save <- "Abbildungen/1_Präsentation_Linke_Kassel/Karte_Hexagon_Kassel_0.png"
ggsave(filename = file_save, plot = Karte_Hexagon, 
       width = 16, height = 9, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))


