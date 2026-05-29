


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######           ABB. KARTE_BOXPLOT        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(foreign)
library(sf) 
library(showtext)
library(patchwork)
library(ggtext)
library(DBI)
library(glue)


font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()


stadt <- "Hamburg"
Offset <- -61      # Entfernung Bars und Boxplots 
Mulitplik <- 0.2   # Länge der Bars



## Daten laden -----------------------------------------------------------------


Geodaten_Stadtteile <- st_read("Daten/Geodaten/Hamburg/Geo_Stadtteile/Stadtteile_Hamburg.shp") %>%
  filter(stadtteil_ != "Neuwerk") %>%
  select(stadtteil = stadtteil_, stadtbezirk = bezirk_nam)

Grenzen_Elbe <- st_read("Daten/Geodaten/Hamburg//Geo_Elbe/Elbe.shp")

St_Teile <- as.data.frame(Geodaten_Stadtteile) %>%
  select(stadtteil) %>% pull()

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql <- glue_sql("
  SELECT gesamtmiete, stadtteil_geocoding
  FROM analysedaten
  WHERE stadt = {stadt}
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND stadtteil_geocoding IN ({teile*})
 ", teile = St_Teile, .con = con_lokal)

Daten_Hamburg <- dbGetQuery(con_lokal, sql) %>%
  filter(!is.na(gesamtmiete)) %>%
  group_by(stadtteil_geocoding) %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.01),
    quantile(gesamtmiete, 0.99)
  )) %>%
  ungroup()



## Boxplot ---------------------------------------------------------------------


Auswahlliste_Fallzahl <- Daten_Hamburg %>%
  group_by(stadtteil_geocoding) %>%
  summarise(fallzahl = n()) %>%
  arrange(desc(fallzahl)) %>%
  slice_head(n = 20) %>%
  pull(stadtteil_geocoding)


Daten_Boxplot <- Daten_Hamburg %>%
  mutate(elbe = ifelse(stadtteil_geocoding 
                       %in% c("Cranz", "Finkenwerder", "Neuenfelde", "Francop",
                              "Neugraben-Fischbek", "Welterhof", "Altenwerder",
                              "Moorburg", "Hausbruch", "Steinwerder", "Kleiner Grasbrook",
                              "Heimfeld", "Eißendorf", "Harburg", "Neuland", "Wilstorf",
                              "Gut Moor", "Marmstorf", "Langenbek", "Rönneburg", "Sinstorf",
                              "Veddel", "Wilhelmsburg", "Waltershof"), 0, 1)) %>%
  filter(stadtteil_geocoding %in% Auswahlliste_Fallzahl) %>%
  group_by(stadtteil_geocoding) %>%
  mutate(fallzahl = n(),
         stadtteil_geocoding = factor(stadtteil_geocoding)) %>%
  filter(fallzahl >= 5)


Daten_Crossbar <- Daten_Boxplot %>%
  mutate(xmin = -50,
         xmax = (xmin + fallzahl)*Mulitplik,
         x = (xmin + xmax)/2) %>%
  select(stadtteil_geocoding, fallzahl, xmin, xmax, x)




Boxplot <- Daten_Boxplot %>%
  
  ggplot(aes(x = gesamtmiete, 
             y = reorder(stadtteil_geocoding, gesamtmiete, FUN = median),
             fill = elbe)) +
  geom_vline(xintercept = median(Daten_Hamburg$gesamtmiete, na.rm = TRUE),
             linetype = "dashed") +
  geom_jitter(color = "black", size = 1.10, show.legend = F, height = 0.2,
              shape = 21, alpha = 0.4) +
  geom_boxplot(outlier.shape = NA, coef = 0,
               alpha = 0.33, show.legend = F, width = 0.6) +
  scale_x_continuous(breaks = c(-45, 250, 500, 750, 1000, 1250),
                     labels = c("<b style='color:#E9C46A;'>Anzahl</b>", 250, 500, 750, 1000, 1250)) +
  geom_crossbar(data = Daten_Crossbar,
                mapping = aes(xmin = xmin, xmax = xmax, x = x, y = stadtteil_geocoding),
                inherit.aes = F, fill = "#E9C46A", color = NA) +
  annotate("text", x = Daten_Crossbar$xmax + Offset + 85, y = Daten_Crossbar$stadtteil_geocoding, 
           label = Daten_Crossbar$fallzahl, size = 2, family = "franklin", 
           hjust = 0, fontface = "italic") +
  annotate("segment", x = c(-60, 127.5), xend = c(77.5, 1350) , y = 0.25, yend = 0.25,
           colour = "gray40", linewidth = 0.5) +
  annotate("segment", x = c(70,120), xend = c(85,135), y = 0.06, yend = 0.44,
           colour = "gray40", linewidth = 0.5) +
  coord_cartesian(clip = "off", expand = F, xlim = c(-60, 1350), ylim = c(0.25, 20.5)) +
  labs(x = "Miete") +
  theme_classic() +
  theme(axis.ticks = element_line(color = "gray40"),
        axis.line = element_line(color = "gray40"),
        axis.text.x = element_markdown(family = "franklin", size = 7),
        axis.line.x = element_blank(),
        axis.text.y = element_markdown(family = "franklin", size = 8),
        axis.title.x = element_markdown(size = 9, family = "franklin", 
                                        margin = margin(t = 10, l = 30)), 
        axis.title.y = element_blank())




## Karte -----------------------------------------------------------------------


Karte <- Geodaten_Stadtteile %>%
  rename(stadtteil_geocoding = stadtteil) %>%
  right_join(Daten_Hamburg, by = "stadtteil_geocoding") %>%
  group_by(stadtteil_geocoding) %>%
  
  summarise(median_gesamtmiete = median(gesamtmiete, na.rm = TRUE),
            fallzahl = n()) %>%
  filter(fallzahl >= 5) %>%
  
  ggplot() +
  geom_sf(data = Geodaten_Stadtteile, color = "transparent", fill = "gray95") +
  geom_sf(aes(fill = median_gesamtmiete)) +
  scale_fill_gradient(
    low = "gray90", high = "black",
    limits = c(375,925),
    breaks = seq(400,900,100),
    name = "Medianmiete") +
  geom_sf(data = Grenzen_Elbe, aes(color = Elbe), linewidth = 0.8, 
          fill = NA, show.legend = F) +
  geom_sf(data = Grenzen_Elbe %>% filter(Elbe == 0) %>% st_transform(25832) %>%  
            mutate(geometry = st_buffer(geometry, dist = -0.007 * sqrt(st_area(geometry) / pi))) %>% 
            st_transform(st_crs(Grenzen_Elbe)),
          linewidth = 0.4, fill = NA, color = "#0f2153") +
  labs(fill = "Medianmiete",
       x = "Stadtteilgrenzen Hamburgs") +
  theme_void() +
  theme(legend.title = element_markdown(size = 7, family = "franklin"),
        legend.position = c(0.925,0.65),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.55, "cm"),
        legend.ticks = element_blank(),
        axis.title.x = element_text(family = "franklin", size = 9,
                                    margin = margin(t = 20)),
        axis.line.x = element_line(color = "gray40"))





## Zusammenfügen ---------------------------------------------------------------


Boxplot_Karte <- Boxplot + Karte +
  plot_layout(ncol = 2, widths = c(0.75, 1.5)) +
  plot_annotation(
    title = "Die beliebtesten Studiviertel Hamburgs",
    caption = "Datengrundlage sind die Anzeigen der Webseite WG-Gesucht. Anzeigen mit einer befristeten Mietdauer von unter 60 Tagen wurden aus der Analyse ausgeschlossen",
    subtitle = glue("Alle Viertel mit einer hohen Medianmiete liegen <b><span style='color:#56b1f7'>nördlich der Elbe</span></b>, während alle Viertel <b><span style='color:#0f2153'>südlich der Elbe</span></b> eine niedrige Medianmiete aufweisen. Die <b><span style='color:#E9C46A'>meisten<br>WG-Anzeigen</span></b> gibt es in {Auswahlliste_Fallzahl[1]}, {Auswahlliste_Fallzahl[2]} und {Auswahlliste_Fallzahl[3]}."),
    theme = theme(plot.title = element_text(size = 12, family = "domine",
                                            margin = margin(l = -4)),
                  plot.subtitle = element_markdown(family = "franklin", size = 8,
                                                   margin = margin(l = 8, b = 5, t = 6)),
                  plot.caption = element_text(size = 6, family = "franklin")))


file_save <- "Abbildungen/Speziell/Anzahl_Boxplot_Karte_Hamburg.png"
ggsave(filename = file_save, plot = Boxplot_Karte, 
       width = 9, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

