



library(DBI)
library(tidyverse)
library(glue)
library(ggiraph)



## Laden Kontextdaten ----------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql_bundesländer <- glue_sql("
  SELECT 
    land, 
    bundesland, 
    stadt, 
    MIN(datum_scraping) as min_scraping,
    AVG(daily_count) as avg_inserate_pro_tag
  FROM (
    SELECT 
      land, 
      bundesland, 
      stadt, 
      datum_scraping,
      COUNT(*) as daily_count
    FROM analysedaten
    GROUP BY land, bundesland, stadt, datum_scraping
  ) daily_counts
  GROUP BY land, bundesland, stadt
", .con = con_lokal)

Daten_Bundesländer <- dbGetQuery(con_lokal, sql_bundesländer)


## Laden Geodaten --------------------------------------------------------------

Grenzen_Deutschland <- read_sf("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Geodaten\\Länder\\Grenzen_Deutschland.shp")

Grenzen_Deutschland_Länder <- read_sf("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Geodaten\\Länder\\Grenzen_Deutschland_Länder.shp")

Grenzen_Österreich <- read_sf("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Geodaten\\Länder\\Grenzen_Österreich.shp")

Grenzen_Schweiz <- read_sf("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Geodaten\\Länder\\Grenzen_Schweiz.shp")

Ländergrenzen <- rbind(Grenzen_Deutschland, Grenzen_Österreich, Grenzen_Schweiz)

Städte <- tribble(
  ~stadt,           ~lon,               ~lat,
  "Aachen",         6.082743532923922,  50.77573001649543, 
  "Augsburg",       10.887522206187763, 48.369686631415746, 
  "Berlin",         13.405360305520869, 52.563487057187885,
  "Bielefeld",      8.539835524643284,  52.024741528101096, 
  "Bochum",         7.217994649408415,  51.482766521512445, 
  "Bonn",           7.118662934643078,  50.7386725713355,
  "Braunschweig",   10.526168436192563, 52.26809559303163, 
  "Bremen",         8.809032820084719,  53.10061603012626,
  "Chemnitz",       12.920531471007271, 50.82810463595815, 
  "Cottbus",        14.33372496204373,  51.754997042998866, 
  "Darmstadt",      8.656881236568923,  49.87442851192214, 
  "Dortmund",       7.457864061520965,  51.52024214385786, 
  "Dresden",        13.736207730218931, 51.0525254969056, 
  "Düsseldorf",     6.775201457735772,  51.22415027836273, 
  "Erfurt",         11.029029139530477, 50.986605670649794,
  "Erlangen",       11.01091704770822,  49.58825764007154, 
  "Essen",          7.020128167144641,  51.46090339647404, 
  "Flensburg",      9.45027735635882,   54.793579775566435, 
  "Frankfurt am Main",    8.663727112875037,  50.10731456019293,
  "Freiburg im Breisgau", 7.818350906644233,  48.0052193092798, 
  "Gießen",         8.675080180760034,  50.5842490625327, 
  "Göttingen",      9.922036345055394,  51.541519896916476, 
  "Graz",           15.438518750036325, 47.06793136229636, 
  "Greifswald",     13.39202469266285,  54.0865001462675, 
  "Halle (Saale)",  11.973421046122118, 51.49644404048509, 
  "Hamburg",        9.987042110080903,  53.553279772406064,
  "Hannover",       9.732286758652755,  52.380895174106875,
  "Heidelberg",     8.673298143559537,  49.39825086843024,
  "Innsbruck",      11.384962214531884, 47.26787245954032,
  "Jena",           11.587344756218398, 50.92637941601262,
  "Kaiserslautern", 7.7524041646974515, 49.43927093279264, 
  "Karlsruhe",      8.383400375271572,  49.00637206785274, 
  "Kassel",         9.49100211603941 ,  51.316177420292426,
  "Kiel",           10.119487891523704, 54.3268875682913, 
  "Koblenz",        7.597470963104658,  50.35756800246554, 
  "Köln",           6.954923435846886,  50.93993506822349, 
  "Konstanz",       9.165428760332635,  47.67819961166951, 
  "Leipzig",        12.378489735450566, 51.35761659312437,
  "Linz",           14.287983765348168, 48.30722045560804, 
  "Lübeck",         10.683097913807499, 53.86339018829212,
  "Magdeburg",      11.594449428183303, 52.114889546869236, 
  "Mainz",          8.24939467059664,   49.998252116651756,
  "Mannheim",       8.47047915758798,   49.49144964361285,
  "Marburg",        8.759962720171446,  50.80123512470428, 
  "München",        11.592605857059318, 48.15169102722596,
  "Münster",        7.62268542473895,   51.95724248772362, 
  "Nürnberg",       11.045106081434792, 49.45080473819774, 
  "Oldenburg",      8.211214055723985,  53.144462614722634, 
  "Osnabrück",      8.038940324177238,  52.28207282713195, 
  "Potsdam",        13.060333269638903, 52.39188143517382, 
  "Regensburg",     12.098657732817207, 49.01110576684179, 
  "Rostock",        12.097478283010805, 54.0907585147431, 
  "Saarbrücken",    6.990737345574007,  49.23413595576404, 
  "Salzburg",       13.044561110255993, 47.806696496629236,
  "Stuttgart",      9.182891701001106,  48.78058458692476,
  "Trier",          6.635716646643825,  49.7476036357669,
  "Tübingen",       9.049146199425564,  48.52248133239845, 
  "Weimar",         11.320513004546225, 50.979592249444586, 
  "Wien",           16.378227551371193, 48.230740742348665, 
  "Wuppertal",      7.139008411696691,  51.25690144056619, 
  "Zürich",         8.541882826334083,  47.380434464308735)%>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(Ländergrenzen)) 


## Aufbereiten Geodaten --------------------------------------------------------

Städte_aufb <- Städte %>%
  left_join(Daten_Bundesländer, by = "stadt") %>%
  mutate(data_id = ifelse(land == "Österreich", "Österreich", bundesland)) %>%
  mutate(min_scraping = as.Date(min_scraping),
         min_scraping = format(min_scraping, "%d.%m.%y"),
         tooltip = glue("<b style='font-weight:700; font-size:16px; margin-bottom:1.5px; display:block;'><u>{stadt}</u></b>Erhebungsstart: {min_scraping}<br>Inserate pro Tag: {avg_inserate_pro_tag}"))


 
## Statische Abbildung erstellen -----------------------------------------------

Karte_erhobeneStädte <- ggplot() +
  geom_sf(data = Ländergrenzen, fill = "gray95") +
  geom_sf(data = Grenzen_Deutschland_Länder, 
          fill = "gray95", colour = "gray80") +
  geom_sf(data = Ländergrenzen %>% filter(shapeISO == "DEU"),
          fill = "transparent") +
  geom_sf_interactive(data = Städte_aufb, 
                      aes(data_id = data_id, tooltip = tooltip),
                      size = 3, color = "darkred",
                      alpha = 0.5) +
  theme_void() +
  theme(plot.title = element_markdown(family = "franklin",
                                      hjust = 0.5, size = 15),
        plot.margin = margin(r=350, t=0, b=0))



## Interaktive Abbildung erstellen ---------------------------------------------

tooltip_css <- "
  background: linear-gradient(145deg, #f0f0f0, #e6e6e6);
  border: none;
  border-radius: 12px;
  box-shadow: 3px 3px 10px #d1d1d1, -3px -3px 10px #ffffff;
  color: #333;
  font-family: 'franklin', Tahoma, Geneva, Verdana, sans-serif;
  font-size: 14px;
  padding: 12px;
  transition: all 1.5s ease-out;
"

hover_css <- "
  cursor: pointer;
  filter: brightness(1.2) drop-shadow(0 0 5px rgba(78, 84, 200, 0.5));
  stroke-width: 1px;
  transition: all 1s ease;
"

hover_css_inv <- "
  r: 0px;
  opacity: 0.4;
  transition: all 1.5s ease;
"

selection_css <- "
  r: 12px;
  fill: orange;
  stroke-width: 2.5px;
  transition: all 1s ease;
"

interactive_plot <- girafe(
  ggobj = Karte_erhobeneStädte,
  width_svg = 12,
  height_svg = 9,
  options = list(
    opts_hover(css = hover_css),
    opts_tooltip(css = tooltip_css, zindex = 9999),
    opts_hover_inv(css = hover_css_inv),
    opts_selection(type = "multiple", css = selection_css, only_shiny = FALSE),
    opts_toolbar(saveaspng = FALSE, hidden = c("lasso_select", "lasso_deselect")),
    opts_sizing(rescale = TRUE, width = 1)
  )
)

interactive_plot

