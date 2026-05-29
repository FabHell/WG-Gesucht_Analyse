
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

query_geo_stadtteile <- glue_sql("
  SELECT stadt, stadtteil, geom.STAsText() AS wkt
  FROM geodaten_stadtteile
    WHERE stadt = {params$stadt}
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

Wohnung_Point <- Daten_Stadt %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  st_transform(crs = 25832) %>%
  st_join(grenzen_stadt %>% rename(stadt_filter = stadt)) %>%
  filter(!is.na(stadt_filter)) %>%
  rename(stadtteil = stadtteil_geocoding)


farbe_grün <- "darkgreen" 
farbe_rot <- "darkred"

Karte_Punkt <- ggplot() +
  geom_sf_interactive(data = grenzen_stadtteile, 
                      aes(data_id = stadtteil, tooltip = stadtteil),
                      color = "darkgray", fill = "gray95") +
  geom_sf_interactive(data = Wohnung_Point, 
                      colour = farbe_grün, fill = farbe_grün, 
                      aes(data_id = stadtteil, tooltip = stadtteil),
          size = 0.5, shape = 1, linewidth= 0.1, alpha = 0.75) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0))

Karte_Punkt_interaktiv <- girafe(
  ggobj = Karte_Punkt,
  options = list(
    opts_hover(css = hover_css),
    opts_tooltip(css = tooltip_css, zindex = 9999),
    opts_hover_inv(css = hover_css_inv),
    opts_selection(type = "multiple", css = selection_css, only_shiny = FALSE),
    opts_toolbar(saveaspng = FALSE, hidden = c("lasso_select", "lasso_deselect")),
    opts_sizing(rescale = TRUE, width = 1)
  )
)


Karte_Punkt_interaktiv
