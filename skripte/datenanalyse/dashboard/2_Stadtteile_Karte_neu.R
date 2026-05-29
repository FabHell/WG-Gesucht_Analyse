


query_geo_stadtteile <- glue_sql("
  SELECT stadt, stadtteil, geom.STAsText() AS wkt
  FROM geodaten_stadtteile
    WHERE stadt = {params$stadt}
      AND stadtteil IS NOT NULL
", .con = con_lokal)

grenzen_stadtteile <- dbGetQuery(con_lokal, query_geo_stadtteile) %>%
  st_as_sf(wkt = "wkt", crs = 4326) %>%
  st_transform(crs = 25832) 

Daten_Stadt <- dbGetQuery(con_lokal, sql) %>%
  filter(!is.na(gesamtmiete), !is.na(stadtteil_geocoding))
  
gesamt_median <- Daten_Stadt %>%
  summarise(gesamt_median = median(gesamtmiete)) %>% pull()

Daten_Stadtteile <- Daten_Stadt %>%
  group_by(stadtteil_geocoding) %>%
  summarise(stadtteil_n = n(),
            stadtteil_n_perc = round(stadtteil_n/nrow(Daten_Stadt)*100,1),
            stadtteil_median = median(gesamtmiete),
            stadtteil_Q1 = quantile(gesamtmiete, 0.25),
            stadtteil_Q3 = quantile(gesamtmiete, 0.75),
            stadtteil_IQA = IQR(gesamtmiete)) %>%
  filter(stadtteil_n > 20) %>%
  arrange(stadtteil_median) %>%
  mutate(stadtteil_geocoding = factor(stadtteil_geocoding, levels = stadtteil_geocoding)) %>%
  mutate(diff_median = stadtteil_median - gesamt_median,
         diff_label = ifelse(diff_median > 0, glue("+{diff_median}"),
                              diff_median),
         tooltip = glue("{stadtteil_geocoding}\nAnteil: {stadtteil_n_perc}%\nAbweichung: {diff_median}"))

color_palette <- setNames(
  colorRampPalette(c("#B3D9FF", "#1A5490"))(nrow(Daten_Stadtteile)),
  Daten_Stadtteile$stadtteil_geocoding
)



Miete_Stadtteile_Karte <- ggplot() +
  geom_sf(data = grenzen_stadtteile,
          fill = "gray95", color =  "gray95") +
  geom_sf(data = grenzen_stadtteile 
            %>% rename(stadtteil_geocoding = stadtteil)
            %>% filter(stadtteil_geocoding %in% Daten_Stadtteile$stadtteil_geocoding),
          aes(fill = stadtteil_geocoding),
          color = "darkgray") +
  scale_fill_manual(values = color_palette) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0),
        legend.position = "none")


Miete_Stadtteile_Barplot <- ggplot(Daten_Stadtteile %>% slice_max(stadtteil_n, n = 20), 
       aes(x = stadtteil_median , y = stadtteil_geocoding, 
               fill = stadtteil_geocoding)) +
  geom_col_interactive(aes(data_id = stadtteil_geocoding, tooltip = tooltip),
                       color = "gray40", width = 0.75, alpha = 0.8, linewidth = 0.2,
                       show.legend = F) +
  geom_vline(xintercept = gesamt_median, color = "gray20", linewidth = 0.33,
             linetype = "dashed") +
  annotate(geom = "text", 
           label = "\u00D8",
           x = gesamt_median + max(Daten_Stadtteile$stadtteil_median)*0.03,
           y = 1.5, size = 3,
           hjust = 0) +
  scale_fill_manual(values = color_palette) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Miete in den beliebtesten Stadtteilen",
    x = NULL,
    y = NULL
  ) +
  theme(
    plot.title = element_text(size = 8, hjust = 0.5, margin = margin(b=10)),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "gray20"),
    axis.ticks = element_blank(),
    axis.text.x = element_text(margin = margin(t=5), size = 4),
    axis.text.y = element_text(margin = margin(r=5), size = 4)
  ) 

Mein_Plot <- Miete_Stadtteile_Barplot + Miete_Stadtteile_Karte +
  plot_layout(widths = c(0.5,1))


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

selection_css <- "
  r: 12px;
  fill: orange;
  stroke-width: 2.5px;
  transition: all 1s ease;
"

Mein_Plot_int <- girafe(
  ggobj = Mein_Plot,
  options = list(
    opts_hover(css = hover_css),
    opts_tooltip(css = tooltip_css, zindex = 9999),
    opts_selection(type = "multiple", css = selection_css, only_shiny = FALSE),
    opts_toolbar(saveaspng = FALSE, hidden = c("lasso_select", "lasso_deselect"))
    )
)
