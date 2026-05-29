

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


daten_stadtteile <- Daten_Stadt %>%
  filter(!is.na(gesamtmiete), !is.na(stadtteil_geocoding)) %>%
  group_by(stadtteil_geocoding) %>%
  summarise(stadtteil_n = n(),
            stadtteil_median = median(gesamtmiete),
            stadtteil_Q1 = quantile(gesamtmiete, 0.25),
            stadtteil_Q3 = quantile(gesamtmiete, 0.75),
            stadtteil_IQA = IQR(gesamtmiete)) %>%
  filter(stadtteil_n > 20) %>%
  arrange(desc(stadtteil_median))

color_palette <- setNames(
  colorRampPalette(c("#1A5490", "#B3D9FF"))(nrow(daten_stadtteile)),
  daten_stadtteile$stadtteil_geocoding
)

plot_density_stadtteile <- function(stadtteil_name) {
  stadtteil_data <- Daten_Stadt %>%
    filter(!is.na(gesamtmiete)) %>%
    filter(stadtteil_geocoding == stadtteil_name) %>%
    group_by(stadtteil_geocoding) %>%
    filter(between(
      gesamtmiete,
      quantile(gesamtmiete, 0.05),
      quantile(gesamtmiete, 0.95)
    )) %>%
    ungroup() %>%
    left_join(daten_stadtteile, by = "stadtteil_geocoding") %>%
    mutate(tooltip = glue("Q1: {stadtteil_Q1}\nMedian: {stadtteil_median}\nQ3: {stadtteil_Q3}"))
  
  full_range <- Daten_Stadt %>%
    filter(!is.na(gesamtmiete)) %>%
    group_by(stadtteil_geocoding) %>%
    filter(between(
      gesamtmiete,
      quantile(gesamtmiete, 0.05),
      quantile(gesamtmiete, 0.95)
    )) %>%
    ungroup() %>%
    pull(gesamtmiete) %>%
    range()
  
  
  # Erstelle den Violin-Plot
  violin <- ggplot(stadtteil_data, aes(x = gesamtmiete, y = "")) +
    geom_violin_interactive(aes(data_id = stadtteil_name, tooltip = tooltip),
                            fill = color_palette[stadtteil_name], 
                            color = "grey50", linewidth = 0.75) +
    annotate(geom = "segment", 
             x = full_range[1], xend = full_range[2], y = 1, yend = 1,
             linetype = "dashed", color = "gray20", linewidth = 0.5) +
    annotate(geom = "segment", x = full_range, xend = full_range, 
             y = 0.85, yend = 1.15,
             linetype = "dashed", color = "gray20", linewidth = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA, coef = 0,
                 fill = "gray90", color = "black", linewidth = 0.5) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(xlim = full_range, expand = F, clip = "off") +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(l=14)
    ) 
  
  girafe(
    ggobj = violin,
    options = list(
      opts_hover(css = hover_css),
      opts_tooltip(css = tooltip_css, zindex = 9999),
      opts_selection(type = "single", css = selection_css, only_shiny = FALSE),
      opts_toolbar(saveaspng = FALSE, hidden = c("lasso_select", "lasso_deselect"))),
    height_svg = 1.5)
}

tabelle <- daten_stadtteile %>%
  select(stadtteil_geocoding, stadtteil_n, stadtteil_median) %>%
  mutate(distribution = stadtteil_geocoding) %>%
  reactable(
    columns = list(
      stadtteil_geocoding = colDef(
        name = "Stadtteil",
        align = "left",
        vAlign = "center",
        minWidth = 120, 
        style = function(value) {
          list(
            color = color_palette[value] %>% unname(),
            fontWeight = "bold"
          )
        }
      ),
      stadtteil_n= colDef(
        name = "Anzahl",
        align = "center",
        vAlign = "center",
        minWidth = 80,
        maxWidth = 120,
        format = colFormat(
          digits = 0
        )
      ),
      stadtteil_median = colDef(
        name = "Median",
        align = "center",
        vAlign = "center",
        minWidth = 80,
        maxWidth = 120,
        format = colFormat(
          digits = 0,
          suffix = ' €'
        )
      ),
      distribution = colDef(
        name = "Verteilung",
        align = "center",
        vAlign = "center",
        minWidth = 250,
        cell = function(value) {
          plot_density_stadtteile(value) 
        }
      )
    ),
    style = list(
      fontFamily = "Source Sans Pro",
      fontSize = 24
    ),
    defaultPageSize = nrow(daten_stadtteile),
    fullWidth = TRUE
  )

