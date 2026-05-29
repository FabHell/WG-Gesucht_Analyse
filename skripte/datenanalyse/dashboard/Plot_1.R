
Daten_Erhebungszeitpunkt <- Daten_Stadt %>%
  mutate(zeitpunkt = format(datum_scraping , "%b %y")) %>%
  group_by(zeitpunkt) %>%
  summarise(zeitpunkt_n = n())

plot_scraping <- Daten_Erhebungszeitpunkt %>%
  ggplot(aes(x = zeitpunkt, y = zeitpunkt_n)) +
  geom_col_interactive(aes(data_id = zeitpunkt, tooltip = zeitpunkt_n),
                       fill = "#DE2D26", width = 0.7, alpha = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = NULL,
       x = NULL, 
       y = "Anzahl Anzeigen") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "gray20", linewidth = 0.25),
        axis.line.y = element_line(color = "gray20", linewidth = 0.25),
        axis.text.x = element_text(size = 8, family = "domine",
                                   margin = margin(t=15)),
        axis.text.y = element_text(size = 8, family = "domine"),
        axis.title.y = element_text(size = 9, family = "domine", face = "bold",
                                    margin = margin(r = 7.5)))

## Interaktive Abbildung erstellen ---------------------------------------------

tooltip_css <- "
  background: linear-gradient(145deg, #f0f0f0, #e6e6e6);
  border: none;
  border-radius: 12px;
  box-shadow: 3px 3px 10px #d1d1d1, -3px -3px 10px #ffffff;
  color: #333;
  font-family: 'Kassel', Tahoma, Geneva, Verdana, sans-serif;
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
  opacity: 0.4;
  transition: all 1s ease;
"

selection_css <- "
  fill: orange;
  stroke-width: 1.5px;
  transition: all 1s ease;
"

interactive_plot_scraping <- girafe(
  ggobj = plot_scraping,
  width_svg = 16,
  height_svg = 6,
  options = list(
    opts_hover(css = hover_css),
    opts_tooltip(css = tooltip_css),
    opts_hover_inv(css = hover_css_inv),
    opts_selection(type = "multiple", css = selection_css, only_shiny = FALSE),
    opts_toolbar(saveaspng = FALSE, hidden = c("lasso_select", "lasso_deselect")),
    opts_sizing(rescale = TRUE, width = 1)
  )
)

interactive_plot_scraping
