


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######      ENTWICKLUNG BAFÖG_PAUSCHALE    #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############



## Pakete und Daten laden ------------------------------------------------------


library(tidyverse)

dat <- tribble(
  ~jahr, ~pauschale, ~geplant,
  2016,  250,        FALSE,
  2017,  250,        FALSE,
  2018,  250,        FALSE,
  2019,  325,        FALSE,
  2020,  325,        FALSE,
  2021,  325,        FALSE,
  2022,  360,        FALSE,
  2023,  360,        FALSE,
  2024,  380,        FALSE,
  2025,  380,        FALSE,
  2026,  380,        FALSE,
  2027,  440,        TRUE
)



## Abbildung erstellen ---------------------------------------------------------


dat_real    <- filter(dat, !geplant)
dat_planned <- filter(dat, jahr >= 2026)  

ggplot(dat, aes(x = jahr, y = pauschale)) +
  annotate(
    "rect",
    xmin = 2026.125, xmax = 2027.875,
    ymin = -Inf, ymax = Inf,
    fill = "grey80", alpha = 0.5
  ) +
  annotate(
    "text",
    x = 2027, y = 480,
    label = "geplant",
    size = 3.5, color = "grey40", fontface = "italic"
  ) +
  geom_line(data = dat_real, color = "#378ADD", linewidth = 1) +
  geom_line(data = dat_planned, color = "#378ADD", linewidth = 1,
            linetype = "dashed") +
  geom_point(data = dat_real, color = "#378ADD", size = 2.5,
             fill = "white", shape = 21, stroke = 1.5) +
  geom_point(data = filter(dat, geplant), color = "#378ADD", size = 3,
             fill = "grey90", shape = 21, stroke = 1.5) +
  geom_text(
    data = filter(dat, jahr %in% c(2016, 2019, 2022, 2024, 2027)),
    aes(label = paste0(pauschale, " €")),
    vjust = -1.75, size = 3.2, color = "#185FA5"
  ) +
  scale_x_continuous(breaks = 2016:2027) +
  scale_y_continuous(
    limits = c(200, 480),
    labels = scales::label_dollar(prefix = "", suffix = " €")
  ) +
  labs(
    x        = NULL,
    y        = "Wohnkostenpauschale (€/Monat)"
    ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.title.y       = element_text(margin = margin(r=10))
  )


file_save <- "Abbildungen/Analyse_Katapult_Sachsen/Entwicklung_Wohnkostenpauschale.png"
ggsave(filename = file_save, plot = last_plot(), 
       width = 9, height = 5, units = "in", dpi = 300)
shell.exec(normalizePath(file_save))

