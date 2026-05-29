


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######     ENTWICKLUNG BAFÖG_GEFÖRDERTE    #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############



## Pakete und Daten laden ------------------------------------------------------


library(tidyverse)
library(glue)
library(ggtext)

data <- read.csv("C:\\Users\\hellm\\Desktop\\WG-Gesucht_Analyse\\Daten\\Kontextdaten\\Analyse_Katapult_Sachsen\\Anzahl_Bafog_Gefoerderte_Sachsen.csv", sep = ";") 

hervor <- c(1994, 2004, 2014, 2024)



## Daten aufbereiten -----------------------------------------------------------


data_bafog <- data %>%
  filter(X2_variable_attribute_label == "Insgesamt") %>%
  filter(value_variable_label == "Geförderte Personen") %>%
  filter(time >= 1994) %>%
  select(jahr = time, gefoerderte = value) %>%
  mutate(jahr = as.integer(jahr)) %>%
  mutate(size = ifelse(jahr %in% hervor, 3, 0.75)) %>%
  mutate(text_label_position = ifelse(jahr == 2014, gefoerderte-500, gefoerderte),
         text_label_hjust = ifelse(jahr == 2014, -0.25, 0.5)) %>%
  mutate(x_axis_size = ifelse(jahr %in% hervor, 12, 9)) %>%
  mutate(x_axis_label = ifelse(
    jahr %in% hervor,
    glue('<span style="font-size:{x_axis_size}px"><b>{jahr}</b></span>'),
    glue('<span style="font-size:{x_axis_size}px">{jahr}</span>')
  )) %>%
  arrange(jahr)



## Abbildung erstellen ---------------------------------------------------------


ggplot(data_bafog, aes(x = jahr, y = gefoerderte)) +
  geom_line(color = "#378ADD", linewidth = 1) +
  geom_point(aes(size = size), color = "#378ADD",
             fill = "white", shape = 21, stroke = 1.5) +
  geom_text(
    data = filter(data_bafog, jahr %in% hervor),
    aes(label = scales::comma(gefoerderte, big.mark = ".", decimal.mark = ","),
        y = text_label_position, hjust = text_label_hjust),
    vjust = -2, size = 3, color = "#185FA5"
  ) +
  scale_x_continuous(breaks = min(data_bafog$jahr):max(data_bafog$jahr),
                     labels = data_bafog$x_axis_label) +
  scale_y_continuous(
    labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
  ) +
  scale_size_identity() +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "BAföG-Geförderte in Sachsen (Anzahl)"
  ) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.background   = element_blank(),
    axis.ticks         = element_blank(), 
    axis.text.x        = element_markdown(angle = 45, hjust = 0.5, vjust = 1,
                                          margin = margin(t = 10)),
    axis.text.y        = element_text(size = 7),
    axis.title.y       = element_text(margin = margin(r = 15), size = 11),
    plot.margin        = margin(t = 15, l = 5, b = 5)
  )


file_save <- "Abbildungen/Analyse_Katapult_Sachsen/Entwicklung_Bafog_Gefoerderte.png"
ggsave(filename = file_save, plot = last_plot(),
       width = 9, height = 5, units = "in", dpi = 300)
shell.exec(normalizePath(file_save))




