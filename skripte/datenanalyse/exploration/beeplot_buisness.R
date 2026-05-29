


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######         ABB. BEEPLOT_BUISNESS       #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(ggbeeswarm)
library(ggdogs)
library(glue)
library(patchwork)
library(showtext)
library(ggtext)


font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()



## Datenarbeit -----------------------------------------------------------------


con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

Daten_ges <- dbGetQuery(con_lokal, "
  SELECT *
  FROM analysedaten
  WHERE stadt IN ('München', 'Berlin')
    AND (gesamtmiete >= 200 AND gesamtmiete <= 1450)
    AND (befristungsdauer IS NULL OR befristungsdauer >= 60)
")


Daten_ges <- Daten_ges %>%
  filter(!is.na(wg_art)) %>%
  mutate(business = str_detect(wg_art, "Business-WG")) 




## Beeplot ---------------------------------------------------------------------


Mittelwerte <- Daten_ges %>%
  group_by(stadt) %>%
  summarise(mean_miete = mean(gesamtmiete, na.rm = TRUE)) %>%
  mutate(label = "\u00D8", x = 0.415, y = mean_miete+25)

Mittelwerte_labels <- Daten_ges %>%
  group_by(stadt) %>%
  mutate(ueber_unter_mean = if_else(gesamtmiete > mean(gesamtmiete, na.rm = TRUE),
                                    "Über Mittelwert", "Unter Mittelwert")) %>%
  filter(business == TRUE) %>%
  group_by(stadt, ueber_unter_mean) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(stadt, ueber_unter_mean, fill = list(n = 0)) %>%
  mutate(label = if_else(ueber_unter_mean == "Über Mittelwert", 
                         paste0('<span style="color:#FFD700"><b>',n,'</b></span> > \u00D8'),
                         paste0('<span style="color:#FFD700"><b>',n,'</b></span> < \u00D8')),
         y = if_else(ueber_unter_mean == "Über Mittelwert", 1360,1304),
         x = 1.37)

Bee <- ggplot(Daten_ges, aes(x = "", y = gesamtmiete, colour = business,
                             alpha = business)) +
  geom_quasirandom(cex = 1.5, show.legend = FALSE) +
  geom_hline(data = Mittelwerte, 
             aes(yintercept = mean_miete), 
             linetype = "dashed", color = "black") +
  geom_textbox(data = Mittelwerte_labels, aes(label=label, x=x, y=y), 
               color = "black", size = 3, inherit.aes = F, box.colour = NA,
               box.size = unit(2,"cm"), family = "domine", width = unit(2.5, "cm"), 
               halign = 1) +
  geom_text(data = Mittelwerte, aes(label=label, x=x, y=y), inherit.aes = F,
            family = "domine", size = 2.5) +
  scale_y_continuous(breaks = seq(250,1500,250))+
  scale_color_manual(
    values = c("TRUE" = "#FFD700", "FALSE" = "#4D4D4D", "NA" = "#4D4D4D")) + 
  scale_alpha_manual(
    values = c("TRUE" = 0.8, "FALSE" = 0.2)) + 
  facet_wrap(~stadt) +
  labs(x = NULL, y = "Zimmermiete in €") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f8f5f2", color = "black"), 
        strip.text = element_text(face = "bold", size = 10), 
        axis.title.y = element_text(margin = margin(r=10)))



# Piechard ---------------------------------------------------------------------


Daten_Piechard <- Daten_ges %>%
  group_by(stadt, business) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(stadt) %>%
  mutate(prozent = n / sum(n) * 100)

Daten_Piechard_labels <- Daten_Piechard %>%
  group_by(stadt) %>%
  arrange(stadt, desc(business)) %>%
  mutate(ypos = cumsum(prozent) - prozent / 2, 
         label = paste0(round(prozent, 1), "%")) %>%
  mutate(Dog = case_when(
    business == TRUE & stadt == "Berlin" ~ "eyes",
    business == FALSE & stadt == "Berlin" ~ "doge",
    business == TRUE & stadt == "München" ~ "glasses",
    business == FALSE & stadt == "München" ~ "chilaquil",
    is.na(business) ~ NA
  ))

hsize <- 1

N_Label <- Daten_ges %>%
  group_by(stadt) %>%
  summarise(n = n()) %>%
  mutate(label = paste0("N: ", n))

Business_Dog <- Daten_Piechard_labels %>%
  mutate(x = hsize) %>%
  ggplot(aes(x=hsize, y=prozent, fill=business)) +
  geom_label(data = N_Label, aes(y = 0, x=0.2, label = label), inherit.aes = F,
             size = 2.5, label.padding = unit(0.5, "cm"), fill = "#f8f5f2",
             family = "domine", fontface = "italic") + 
  geom_col(show.legend = F) +
  geom_label(aes(y = ypos, x=0.8, label = label),
             size = 3, fill = "white", label.size = 0.3,    
             label.r = unit(0.15, "lines"))  + 
  geom_dog(aes(y = ypos, x= 1.2, label = label, dog = Dog),
           size = 3.5, inherit.aes = F) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  scale_fill_manual(values = c("TRUE" = "#FFD700", 
                               "FALSE" = "#4D4D4D")) +
  facet_wrap(~stadt) +
  labs(x = "WG-Kategorien\nim Vergleich") +
  theme_void() +
  theme(strip.text = element_blank(),
        axis.title.y = element_text(angle = 90, family = "franklin", vjust = 24.5),
        panel.spacing = unit(1.65, "cm"))



# Zusammenfügen ----------------------------------------------------------------


Ges <- Bee + Business_Dog +
  plot_layout(ncol = 1, heights = c(1, 0.55)) +
  plot_annotation(
    title = "Unternehmertum in Wohngemeinschaften",
    subtitle = glue("Die Zimmermiete für WG's, die sich selbst als <b><span style='color:#FFD700'>Business-WG</span></b> bezeichnen, ist in der Tendenz höher als bei <b><span style='color:#4D4D4D'>WG's<br>ohne dieses Label</span></b>. In München ist dieser WG-Typ proportional {round(Daten_Piechard[4,4]/Daten_Piechard[2,4],1)}x so oft vertreten als in Berlin."),
    theme = theme(plot.title = element_text(size = 12, family = "domine",
                                            margin = margin(l = -4)),
                  plot.subtitle = element_markdown(family = "franklin", size = 8, , lineheight = 1.1,
                                                   margin = margin(l = 8, b = 5, t = 6)),
                  plot.caption = element_text(size = 6, family = "franklin")))


file_save <- "Abbildungen/Business_Dog.png"
ggsave(filename = file_save, plot = Ges, 
       width = 6.25, height = 7, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))


