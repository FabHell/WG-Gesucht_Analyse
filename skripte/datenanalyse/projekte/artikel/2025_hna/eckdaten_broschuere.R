


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          ECKDATEN BROSCHÜRE         #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(showtext)
library(ggtext)
library(tidytext)
library(stopwords)


font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()



## Tabelle Infotafel -----------------------------------------------------------

Wochendaten_Kassel <- WGdaten_Kassel %>% 
  filter(datum_scraping > Sys.Date()-7)

# Anzahl neuer Insterate
nrow(Wochendaten_Kassel)

# Kleinste Zimmerfläche
min(Wochendaten_Kassel$zimmergröße)

# Durchschnittliche Miete
Wochendaten_Kassel %>% filter(befristungsdauer > 30 | is.na(befristungsdauer)) %>%
  summarise(mean = round(mean(gesamtmiete),1),
            median = round(median(gesamtmiete),1))

# Beliebtester Stadtteil
Wochendaten_Kassel %>% 
  group_by(stadtteil) %>%
  summarise(fallzahl = n()) %>%
  arrange(desc(fallzahl)) %>%
  head(1)

# Befristete Mietverträge
round((nrow(Wochendaten_Kassel %>% filter(!is.na(befristungsdauer))) / 
 nrow(Wochendaten_Kassel)),3) * 100

# Beliebtestes Adjektiv
tibble(title = Wochendaten_Kassel$titel) %>%
  unnest_tokens(word, title, strip_numeric = T) %>%
  filter(nchar(word) > 2) %>%  
  count(word, sort = TRUE) %>%
  anti_join(tibble(word = stopwords("de")), by = "word") %>%
  print(n = 50)



## Histogram Infotafel ---------------------------------------------------------

Histogram_Miete <- ggplot(data = WGdaten_Kassel_geo2 %>% filter(datum_scraping > Sys.Date()-7), 
       aes(x=gesamtmiete)) +
  geom_histogram(fill= "gray80", colour= "gray20", binwidth = 30) +
  scale_y_continuous(breaks = c(1,3,5,7,9,11)) +
  scale_x_continuous(breaks = c(150,300,450,600,750,900),
                     limits = c(150,920)) +
  
  labs(x = "Zimmermiete in Euro",
       y = "Anzahl der Inserate") +
  theme_minimal() +
  theme(
    axis.title = element_markdown(family = "franklin", size = 20),,
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_markdown(family = "franklin", size = 15),
    axis.line = element_line(),
    panel.grid = element_line(colour = "gray80")
  )
 
file_save <- "Abbildungen/Histogram_Miete.png"
ggsave(filename = file_save, plot = Histogram_Miete, 
       width = 7, height = 4.5, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

