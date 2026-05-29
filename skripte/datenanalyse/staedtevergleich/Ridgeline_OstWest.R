


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######         RIDGELINEPLOT OST-WEST      #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(DBI)
library(tidyverse)
library(ggridges)
library(glue)
library(showtext)
library(ggtext)

font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()

farbe_west <- "#b3cde3"
farbe_ost  <- "#74a9cf"

## Daten laden -----------------------------------------------------------------

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

sql <- glue_sql("
  SELECT bundesland, stadt, gesamtmiete
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND gesamtmiete IS NOT NULL
    AND land = 'Deutschland'
", .con = con_lokal)

WGdaten_ges <- dbGetQuery(con_lokal, sql)


## Daten aufbereiten -----------------------------------------------------------

WGdaten_trim <- WGdaten_ges %>%
  mutate(region = ifelse(bundesland %in% c("Brandenburg", "Mecklenburg-Vorpommern", 
                                           "Sachsen", "Sachsen-Anhalt", "Thüringen"),
                         "Ost", "West")) %>%
  mutate(stadt = case_when(
    stadt == "Freiburg im Breisgau" ~ "Freiburg i.B.",
    stadt == "Frankfurt am Main" ~ "Frankfurt a.M.",
    TRUE ~ stadt)) %>%
  group_by(stadt) %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.01),
    quantile(gesamtmiete, 0.99)
  )) %>%
  ungroup()

median_mieten <- WGdaten_trim %>%
  group_by(stadt, region) %>%
  summarise(median_miete = median(gesamtmiete, na.rm = TRUE), .groups = "drop") %>%
  mutate(stadt = ifelse(region == "Ost", 
                        glue("**{stadt}**"), 
                        as.character(stadt))) %>%
  arrange(median_miete)

dichte_df <- WGdaten_trim %>%
  group_by(stadt, region) %>%
  summarise(dichte = list(density(gesamtmiete, na.rm = TRUE)), .groups = "drop") %>%
  mutate(x = map(dichte, ~.$x),
         y = map(dichte, ~.$y)) %>%
  select(-dichte) %>%
  unnest(cols = c(x, y)) %>%
  group_by(stadt) %>%
  mutate(scale_y = y / max(y)) %>%
  ungroup() %>%
  mutate(stadt = ifelse(region == "Ost", 
                        glue("**{stadt}**"), 
                        as.character(stadt))) %>%
  mutate(stadt = factor(stadt, levels = median_mieten$stadt))


## Abbildung erstellen und speichern -------------------------------------------
dichte_df %>%
  ggplot(aes(x = x, y = stadt, fill = region, height = scale_y)) + 
  geom_ridgeline(color = "#1D1E34", linewidth = 0.3, alpha = 1, scale = 2) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = c("Ost" = farbe_ost, "West" = farbe_west)) +
  scale_x_continuous(breaks = c(250, 500, 750, 1000, 1250)) +
  labs(x = "Zimmermiete", y = NULL, title = NULL) +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.925, 0.05),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_line(color = "gray90"),
        axis.line = element_line(color = "gray10"),
        axis.title.x = element_text(margin = margin(t = 5), family = "domine"),
        axis.text.x = element_text(family = "franklin"),
        axis.text.y = element_markdown(family = "franklin"))

file_save <- "Abbildungen/Ridgeline_ostwest.png"
ggsave(filename = file_save, plot = last_plot(), 
       width = 6, height = 9, units = "in", dpi = 300)
shell.exec(normalizePath(file_save))




