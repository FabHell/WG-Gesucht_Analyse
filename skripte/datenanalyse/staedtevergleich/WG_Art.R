

library(ggtext)
library(tidyverse)
library(DBI)
library(patchwork)
library(glue)

con_lokal <- dbConnect(odbc::odbc(),
                       Driver             = "ODBC Driver 17 for SQL Server",
                       Server             = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database           = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt            = "No")

sql <- glue_sql("
  SELECT stadt, gesamtmiete, wg_art
  FROM analysedaten
  WHERE (befristungsdauer IS NULL OR befristungsdauer >= 60)
    AND wg_art IS NOT NULL
    AND gesamtmiete IS NOT NULL
    AND stadtteil_geocoding IS NOT NULL
 ", .con = con_lokal)

WGdaten_ges <- dbGetQuery(con_lokal, sql) %>%
  mutate(stadt = case_when(
    stadt == "Freiburg im Breisgau" ~ "Freiburg i.B.",
    stadt == "Frankfurt am Main" ~ "Frankfurt a.M.",
    TRUE ~ stadt)) %>%
  mutate(gesamtmiete = ifelse(stadt == "Zürich", gesamtmiete * 0.93, gesamtmiete)) %>%
  group_by(stadt) %>%
  filter(between(
    gesamtmiete,
    quantile(gesamtmiete, 0.01),
    quantile(gesamtmiete, 0.99)
  )) %>%
  ungroup()




## Deskription WG-Art ----------------------------------------------------------


Label <- tribble(
  ~label,             ~x,  ~y,
  "Studenten-WG",     -50, length(unique(WGdaten_ges$stadt)) + 1.75,
  "Berufstätigen-WG",  50, length(unique(WGdaten_ges$stadt)) + 1.75
)

Median_Miete <- WGdaten_ges %>%
  group_by(stadt) %>%
  summarise(median_miete = median(gesamtmiete))
  

farbe_beruf <- "#4c72b0"   
farbe_student <- "#c44e52" 
farbe_linie <- "#F0C808"   

Abb1 <- WGdaten_ges %>%
  mutate(beruf = str_detect(wg_art, "Berufstätigen-WG"),
         student = str_detect(wg_art, "Studenten-WG")) %>%
  group_by(stadt) %>%
  summarise(
    n_ges = n(),
    perc_beruf = sum(beruf) / n_ges * 100,
    perc_student = sum(student) / n_ges * 100,
    auszugskoeff = perc_beruf - perc_student,
    median_miete = median(gesamtmiete)) %>%
  arrange(auszugskoeff) %>%
  mutate(
    alpha_stufe = cut(
      median_miete,
      breaks = quantile(median_miete, probs = seq(0, 1, 0.25)),
      include.lowest = TRUE,
      labels = c(rep("0.4", 3), "0.8")
    ),
    alpha_stufe = as.numeric(as.character(alpha_stufe))
  ) %>%
  mutate(stadt = ifelse(alpha_stufe == 0.8, glue("**{stadt}**"), stadt)) %>%
  mutate(stadt = factor(stadt, levels = stadt)) %>%
  
  ggplot() +
  geom_col(aes(x = perc_beruf, y = stadt, alpha = as.numeric(alpha_stufe)), fill = farbe_beruf) +
  geom_col(aes(x = -perc_student, y = stadt, alpha = as.numeric(alpha_stufe)), fill = farbe_student) +
  geom_vline(aes(xintercept = 0), color = "gray40") +
  geom_line(aes(x = auszugskoeff, y = stadt, group = 1),
            color = farbe_linie, linewidth = 1.5, lineend = "round") +
  geom_text(data = Label, aes(x = x, y = y, label = label), vjust = -0.5,
            fontface = "bold", size = 3.5, color = "gray20") +
  labs(x = "WG-Typ", y = NULL) +
  scale_alpha_identity() +
  
  scale_x_continuous(
    limits = c(-82, 82),
    breaks = seq(-80, 80, by = 20),
    labels = c(
      glue("<span style='color:{farbe_student}'>80%</span>"),
      glue("<span style='color:{farbe_student}'>60%</span>"),
      glue("<span style='color:{farbe_student}'>40%</span>"),
      glue("<span style='color:{farbe_student}'>20%</span>"),
           "<span style='color:gray25'>0%</span>",
      glue("<span style='color:{farbe_beruf}'>20%</span>"),
      glue("<span style='color:{farbe_beruf}'>40%</span>"),
      glue("<span style='color:{farbe_beruf}'>60%</span>"),
      glue("<span style='color:{farbe_beruf}'>80%</span>")
    )
  ) +
  coord_cartesian(clip = "off") + 
  theme(
    legend.position = "top",
    axis.title.x = element_text(margin = margin(t=5)),
    panel.grid.major.x = element_line(color = "gray95"),
    axis.line.x = element_line(color = "gray25"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_markdown(size = 7, margin = margin(r=5)),
    axis.text.x = element_markdown(size = 8),
    plot.margin = margin(t = 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )



Korrelation <- WGdaten_ges %>%
  mutate(beruf = str_detect(wg_art, "Berufstätigen-WG"),
         student = str_detect(wg_art, "Studenten-WG")) %>%
  group_by(stadt) %>%
  summarise(n_ges = n(),
            perc_beruf = sum(beruf)/n_ges*100,
            perc_student = sum(student)/n_ges*100,
            auszugskoeff = perc_beruf- perc_student,
            median_miete = median(gesamtmiete)) %>%
  ungroup() %>%
  summarise(cor = cor(auszugskoeff, median_miete))


Abb2 <- WGdaten_ges %>%
  mutate(beruf = str_detect(wg_art, "Berufstätigen-WG"),
         student = str_detect(wg_art, "Studenten-WG")) %>%
  group_by(stadt) %>%
  summarise(n_ges = n(),
            perc_beruf = sum(beruf)/n_ges*100,
            perc_student = sum(student)/n_ges*100,
            auszugskoeff = perc_beruf- perc_student,
            median_miete = median(gesamtmiete)) %>%
  mutate(fill = ifelse(auszugskoeff > 0, farbe_beruf, farbe_student)) %>%
  
  ggplot() +
  geom_point(aes(x = auszugskoeff, y = median_miete, fill = fill),
             size = 2, shape = 21, color = "gray20") +
  geom_vline(aes(xintercept = 0), color = "gray75" , alpha = 0.7, linetype = "dashed") +
  geom_textbox(
    label = glue("Korrelation: **{round(Korrelation$cor, 2)}**"),
    x = -37.5,
    y = 1075,
    halign = 0.5,
    valign = 0.5,
    box.colour = "gray40",
    fill = "#F6F3EE",
    box.padding = unit(c(6, 6, 4, 6), "pt"),
    width = NULL
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    limits = c(-65, 65),
    breaks = seq(-50, 50, by = 25),
        labels = c(
      glue("<span style='color:{farbe_student}'>+50%</span>"),
      glue("<span style='color:{farbe_student}'>+25%</span>"),
           "<span style='color:gray25'>0%</span>",
      glue("<span style='color:{farbe_beruf}'>+25%</span>"),
      glue("<span style='color:{farbe_beruf}'>+50%</span>"))) +
  labs(x = "WG-Typdifferenz", y = "Medianmiete in €") +
#  scale_y_continuous(position = "right") +

  theme(
    axis.text.x = element_markdown(size = 8),
    axis.text.y = element_markdown(size = 8),
    axis.title.x = element_text(margin = margin(t=5)),
    axis.line.x = element_line(color = "gray25"),
    axis.ticks.y = element_blank(), 
    panel.grid.major = element_line(color = "gray95"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
  )


Plot_Combinded <- Abb1 + Abb2 +
  plot_layout(nrow = 1, widths = c(1, 1))



file_save <- "Abbildungen/Städtevergleich/WG_art.png"
ggsave(filename = file_save, plot = Plot_Combinded, 
       width = 11, height = 6.7, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))




