


#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######           ALLUVIAL BEFRISTUNG       #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidyverse)
library(glue)
library(DBI)
library(ggalluvial)
library(ggtext)
library(showtext)
library(patchwork)


font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()


stadt_filter <- "Düsseldorf"


farbe_unbefristet = "transparent"
farbe_kurzbefristung = "#dc2626"
farbe_mittelbefristung = "#ef4444"
farbe_langbefristung = "#f97316"



## Daten laden -----------------------------------------------------------------


con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql <- glue_sql("
  SELECT stadt, befristungsdauer, datum_scraping, einzugsdatum
  FROM analysedaten
 ", teile = St_Teile, .con = con_lokal)

Daten_Stadt <- dbGetQuery(con_lokal, sql)



# Daten vorbereiten ------------------------------------------------------------


daten_plot <- Daten_Stadt %>%
  filter(stadt == stadt_filter) %>%
  mutate(befristung_detail = case_when(
           is.na(befristungsdauer) ~ "Unbefristet",
           befristungsdauer <= 31 ~ "Kurzbefristung",
           befristungsdauer <= 150 ~ "Mittelbefristung",
           befristungsdauer > 150 ~ "Langbefristung"),
    befristung_status = if_else(is.na(befristungsdauer), 
                                "Unbefristet", "Befristet")) %>%
  count(befristung_status, befristung_detail) %>%
  mutate(
    befristung_status = factor(befristung_status, levels = c("Befristet",
                                                             "Unbefristet")),
    befristung_detail = factor(befristung_detail, levels = c("Kurzbefristung",
                                                             "Mittelbefristung",
                                                             "Langbefristung",
                                                             "Unbefristet")))

daten_alluvial <- daten_plot %>%
  mutate(fill = case_when(befristung_detail == "Kurzbefristung" ~ farbe_kurzbefristung,
                          befristung_detail == "Mittelbefristung" ~ farbe_mittelbefristung,
                          befristung_detail == "Langbefristung" ~ farbe_langbefristung,
                          befristung_detail == "unbefristet" ~ farbe_unbefristet))

daten_stratum <- daten_plot %>%
  mutate(fill = case_when(befristung_status == "Befristet" ~ "gray50",
                          befristung_status == "Unbefristet" ~ "gray60"))

df_label_1 <- daten_plot %>%
  filter(befristung_status == "Befristet") %>%
  arrange(desc(befristung_detail)) %>%
  mutate(cum_sum = cumsum(n)) %>%
  mutate(y = cum_sum - (0.5 * n)) %>%
  mutate(label = glue("{round(n / sum(n)*100,0)}%"),
         color = case_when(
           befristung_detail == "Kurzbefristung" ~ farbe_kurzbefristung,
           befristung_detail == "Mittelbefristung" ~ farbe_mittelbefristung,
           befristung_detail == "Langbefristung" ~ farbe_langbefristung))

df_label_2 <- daten_plot %>%
  arrange(desc(befristung_detail)) %>%
  mutate(cum_sum = cumsum(n)) %>%
  mutate(y = cum_sum - (0.5 * n)) %>%
  mutate(label = glue("{round(n / sum(n)*100,0)}%"),
         color = case_when(
           befristung_detail == "Kurzbefristung" ~ farbe_kurzbefristung,
           befristung_detail == "Mittelbefristung" ~ farbe_mittelbefristung,
           befristung_detail == "Langbefristung" ~ farbe_langbefristung)) %>%
  filter(befristung_status == "Befristet")
  
df_label_3 <- daten_plot %>%
  group_by(befristung_status) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(befristung_status)) %>%
  mutate(cum_sum = cumsum(n)) %>%
  mutate(y = cum_sum - (0.5 * n)) %>%
  mutate(label = glue("{befristung_status}: {round(n / sum(n)*100,0)}%"),
         color = case_when(befristung_status == "Befristet" ~ "gray50",
                           befristung_status == "Unbefristet" ~ "gray60"))



# Subtitle Labels --------------------------------------------------------------


durchschnitt_bef_ges <- Daten_Stadt %>%
  mutate(kurzbefristung = ifelse(is.na(befristungsdauer),F,T)) %>%
  group_by(stadt, kurzbefristung) %>%
  summarise(n = n()) %>%
  mutate(n_perc = round(n/sum(n)*100,1)) %>%
  filter(kurzbefristung == T) %>%
  ungroup() %>%
  summarise(n_perc = round(mean(n_perc),1)) %>%
  pull(n_perc)

durchschnitt_bef_stadt <- Daten_Stadt %>%
  filter(stadt == stadt_filter) %>%
  mutate(kurzbefristung = ifelse(is.na(befristungsdauer),F,T)) %>%
  group_by(kurzbefristung) %>%
  summarise(n = n()) %>%
  mutate(n_perc = round(n/sum(n)*100,1)) %>%
  filter(kurzbefristung == T) %>%
  pull(n_perc)

kurzbef_stadt <- Daten_Stadt %>%
  filter(stadt == stadt_filter) %>%
  filter(!is.na(befristungsdauer)) %>%
  mutate(kurzbefristung = ifelse(befristungsdauer <= 31,T,F)) %>%
  group_by(kurzbefristung) %>%
  summarise(n = n()) %>%
  mutate(n_perc = round(n/sum(n)*100,1)) %>%
  filter(kurzbefristung == T) %>%
  pull(n_perc)

datum_stadt <- Daten_Stadt %>%
  filter(stadt == stadt_filter) %>%
  mutate(datum_scraping = as.Date(datum_scraping))



# Plot erstellen ---------------------------------------------------------------


plot_befristung <- ggplot(daten_plot,
       aes(y = n, axis1 = befristung_status, axis2 = befristung_detail)) +
  geom_alluvium(data = daten_alluvial, aes(fill = fill), segments = 100,
                curve_type = "sine", width = 1/12, alpha = 0.7) +
  geom_stratum(data = daten_stratum, aes(fill = fill),
               width = 1/12, color = "gray30") +
  geom_rect(aes(xmin=1.958, xmax=2.042,
                ymin=sum(daten_plot$n) -daten_plot[4,3]*0.995,
                ymax=sum(daten_plot$n)),
            fill="#f8f3f2", color = "#f8f3f2") +
  geom_text(data = df_label_1, aes(y = y,label = label, color = color), 
                x = 1.95, alpha = 1, hjust = 1,
                family = "franklin", inherit.aes = F, fontface = "bold") +
  geom_text(data = df_label_2, aes(y = y,label = label, color = color), 
            x = 1.05, alpha = 1, hjust = 0,
            family = "franklin", inherit.aes = F, fontface = "italic") +
  geom_text(data = df_label_3, aes(y = y,label = label, color = color), 
            x = 0.93, alpha = 1, hjust = 0.5, angle = 90,
            family = "franklin", inherit.aes = F, fontface = "bold") +
  geom_text(y= (daten_alluvial[1,3] + daten_alluvial[2,3] + daten_alluvial[3,3])/2, 
            x = 2.07, label = "Befristungstpyen",
            alpha = 1, hjust = 0.5, angle = -90, color = "gray50",
            family = "franklin", inherit.aes = F, fontface = "bold") +
  scale_y_continuous(expand = F) +
  scale_color_identity() +
  scale_fill_identity(
    guide = "legend",
    labels = setNames(
      c("Kurzbefristung *(< 31 Tage)*", "Mittelbefristung *(< 150 Tage)*", "Langbefristung *(> 150 Tage)*", "Befristet", "Unbefristet"),
      c(farbe_kurzbefristung, farbe_mittelbefristung, farbe_langbefristung, "gray50", "gray60")),
    breaks = c("gray50", "gray60", farbe_kurzbefristung, farbe_mittelbefristung, farbe_langbefristung)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.955, 0.975), 
    legend.justification = c(1, 1), 
    panel.background = element_rect(fill = "#f8f3f2", color = NA),
    plot.background = element_rect(fill = "#f8f3f2", color = NA),
    legend.key.spacing.y = unit(c(0.5, 3, 0, 0, 0), "pt"),
    legend.title = element_blank(),
    legend.text = element_markdown(family = "franklin", size = 7),
    legend.key.size = unit(14, "pt"),
    legend.background = element_rect(fill = "gray95", color = "gray20",
                                     linewidth = 0.25))


plot_monat <- Daten_Stadt %>%
  filter(stadt == stadt_filter) %>%
  filter(befristungsdauer < 32) %>%
  mutate(einzugsdatum = as.Date(einzugsdatum),
         einzugsdatum_monat = format(einzugsdatum, "%b"),
         einzugsdatum_monat = factor(einzugsdatum_monat, 
                                     levels = c("Dez", "Nov", "Okt", "Sep",  
                                                "Aug", "Jul", "Jun", "Mai", 
                                                "Apr", "Mrz", "Feb", "Jan"))) %>%
  group_by(einzugsdatum_monat, .drop = FALSE) %>% 
  summarise(N = n()) %>%
  
  ggplot() +
  geom_col(aes(y = einzugsdatum_monat, x = N),
           fill = "#dc2626", color = "gray25", alpha = 0.7) +
  scale_x_continuous(expand = c(0.075, 0.05)) +
  scale_y_discrete(drop = FALSE) +
  labs(x = NULL,
       y = "Einzugsmonat für Kurzbefristungen") +
  theme(axis.line = element_line(),
        axis.text = element_text(family = "franklin"),
        axis.text.x = element_text(family = "franklin", margin = margin(b=20, t=2.5)),
        axis.title.y = element_text(family = "domine", margin = margin(r=7.5, l=15)),
        panel.background = element_rect(fill = "#f8f3f2", color = NA),
        plot.background = element_rect(fill = "#f8f3f2", color = NA),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())


combinded_plot <-plot_befristung + plot_monat +
  plot_layout(ncol = 2, widths = c(2, 0.75)) +
  plot_annotation(
    title = glue("Kurzbefristungen auf WG-Gesucht"),
    subtitle = glue("In {stadt_filter} sind **{durchschnitt_bef_stadt}%** der Anzeigen befristet (Durchschnitt aller Städte {durchschnitt_bef_ges}%). Von diesen Angeboten ist die Befristungsdauer bei **{kurzbef_stadt}%** niedriger als 31 Tage. Das Einzugsdatum dieser Angebote liegt schwerpunktmäßig in den Monaten der Sommersemsterferien (**Aug, Sep**)."),
    caption = glue("Datengrundlage sind {nrow(datum_stadt)} Anzeigen die zwischen dem {format(min(datum_stadt$datum_scraping), '%d.%m.%y')} und dem {format(max(datum_stadt$datum_scraping), '%d.%m.%y')} erhoben wurden"),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", family = "domine",
                                hjust = 0, margin = margin(b=10,t=5)),
      plot.subtitle = element_textbox_simple(size = 9.5, color = "grey20",
                                             family = "domine", lineheight = 1.25,
                                             margin = margin(b=20)),
      plot.caption = element_text(size = 7, face = "italic", family = "franklin",
                                  margin = margin(b=5,l=0,t=-13, r=-10)),
      plot.background = element_rect(fill = "#f8f3f2"),
      plot.margin = margin(r=10)
    ))


# Plot speichern ---------------------------------------------------------------


file_save <- glue("Abbildungen/Befristung/{stadt_filter}_Befristung.png")
ggsave(filename = file_save, plot = combinded_plot, 
       width = 10, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))


