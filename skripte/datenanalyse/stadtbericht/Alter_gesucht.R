

library(tidyverse)
library(ggridges)
library(DBI)
library(glue)

stadt_filter <- "Chemnitz"

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")

sql <- glue_sql("
  SELECT alter_ges
  FROM analysedaten
   WHERE stadt = {stadt_filter}
 ", .con = con_lokal)


Daten_Staedte <- dbGetQuery(con_lokal, sql) 


glue("{round(nrow(subset(Daten_Staedte, is.na(alter_ges))) / nrow(Daten_Staedte) * 100,1)}% ist das Alter egal")

df <- Daten_Staedte %>%
  mutate(alter_ges = ifelse(is.na(alter_ges), "16 und 99", alter_ges)) %>%
  mutate(alter_seq = map(alter_ges, ~ {
    teile <- str_extract_all(.x, "\\d+")[[1]] %>% as.integer()
    seq(teile[1], teile[2])
  })) %>%
  unnest(alter_seq) %>%
  count(alter_seq, name = "haeufigkeit") %>%
  filter(alter_seq >= 18 & alter_seq <= 60) %>%
  mutate(anteil = haeufigkeit / nrow(Daten_Staedte) * 100) 


ggplot(df, aes(x = alter_seq, y = 1, height = anteil,
                     fill = after_stat(height))) +
  geom_density_ridges_gradient(stat = "identity", scale = 1) +
  scale_fill_gradientn(colours = c("#6baed6", "#2171b5", "#08306b")) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Alter", y = "", fill = "Anteil") +
  theme_ridges()


df %>%
  ggplot() +
  geom_line(aes(x=alter_seq, y=anteil, group = 1)) +
  scale_x_continuous(breaks = seq(18, 60, by=2)) +
  scale_y_continuous(limits = c(0,100)) 
  
  
  

stadt_filter <- "Kassel"

sql <- glue_sql("
  SELECT alter_ges, geschlecht_ges
  FROM analysedaten
   WHERE stadt = {stadt_filter}
 ", .con = con_lokal)


Daten_Staedte <- dbGetQuery(con_lokal, sql) 


df_2 <- Daten_Staedte %>%
  mutate(alter_ges = ifelse(is.na(alter_ges), "16 und 99", alter_ges)) %>%
  mutate(alter_seq = map(alter_ges, ~ {
    teile <- str_extract_all(.x, "\\d+")[[1]] %>% as.integer()
    seq(teile[1], teile[2]) %>% as.vector()
  }))


## Mann - 18 Jahre -------------------------------------------------------------

df_2 %>%
  filter(map_lgl(alter_seq, ~ 25 %in% .x) & geschlecht_ges %in% c("Mann", "Geschlecht egal")) %>%
  nrow() / nrow(df_2) * 100


## Frau - 25 Jahre -------------------------------------------------------------

df_2 %>%
  filter(map_lgl(alter_seq, ~ 25 %in% .x) & geschlecht_ges %in% c("Frau", "Geschlecht egal")) %>%
  nrow() / nrow(df_2) * 100
