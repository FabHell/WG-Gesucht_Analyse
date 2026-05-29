

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######              ABB. TREEMAP           #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(tidygraph)
library(ggraph)
library(viridis)
library(DBI)

con_lokal <- dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = Sys.getenv("SERVER_SQL_LOKAL"),
                       Database = Sys.getenv("DATABASE_SQL_LOKAL"),
                       Trusted_Connection = "Yes",
                       Encrypt = "No")


# Edges konstuieren ------------------------------------------------------------

edges_raw <- dbGetQuery(con_lokal, "
   SELECT DISTINCT bundesland, stadt 
   FROM analysedaten
") %>%
  mutate(bundesland = ifelse(stadt %in% c("Wien", "Linz", "Salzburg", "Graz",
                                               "Innsbruck", "Zürich"), 
                             "Ausland", bundesland)) 


bundeslaender_edges <- edges_raw %>%
  filter(bundesland != "Ausland") %>%
  distinct(bundesland) %>%
  mutate(from = "Deutschland",
         to = bundesland) %>%
  select(from, to)

ausland_edge <- data.frame(
  from = "Deutschland",
  to = "Ausland",
  stringsAsFactors = FALSE
)

staedte_edges <- edges_raw %>%
  mutate(from = bundesland,
         to = stadt) %>%
  select(from, to)

edges_hierarchisch <- bind_rows(
  bundeslaender_edges,
  ausland_edge,
  staedte_edges
)



# Verticles konstuieren --------------------------------------------------------

vertices_raw <- dbGetQuery(con_lokal, "
  SELECT stadt, datum_scraping
  FROM analysedaten
  WHERE CAST(datum_scraping AS DATE) >= CAST(DATEADD(day, -14, GETDATE()) AS DATE)
") %>%
  select(-datum_scraping)


staedte_vertices <- vertices_raw %>%
  group_by(stadt) %>%
  summarise(size = n(), .groups = 'drop') %>%
  rename(name = stadt)

bundeslaender_vertices <- edges_raw %>%
  distinct(bundesland) %>%
  mutate(size = 0) %>%
  rename(name = bundesland)

deutschland_vertex <- data.frame(
  name = "Deutschland",
  size = 0,
  stringsAsFactors = FALSE
)

vertices <- bind_rows(
  deutschland_vertex,
  bundeslaender_vertices,
  staedte_vertices
)



## Abbildung erstellen ---------------------------------------------------------


# Graph erstellen
# Bundesland-Größen berechnen (Summe der Städte)
bundesland_sizes <- edges_hierarchisch %>%
  filter(from != "Deutschland") %>%
  left_join(vertices %>% rename(to = name), by = "to") %>%
  group_by(from) %>%
  summarise(bl_size = sum(size, na.rm = TRUE), .groups = 'drop') %>%
  rename(name = from)

# Vertices mit korrigierten Größen
vertices_corrected <- vertices %>%
  left_join(bundesland_sizes, by = "name") %>%
  mutate(size = ifelse(!is.na(bl_size), bl_size, size)) %>%
  select(name, size) %>%
  distinct()

# Graph neu erstellen mit korrigierten Größen
gr <- tbl_graph(nodes = vertices_corrected, edges = edges_hierarchisch)

# Bundesland-Zuordnung
bundesland_mapping <- edges_hierarchisch %>%
  filter(from != "Deutschland") %>%
  rename(name = to, bl = from)

# Hierarchie-Ebene für Labels berechnen
gr <- gr %>%
  activate(nodes) %>%
  mutate(
    hierarchy_level = case_when(
      name == "Deutschland" ~ 0,
      name %in% edges_hierarchisch$from[edges_hierarchisch$from != "Deutschland"] ~ 1,
      TRUE ~ 2
    )
  ) %>%
  left_join(bundesland_mapping, by = "name") %>%
  mutate(
    # Stadtstaaten identifizieren
    is_stadtstaat = name %in% c("Berlin", "Hamburg", "Bremen"),
    
    # Labels: Städte ODER Stadtstaaten anzeigen
    label = ifelse(hierarchy_level == 2 | is_stadtstaat, name, ""),
    
    bundesland = case_when(
      name == "Deutschland" ~ "Deutschland",
      hierarchy_level == 1 ~ name,
      !is.na(bl) ~ bl,
      TRUE ~ "Sonstiges"
    )
  )

# Treemap Plot (bleibt gleich)
ggraph(gr, layout = 'treemap', weight = size) +
  geom_node_tile(aes(fill = bundesland), size = 0.25, color = "white") +
  geom_node_text(aes(label = label), size = 2.5, color = "white") +
  scale_fill_viridis_d(option = "cividis", begin = 0.2, end = 0.9) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none"
  ) +
  labs(
    title = "Fallzahlen nach Bundesland und Stadt",
    subtitle = "Hierarchische Darstellung: Deutschland → Bundesländer → Städte"
  )




# ggraph(gr, layout = 'circlepack', weight=size) + 
#   geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
#   geom_node_label(aes(label=label), size=4)

