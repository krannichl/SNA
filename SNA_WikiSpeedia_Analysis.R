library(tidyverse)

# TSV-Datei einlesen
df <- read_tsv("C:/Users/Krann/GitHub/SNA/paths-finished-tsv.tsv", col_names = TRUE)

head(df,5)

# Funktion zum Extrahieren der Übergänge aus einem Pfad
extract_edges <- function(path_string) {
  pages <- unlist(strsplit(path_string, ";"))
  if (length(pages) < 2) return(NULL)
  edges <- data.frame(
    source = pages[-length(pages)],
    target = pages[-1]
  )
  return(edges)
}

# Anwenden auf alle Pfade
edge_list <- df %>%
  pull(path) %>%
  map_dfr(extract_edges)

weighted_edges <- edge_list %>%
  count(source, target, name = "weight")

library(igraph)

# Netzwerkobjekt erzeugen
g <- graph_from_data_frame(weighted_edges, directed = FALSE)


library(ggraph)

# Berechne zentrale Metriken
V(g)$betweenness <- betweenness(g)
V(g)$degree <- degree(g)

# Zeichnen des Netzwerks
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = "darkgreen") +
  geom_node_point(aes(size = betweenness, color = degree)) +
  scale_size_continuous(range = c(2, 10)) +
  scale_edge_width(range = c(0.2, 2)) +
  theme_void() +
  labs(title = "Wikispeedia Netzwerkvisualisierung (mit R)", 
       subtitle = "Größe = Betweenness Centrality | Farbe = Degree")

print(plot)


