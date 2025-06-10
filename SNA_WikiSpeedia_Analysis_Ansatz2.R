library(tidyverse)
library(igraph)
library(ggraph) # Für die Visualisierung
library(tidygraph) # Nützlich für Tidyverse-ähnliche Operationen auf Graphen

# TSV-Datei einlesen
df <- read_tsv("C:/Users/Krann/GitHub/SNA/paths-finished-tsv.tsv", col_names = TRUE)

# Zeilen mit "NULL" im 'rating' filtern (wie von Ihnen bereits gemacht)
df <- df %>%
  filter(rating != "NULL")

# Funktion zum Extrahieren der Übergänge aus einem Pfad (wie von Ihnen bereits gemacht)
extract_edges <- function(path_string) {
  pages <- unlist(strsplit(path_string, ";"))
  if (length(pages) < 2) return(NULL)
  edges <- data.frame(
    source = pages[-length(pages)],
    target = pages[-1]
  )
  return(edges)
}

# Anwenden auf alle Pfade (wie von Ihnen bereits gemacht)
edge_list <- df %>%
  pull(path) %>%
  map_dfr(extract_edges)

# Kanten gewichten (wie von Ihnen bereits gemacht)
weighted_edges <- edge_list %>%
  count(source, target, name = "weight")

# Netzwerkobjekt erzeugen (ungerichtet, wie gewünscht)
g <- graph_from_data_frame(weighted_edges, directed = FALSE)

# Berechne zentrale Metriken und speichere sie als Knotenattribute
V(g)$betweenness <- betweenness(g)
V(g)$degree <- degree(g)

# Filtern Sie Knoten mit einem Degree von 3 oder weniger
# # ALTER CODE: Filter für Degree <= 3
# g_filtered <- g %>%
#   delete_vertices(V(g)$degree <= 3)

# NEUER CODE: Stärkere Filterung des Netzwerks nach Degree
# Experimentieren Sie mit diesem Schwellenwert (z.B. 10, 20, 50, 100)
min_degree_threshold <- 40 # Beispiel: Nur Knoten mit mindestens 10 Verbindungen behalten

g_filtered <- g %>%
  delete_vertices(V(g)$degree < min_degree_threshold) # Knoten mit weniger als X Verbindungen entfernen

cat("Anzahl der Knoten im ursprünglichen Graphen:", vcount(g), "\n")
cat("Anzahl der Knoten im gefilterten Graphen (Degree-Filter):", vcount(g_filtered), "\n")

# WICHTIG: Wenn Sie Knoten löschen, sollten Betweenness und Degree neu berechnet werden,
# da sich die Pfade und Verbindungen im gefilterten Graphen ändern.
V(g_filtered)$betweenness <- betweenness(g_filtered)
V(g_filtered)$degree <- degree(g_filtered)

# Visualisierung des gefilterten Netzwerks
plot_filtered_network <- ggraph(g_filtered, layout = "fr") +
  # Kanten: Breite nach Gewicht, Farbe dunkelgrün
  geom_edge_link(aes(width = weight), alpha = 0.3, color = "darkgreen") + # alpha leicht erhöht
  scale_edge_width(range = c(0.5, 5)) + # Bereich für Kantenbreite angepasst für bessere Sichtbarkeit
  
  # Knoten: Größe nach Betweenness, Farbe nach Degree
  geom_node_point(aes(size = betweenness, color = degree), show.legend = TRUE) +
  scale_size_continuous(range = c(3, 15)) + # Bereich für Knotengröße angepasst für bessere Sichtbarkeit
  scale_color_viridis_c(option = "B", direction = -1) + # Eine bessere Farbskala (viridis)
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 20) + # Knotenbeschriftungen
  
  theme_void() + # Entfernt Achsen und Hintergrund
  labs(
    title = "Wikispeedia Netzwerkvisualisierung (gefiltert)",
    subtitle = "Größe = Betweenness Centrality | Farbe = Degree | Filter: Degree > 3"
  ) +
  theme(
    legend.position = "right", # Legendenposition anpassen
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

print(plot_filtered_network)

