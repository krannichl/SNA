#Phind anfrage

#du bist ein experte in R und spezialist im bereich netzwerk analyse.
#Es gibt einen Datensatz der die verbindungen von Wikipedia seiten beschreibt.
#Ein ausschnitt wie die Daten vorliegen sieht wie folgt aus: 14th_century;Europe;Africa;Atlantic_slave_trade;African_slave_trade
#Der datensatz hat 24000 zeilen und beinhaltet weitere attribute, wie die benötigte zeit in sekunden um von einem Wikipedia Artikel zum nächsten zu gelangen oder ein Rating.

#Ich bin neu in R und habe keine ahnung und kein vorwissen.
#Ich möchte ein Netzwerk erstellen, das aufzeigt, welche Wikipedia artikel am häufigsten aufgerufen werden.
#Bitte hilf mir schritt für schritt dies zu erreichen, sage mir auch, welche informationen du hierfür von mir brauchst.


# Laden Sie die benötigten Bibliotheken
library(tidyverse)
library(igraph)
library(conflicted)

# Laden Sie die Daten
daten <- read_delim("C:/Users/Krann/OneDrive - bwedu/Studium/5. Semester/Social Network Analysis/paths-finished-tsv.tsv", delim = "\t")

# Überprüfen Sie die Struktur der Daten
str(daten)

# Erstellen Sie ein leeres Netzwerk mit den eindeutigen Artikeln als Knoten
netzwerk <- make_empty_graph(n = length(unique(daten$path)), directed = FALSE)

# Fügen Sie die Kanten zum Netzwerk hinzu
kanten <- unique(daten$path)

# Setzen Sie Attribute für jeden Knoten (in diesem Fall die Artikelnamen)
set_vertex_attr(netzwerk, name = "Artikel", index = V(netzwerk), value = unique(daten$path))

# Zähle die Anzahl der Verbindungen für jeden Artikelpaar
verbindungshäufigkeit <- daten %>%
  count(path) %>%
  mutate(Häufigkeit = n) %>%
  select(path, Häufigkeit)

# Sortieren Sie die Ergebnisse nach Häufigkeit absteigend
top_verbindungen <- verbindungshäufigkeit %>%
  arrange(desc(Häufigkeit)) %>%
  slice(1:10) # Nehmen Sie nur die 10 häufigsten Verbindungen

# Visualisieren Sie das Netzwerk mit ggplot2
ggplot() +
  geom_point(data = top_verbindungen, aes(x = path, y = path), color = "grey") +
  geom_line(data = top_verbindungen, aes(x = path, y = path), color = "red") +
  geom_text(data = top_verbindungen, aes(x = path, y = path, label = Häufigkeit), check_overlap = TRUE) +
  theme_minimal() +
  labs(title = "Häufigste Verbindungen zwischen Wikipedia-Artikeln",
       x = "", y = "") +
  theme(axis.title = element_blank())


