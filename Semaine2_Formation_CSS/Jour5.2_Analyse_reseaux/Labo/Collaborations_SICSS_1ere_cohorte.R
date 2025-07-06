##
# Définir votre adresse mail pour l’API OpenAlex (remplacez par votre propre adresse)
Sys.setenv(OPENALEX_MAILTO = "xxx@xxx.co")

# 1. Liste des packages requis
pkgs <- c(
  "openalexR",  # interface avec l’API OpenAlex
  "dplyr",      # manipulation de données
  "purrr",      # programmation fonctionnelle pour map(), etc.
  "tidyr",      # transformation et mise en forme de données
  "igraph",     # construction et analyse de graphes
  "tidygraph",  # intégration de graphes avec le tidyverse
  "ggraph"      # visualisation de graphes avec ggplot2
)

# 2. Installation des packages manquants
missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  install.packages(missing)
}

# 3. Chargement de tous les packages
lapply(pkgs, library, character.only = TRUE)


### Le manuel d'OpenAlexR !!!!

# --- Recherche des auteurs dans OpenAlex ---

# Voici une liste contenant le nom des participants de la première cohorte de SICSS (2017)
L <- c(
  "Vissého Adjiwanou", "Kathryn Albrecht", "Abdullah Almaatouq",
  "Lisa P. Argyle", "Elliott Ash", "Joshua Becker", "Anjali Bhatt",
  "Moritz Büchi", "Bo Cowgill", "Anna Filippova", "Connor Gilroy",
  "Ian Gray", "Jeffrey Jacobs", "Ridhi Kashyap", "Antje Kirchner",
  "Peter Krafft", "Molly Lewis", "Charlotte Lloyd", "Allison Morgan",
  "Matti Nelimarkka", "Kivan Polimis", "Ethan Porter",
  "Maria Y. Rodriguez", "Hirokazu Shirado", "Rochelle Terman",
  "Adaner Usmani", "Michael Yeomans"
)

# 2. Appel API unique pour récupérer tous les auteurs correspondant à la recherche textuelle
all_hits <- oa_fetch(
  entity               = "authors",
  `display_name.search` = L,
  per_page             = 200,
  output               = "tibble",
  verbose              = FALSE
)

## Ici je vais chercher Manuellement "Tong Wang" pour me simplifier la tâche, car il y avait beaucoup d'autrices avec le même nom.
# Normalement on évite ce genre d'étapes manuelles et on trouve des solutions reproductibles!
TW <- oa_fetch(
  entity = "authors",
  "https://openalex.org/A5100450972")

# 3. Filtrer pour ne garder que les correspondances exactes,
#    puis sélectionner la ligne par nom ayant le score de pertinence le plus élevé
best_by_score <- all_hits %>%
  filter(display_name %in% L) %>%
  group_by(display_name) %>%
  slice_max(order_by = relevance_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  bind_rows(TW) %>%
  transmute(
    query_name    = display_name,     # nom recherché
    openalex_id   = id,               # identifiant OpenAlex de l’auteur
    matched_name  = display_name,     # nom exact retourné
    works_count   = works_count,      # nombre total de publications
    relevance     = relevance_score,  # score de pertinence
    works_api_url = works_api_url     # URL pour récupérer les œuvres
  )

# 4. Conserver l’ordre original de L et joindre les résultats
results <- tibble(query_name = L) %>%
  left_join(best_by_score, by = "query_name")

print(results)


# --- Récupération des articles publiés de chaque auteur ---

all_works <- results %>%
  mutate(
    works_df = map(
      works_api_url,           # pour chaque URL d’article
      ~ oa_request(.x) %>%     # exécuter la requête
        oa2df("works")         # convertir en data.frame
    )
  )

df <- all_works %>%
  select(query_name, works_df) %>%  # ne garder que le nom de requête et le data.frame produit
  unnest(works_df)                  # déplier en une seule grosse table


# --- Construction du réseau de co-citation ---

# 1. Extraire la liste unique des ID de publications
focal_ids <- df %>% pull(id) %>% unique()

# 2. Créer la liste d’arêtes "from → to" pour les citations internes
edge_list <- df %>%
  select(id, referenced_works) %>%   # ne conserver que l’ID et la liste des références
  unnest_longer(referenced_works) %>%# transformer chaque référence en ligne séparée
  rename(
    from = id,
    to   = referenced_works
  ) %>%
  filter(
    from %in% focal_ids,             # ne garder que si la source est dans l’échantillon
    to   %in% focal_ids              # et si la cible aussi
  )

# Inspection rapide
print(edge_list)


# --- Attributs des nœuds (auteurs) ---

# 1. Tibble de tous les nœuds uniques
nodes <- tibble(node = unique(c(edge_list$from, edge_list$to)))

# 2. Joindre le nom de l’auteur correspondant à chaque nœud
node_attrs <- nodes %>%
  left_join(
    df %>% select(id, query_name),
    by = c("node" = "id")
  ) %>%
  rename(author_name = query_name)  # renommage pour plus de clarté

# Inspection rapide
print(node_attrs)

# --- Création et visualisation du graphe ---

# 1. Construire un objet tbl_graph pour ggraph
graph <- tbl_graph(
  nodes    = node_attrs %>% rename(name = node),
  edges    = edge_list %>% rename(from = from, to = to),
  directed = TRUE
)

# 2. Fixer la graine pour une mise en page reproductible
set.seed(2025)

## Ouvrir l'interface de viz
dev.new()

# 3. Tracer avec ggraph
ggraph(graph, layout = "fr") +
  # Arêtes en gris clair
  geom_edge_link(alpha = 0.4, colour = "grey70") +
  # Nœuds colorés par auteur
  geom_node_point(aes(color = author_name), size = 4) +
  # Étiqueter les nœuds les plus cités (top 10 % en degré entrant)
  geom_node_text(
    aes(label = ifelse(
      degree(graph, mode = "in") > quantile(degree(graph, mode = "in"), 0.9),
      author_name, NA
    )),
    repel = TRUE,
    size = 3
  ) +
  # Thème épuré adapté aux graphes
  theme_graph() +
  labs(color = "Auteur") +
  ggtitle("Réseau de co‐citation des publications des auteurs sélectionnés")


# 4. Compter le nombre de composantes déconnectées
composantes <- components(graph)               # calcul des composantes
nombre_composantes <- composantes$no           # nombre total de composantes
cat("Nombre de composantes déconnectées :",
    nombre_composantes, "\n")

# Taille de chaque composante
cat("Taille des composantes :", sort(composantes$csize), "\n")


### Puisque on préfère travailler avec des graphes connectés, selectionnons le graphe avec la plus grande composante
comp <- components(g, mode = "weak")  

# 5.2 Identifier l’indice de la plus grande composante
idx_gigante <- which.max(comp$csize)   

# 5.3 Sélectionner les sommets de cette composante
sommets_gigante <- V(g)[comp$membership == idx_gigante]  

# 5.4 Construire le sous-graphe induit par ces sommets
g_gigante <- induced_subgraph(g, vids = sommets_gigante)  


# Récupérer les IDs des sommets dans l’ordre de V()
vids <- V(g_gigante)$name

# Pour chaque attribut à ajouter, faire un match() et l’assigner
attrs <- c("author_name", "works_count", "relevance")
for (attr in attrs) {
  values <- node_attrs[[attr]][match(vids, node_attrs$node)]
  g_gigante <- set_vertex_attr(
    g_gigante,
    name  = attr,
    value = values
  )
}

# Vérification rapide
vertex_attr_names(g_gigante)       # listes des attributs disponibles
head(as_data_frame(g_gigante, what = "vertices"))
set.seed(2025)


# 5.5 convertir en tbl_graph pour réutiliser ggraph + tidygraph
graph_gigante <- as_tbl_graph(g_gigante)


# Tracer la plus grande composante (objet tbl_graph `graph_gigante`)
ggraph(graph_gigante, layout = "fr") +
  # Arêtes en gris clair, transparence modérée
  geom_edge_link(alpha = 0.4, colour = "grey70") +
  # Nœuds colorés par nom d’auteur, taille fixe
  geom_node_point(aes(color = author_name), size = 4) +
  # Étiqueter les nœuds dont le degré entrant est dans le top 10 %
  geom_node_text(
    aes(label = ifelse(
      degree(as.igraph(graph_gigante), mode = "in") >
        quantile(degree(as.igraph(graph_gigante), mode = "in"), 0.9),
      author_name, NA
    )),
    repel = TRUE,
    size = 3
  ) +
  # Thème épuré pour graphes
  theme_graph() +
  labs(color = "Auteur") +
  ggtitle("Composante géante du réseau de co‐citation")

### Petit exercice
# Quelle est la densité de ce graphe?
# Réfléchissez à des mesures de centralité pertinentes
# Identifiez les acteurs clés pour la connectivité de ce graphe



### Ok nous venons de produire/travailler avec un réseau de co-citation au niveau des articles scientifiques 
## Alternative : construire un réseau de co‐citation au niveau des auteurs

# --- À partir de edge_list (citations d’articles), créer un réseau pondéré de co‐citations entre auteurs ---

# 1. edge_list contient :
#    • from : ID de l’œuvre citante  
#    • to   : ID de l’œuvre citée

# 2. Ajouter le nom de l’auteur à chaque extrémité
edge_authors <- edge_list %>%
  # joindre le nom de l’auteur “source”
  left_join(
    node_attrs %>% 
      select(node, author_name) %>% 
      rename(from = node, author_from = author_name),
    by = "from"
  ) %>%
  # joindre le nom de l’auteur “cible”
  left_join(
    node_attrs %>% 
      select(node, author_name) %>% 
      rename(to = node, author_to = author_name),
    by = "to"
  )

# 3. Agréger pour obtenir un réseau dirigé et pondéré d’auteurs
author_network <- edge_authors %>%
  # ne conserver que les citations où les deux auteurs sont dans l’échantillon
  filter(!is.na(author_from), !is.na(author_to)) %>%
  # compter le nombre de citations de author_from vers author_to
  count(author_from, author_to, name = "weight") %>%
  ungroup()

# 4. Aperçu du réseau auteur × auteur
print(author_network)

# 5. Construire l’objet igraph dirigé pondéré
library(igraph)
g_authors <- graph_from_data_frame(
  d = author_network,
  directed = TRUE,
  vertices = tibble(name = unique(c(author_network$author_from, author_network$author_to)))
)
# assigner le poids des arêtes
E(g_authors)$weight <- author_network$weight

# 6. Vérifier dimensions du graphe
cat("Nombre d’auteurs (nœuds) :", vcount(g_authors), "\n")
cat("Nombre de liaisons (arêtes)  :", ecount(g_authors), "\n")


# --- Visualisation avec taille des nœuds ∝ nombre de travaux citants dans l’échantillon ---

# 1. Estimer le nombre de travaux “source” par auteur
works_per_author <- edge_authors %>%
  distinct(from, author_from) %>%  # un enregistrement “from” par auteur
  count(author_from, name = "num_works")

# 2. Ajouter cet attribut au graphe igraph
vids <- V(g_authors)$name
num_works_vec <- works_per_author$num_works[match(vids, works_per_author$author_from)]
num_works_vec[is.na(num_works_vec)] <- 0  # auteurs sans travaux dans edge_list
g_authors <- set_vertex_attr(
  g_authors,
  name  = "num_works",
  value = num_works_vec
)

# 3. Transformer en tbl_graph pour ggraph
library(tidygraph)
library(ggraph)
graph_authors_tbl <- as_tbl_graph(g_authors)

# 4. Tracé final
set.seed(2025)

ggraph(graph_authors_tbl, layout = "fr") +
  # arêtes : épaisseur selon le nombre de co‐citations
  geom_edge_link(aes(width = weight), colour = "grey70", alpha = 0.4,
                 arrow = arrow(length = unit(3, "mm"), type = "closed")) +
  scale_edge_width(range = c(0.5, 4), name = "Co-citations") +
  
  # nœuds : taille selon num_works, couleur purement esthétique
  geom_node_point(aes(size = num_works, color = name)) +
  
  # étiquettes : toujours afficher le nom de l’auteur,
  # taille du texte proportionnelle au nombre de travaux
  geom_node_text(
    aes(label = name, size = num_works),
    repel = TRUE,
    show.legend = FALSE  # masquer la légende des textes
  ) +
  
  # échelle partagée pour la taille des points et du texte
  scale_size_continuous(
    range = c(3, 8),
    name  = "Travaux citants"
  ) +
  
  # supprimer la légende de la couleur
  guides(color = "none") +
  
  # thème épuré adapté aux graphes
  theme_graph() +
  ggtitle("Réseau de co-citations d’auteurs") +
  labs(edge_width = "Co-citations")



set.seed(2025)

ggraph(graph_authors_tbl, layout = "fr") +
  # boucles (self-loops) : épaisseur selon weight
  geom_edge_loop(aes(width = weight), colour = "grey70", alpha = 0.4) +
  # arêtes entre nœuds distincts
  geom_edge_link(aes(width = weight), colour = "grey70", alpha = 0.4) +
  scale_edge_width(range = c(0.5, 4), name = "Co-citations") +
  
  # nœuds : taille selon num_works, couleur purement esthétique
  geom_node_point(aes(size = num_works), color = "steelblue") +
  
  # étiquettes : nom de l’auteur, taille proportionnelle à num_works
  geom_node_text(
    aes(label = name, size = num_works),
    repel = TRUE,
    show.legend = FALSE
  ) +
  scale_size_continuous(range = c(3, 8), name = "Travaux citants") +
  
  # masquer seule la légende de la couleur
  guides(color = "none") +
  
  theme_graph() +
  ggtitle("Réseau de co-citations d’auteurs (avec boucles)")

