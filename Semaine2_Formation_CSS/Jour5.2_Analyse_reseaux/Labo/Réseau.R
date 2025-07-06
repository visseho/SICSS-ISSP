
## Installer manquants, puis charger
packages <- c("igraph","ggraph", "tidygraph", "ggplot2", "scales", "tidyverse")
invisible(lapply(packages, function(x) if (!requireNamespace(x, quietly = TRUE)) install.packages(x)))
invisible(lapply(packages, library, character.only = TRUE))

## Répertoire
setwd("C:\\Users\\yacin\\OneDrive - INRS\\0.0.2.Cours et charges\\Formation SICCS\\Workshops\\Démo")

## Charger les données
load("nodes_profs.Rda")
load("edgelist_profs.Rda")

## Définir l'objet réseau
g <- graph_from_data_frame(d = edgelist, vertices = nodes, directed = FALSE)

## Assign node attributes
V(g)$scrape_name <- nodes$scrape_name
V(g)$centre <- nodes$centre
V(g)$works_count <- nodes$works_count
V(g)$cited_by_count <- nodes$cited_by_count

## Définir les poids
E(g)$weight <- edgelist$weight
E(g)$scaled_weight <- rescale(E(g)$weight, to = c(0.5, 10))

## 
dev.new()
ggraph(g, layout = "fr") +  # Force-directed layout
  geom_edge_link(aes(width = scaled_weight), color = "gray") +
  geom_node_point(aes(size = works_count, fill = centre), 
                  shape = 21,  # Allows a fill & stroke
                  stroke = .5, color = "black", alpha = 0.9) +  # avec contour
  geom_node_text(aes(label = scrape_name), repel = TRUE, size = 3) +  # Labels
  scale_edge_width(range = c(0.5, 15)) +  # Edge weight 
  scale_edge_alpha(range = c(0.4, 1)) + # on ajout de la transparence
  theme_void() +
  labs(title = "Réseaux de collaboration INRS", subtitle = "Taille des Noeuds = Nombre total de publications, épaisseur des liens = Fréquence des collaborations")

## Mesures de centralité
?degree()
?betweenness()
?eigen_centrality()

centrality_df <- tibble(
  author_id = V(g)$name,  
  author_name = V(g)$scrape_name,  
  degree = degree(g, mode = "all"), 
  betweenness = betweenness(g, directed = F, normalized = TRUE), 
  eigenvector = eigen_centrality(g, directed = F)$vector
  )



## Allons voir le top 10 de chaque

measures <- c("degree", "betweenness", "eigenvector")

top10_df <- measures %>%
  set_names(paste0("top10_", .)) %>%             # va donner les noms de colonnes
  map_dfc(
    ~ centrality_df %>%
      slice_max(order_by = .data[[.x]], n = 10, with_ties = FALSE) %>%  
      pull(author_name)
  )

top10_df



# Community
community <- cluster_louvain(g)

# Extract memberships
community_df <- tibble(
  author_id = V(g)$name,
  author_name = V(g)$scrape_name,
  community = membership(community)
)

# Identify isolates (degree == 0)
isolates <- V(g)[degree(g) == 0]

# Assign a new community ID for isolates (e.g., max community ID + 1)
if (length(isolates) > 0) {
  new_community_id <- max(community_df$community) + 1  # Next available community ID
  community_df <- community_df %>%
    mutate(community = ifelse(author_id %in% isolates$name, new_community_id, community))
}

# Convert community to factor for visualization
community_df$community <- as.factor(community_df$community)

# Assign updated community values back to graph nodes
V(g)$community <- community_df$community


ggraph(g, layout = "fr") +  # Force-directed layout
  geom_edge_link(aes(width = scaled_weight), color = "gray") +
  geom_node_point(aes(size = works_count, color = community, shape = centre), alpha = 0.9) +  # avec contour
  geom_node_text(aes(label = scrape_name), repel = TRUE, size = 3) +  # Labels
  scale_edge_width(range = c(0.5, 15)) +  # Edge weight 
  scale_edge_alpha(range = c(0.4, 1)) + # on ajout de la transparence
  theme_void() +
  labs(title = "Réseaux de collaboration INRS", subtitle = "Taille des Noeuds = Nombre total de publications, épaisseur des liens = Fréquence des collaborations")

