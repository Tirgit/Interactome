# LOAD LIBRARIES
library(visNetwork)
library(geomnet)
library(igraph)
library(dplyr)

# LOAD CLEANED & PREPROCESSED FILES
edges <- readRDS("C:/Users/vrw657/Documents/Interactome_data/Clean/edges.rds")
nodes <- readRDS("C:/Users/vrw657/Documents/Interactome_data/Clean/nodes.rds")

# VISUALIZE NETWORK
visNetwork(nodes, edges)

# GROUP INFORMATION











# LES MISERABLES PLAY DATASET
data(lesmis)

nodes <- as.data.frame(lesmis[2])
colnames(nodes) <- c("id", "label")
nodes$id <- nodes$label

edges <- as.data.frame(lesmis[1])
colnames(edges) <- c("from", "to", "width")

visNetwork(nodes, edges)

graph <- graph_from_data_frame(edges, directed = FALSE)


# CLUSTERING
cluster <- cluster_louvain(graph)

cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)

#Create group column
nodes <- left_join(nodes, cluster_df, by = "label")
colnames(nodes)[3] <- "group"


visNetwork(nodes, edges)


# GROUPWISE
visNetwork(nodes, edges, width = "100%") %>%
  visIgraphLayout() %>%
  visNodes(
    shape = "dot",
    color = list(
      background = "#0085AF",
      border = "#013848",
      highlight = "#FF8000"
    ),
    shadow = list(enabled = TRUE, size = 10)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B")
  ) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             selectedBy = "group") %>% 
  visLayout(randomSeed = 11)


