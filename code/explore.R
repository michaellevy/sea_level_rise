visColors = c(org = "dodgerblue", proj = "firebrick")
library(tidyverse)
library(stringr)
library(visNetwork)
edges <- read_csv("data/EdgelistIn.csv")
nodes <- read_csv("data/Attributes.csv")

nodes = nodes %>%
  # at$Entity has "1" or "2" prepended based on type. Remove that:
  mutate(Entity = str_sub(Entity, 2)) %>%
  rename(id = Entity)

edges = edges %>%
  rename(from = Org, to = Proj)

# Temporary subset to make rendering faster
# projtmp = head(unique(edges$to))
# edges = filter(edges, to %in% projtmp)
# nodes = filter(nodes, 
#               id %in% unique(c(projtmp, edges$from)))
###

# Get counties in single column, comma-separated format for visOptions
# Pull out the county columns, look for "1" entries, and replace with colnames
counties = select(nodes, Alameda:`Online Only`) 
presence = which(counties == 1, arr.ind = TRUE)
counties[presence] = colnames(counties)[presence[, 2]]
# Collapse non-empty columns with comma separator
nodes$Counties = apply(counties, 1, function(x) paste(x[!is.na(x)], collapse = ", "))


nodes = nodes %>%
  mutate(label = id,
         color = visColors[BiType],
         shape = c("box", "diamond")[BiType]
  )


visNetwork(nodes, edges) %>%
  visNodes(shadow = TRUE) %>%
  visEdges(shadow = TRUE
           , color = "gray"
  ) %>%
  visOptions(
    # select by county
    selectedBy = list(variable = "Counties", multiple = TRUE)
    # select by ID and highlight associated orgs/projects
    , nodesIdSelection = TRUE, highlightNearest = list(enabled = TRUE, degree = 1)
  ) 
