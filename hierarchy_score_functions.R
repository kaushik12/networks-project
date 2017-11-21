library(igraph)
library(sna)
library(ergm)
library(network)
library(intergraph)

data(sampson)
plot(samplike)

g <- asIgraph(samplk1)

# g <- graph_from_literal(1-+2,2-+4,2-+5,3-+6,3-+7,2-+3)
# plot(g)

## Betweenness Centrality based Hierarchy score

get_hierarchy_score_bw <- function(g){
  bw_centrality <- centr_betw(g,directed=FALSE)$res
  in_degrees <- centr_degree(g,mode = "in")$res 
  out_degrees <- centr_degree(g,mode = "out")$res
  bw_hierarchy_measure <- bw_centrality/sqrt((in_degrees + 1)*(out_degrees + 1))
  nodes <- vertex.attributes(g)$vertex.names  
  adj_matrix <- as_adj(g)
  hierarchy_matrix <- matrix(NA,length(nodes),length(nodes))
  for (i in seq_along(nodes)){
    neighboring_nodes <- neighbors(g,i)
    for (j in seq_along(neighboring_nodes)){
      hierarchy_matrix[i,neighboring_nodes[j]] <- bw_hierarchy_measure[i]/bw_hierarchy_measure[neighboring_nodes[j]]
    }
  }
  return(hierarchy_matrix)
}


## Page-Rank based Hierarchy score
get_hierarchy_score_pr <- function(g){
  
}
  
## HITS based Hierarchy score
get_hierarchy_score_hits <- function(g){
  
}

## Attraction Basin based Hierarchy score
get_hierarchy_score_ab <- function(g,alpha=2){
  
}


get_tree_from_hierarchy_score <- function(h,upper_cutoff=0.8,lower_cutoff=0.1){
  upper_cutoff <- upper_cutoff
  lower_cutoff <- lower_cutoff
  h[h > upper_cutoff] <- NA
  h[h < lower_cutoff] <- NA  
  
  ## Get Tree Structure
  
}
  







