Introduction
================
Kaushik Mohan

This project is an attempt to use (and improve) methods described in the paper by Muchnik, et.al. for extracting hierarchies from a network. The paper has been made available for reference in the respective folder within the repo. The end goal is to be able to use these techniques to create a knowledge tree from the Wikipedia EN article network which could be used to create a more structured online-reference/learning platform akin to a traditional Encyclopedia.

For the purpose of this project though, we use a smaller and more specific network of articles from HyperPhysics, a HTML Textbook (<http://hyperphysics.phy-astr.gsu.edu/hbase/hframe.html>). This textbook also has a more complete hyperlink structure compared to Wikipedia which should give us better results. Based on this network, we should be able to arrive at a tree structure which denotes the evolution of Physics literature and traditional structure in Physics instruction. The HyperPhysics page provides hierarchies based on content and some semantics which could be used as a reference to validate the results of the analysis.

As a work in progress at the end, we look at developing a better algorithm for extracting hierarchies using the idea of community detection to create each level of the hierarchy iteratively. The intution/hypothesis behind formulating such an approach is two-fold. Firstly, the previous methodologies identify hierarchies locally across all nodes to generate the full structure. Secondly, the cutoffs based on which hierarchical relationships are determined arbitrarily and tuned through validation. The proposed methodologies address these issues by using eigenvector based community detection to create the hierarchy through a top-down approach. By first identifying a community of nodes (topics in this sense) and their hierarchical relationships, we then dive deeper into each group to determine the hierarchies within. By iteratively going through this process, we hope to have a better and more intuitive hierarchical structure developing from the underlying network.

------------------------------------------------------------------------

#### Part 1: Scraping for Data

The first part of the project involves scraping through the HyperPhysics domain to create a network with pages as nodes and hyperlink between pages as edges. The webpage used here is HyperPhysics, a Physics and Math HTML textbook developed by Carl R. nave of Georgia State University. We use this because it's comprehensive coverage of most of physics and the linking structure between content pages. It also has a map of the content in some form which could provide validation to the extraction of trees from the link structure.

The url is <http://hyperphysics.phy-astr.gsu.edu/hbase/hframe.html>. We use the package 'rvest' which is similar to 'beautifulsoup' in Python used for web-scraping.

``` r
## We start with the first article in the Index, acceleration as the index page itself is a hframe and we are unable to parse it for links. 

url <- "http://hyperphysics.phy-astr.gsu.edu/hbase/acca.html"
webpage <- read_html(url)
nodes <- html_nodes(webpage,'a')
titles <- html_text(nodes)
links <- tolower(html_attr(nodes,"href"))
headings <- html_nodes(webpage,'h1')
h_regex <- regexpr(">(.*)<",as.character(headings[1]))
heading <- substr(as.character(headings[1]),(h_regex[1]+1),h_regex[1]+attr(h_regex,"match.length")-2)
heading
```

    ## [1] "Acceleration"

``` r
print("Links")
```

    ## [1] "Links"

``` r
links
```

    ##  [1] NA                          "vel2.html#c1"             
    ##  [3] "vect.html#veccon"          "vect.html#vec1"           
    ##  [5] "units.html#uni4"           "deriv.html#c1"            
    ##  [7] "mot.html#mot1"             "hframe.html"              
    ##  [9] "hph.html"                  "hph.html#mechcon"         
    ## [11] "javascript:history.go(-1)"

``` r
print("Titles")
```

    ## [1] "Titles"

``` r
titles
```

    ##  [1] ""                                              
    ##  [2] "velocity"                                      
    ##  [3] "vector"                                        
    ##  [4] "vector addition"                               
    ##  [5] "units"                                         
    ##  [6] "derivative"                                    
    ##  [7] "Motion equations when acceleration is constant"
    ##  [8] "Index"                                         
    ##  [9] " HyperPhysics"                                 
    ## [10] "     Mechanics "                               
    ## [11] "Go Back"

We can see all the links and page titles extracted from the HTML page. Next, we need to traverse through these URLs to find links in those iteratively. We keep track of the URLs visted and also include some URLs that we do not wish to look into as they do not have topic related content. In the below chunk, we write the function to extract all the links from a page while simultaneously cleaning the data for further use. We exclude javascript links, multimedia links and also stop the parsing from entering into an infinite loop when it reaches an index directory (<http://hyperphysics.phy-astr.gsu.edu/hbase/Kinetic/>) where sorting by Name, Size etc. create a slightly different URL. We also exclude URLs under the class/ subdomain as these don't contain Physics content but are structured courses that have been created within the hyperphysics domain.

    ## $main_page
    ## [1] "electric/lightncon.html"
    ## 
    ## $main_page_title
    ## character(0)
    ## 
    ## $page_links
    ## [1] "emcon.html"
    ## 
    ## $page_titles
    ## [1] "Electricity and Magnetism"

We note that the results now are lot cleaner compared to the raw output from before. Now that we have written a function to extract urls and titles, we will create a data structure for storing this information for all the pages we visit.

``` r
## Storing the page URL and title in data frame page_details
page_details <- data.frame(matrix(ncol=2),stringsAsFactors = FALSE)
colnames(page_details) <- c("url","title")

## Initializing a data frame to store the edge list which would be the hyperlinks
edge_list <- data.frame(matrix(ncol=2),stringsAsFactors = FALSE)
colnames(edge_list) <- c("from_url","to_url")
```

We also define functions to add new pages and edges to the respective data frames.

``` r
## Function to add new page details
add_page_details <- function(l,df){
  existing_urls <- df$url
  existing_titles <- df$title
  new_urls <- c(l$main_page,l$page_links)
  new_titles <- c(l$main_page_title,l$page_titles)
  ## Checking if Title is as per page and not the hyperlink text
  if(!(df[df$url == l$main_page,2] == l$main_page_title)){
    df[df$url == l$main_page,2] <- l$main_page_title
  }
  ## Removing duplicates before appending
  new_titles <- new_titles[!new_urls %in% existing_urls]
  new_urls <- new_urls[!new_urls %in% existing_urls]
  temp_df <- data.frame(url=new_urls,title=new_titles)
  return(rbind(df,temp_df))
}

## Function to get new edge information
get_edges <- function(l){
  if(length(l$page_links) > 0){
    return(data.frame(from_url=l$main_page,to_url=l$page_links))
  }else{
    return(NULL)
  }
}
```

We now use the function defined above to go over all the URLs and store the network structure.

We find that there are 295 broken URLs. The next step is to try and fix these before scraping through them again. Through some manual inspection, we find that most broken URLs are of the form xxx/yyy/zzz.html where the correct URL is supposed to be yyy/zzz.html. We also note that some of them aren't broken and do infact work.

    ## [1] 295

    ## [1] 40

After correcting for these two cases, we are left with 40 broken URLs. Fixing these required a lot of manual checking. One of the ways of identifying these was to add the parent URL domain to check if that works and we were able to reduce the number of errors with this.

    ## [1] 20

We ignore the 20 remaining cases, as it was difficult to manually identify the correct URLs for these. Finally, we visit the unvisited fixed URLs and add the information to the dataset.

Finally, we check the dataset once more to remove self-edges and any duplicates.

``` r
## Removing self edges from edge_list and 1st row with an NA
edge_list <- edge_list[-1,]
edge_list <- edge_list[-which(edge_list$from_url == edge_list$to_url),]

## Checking for and removing duplicate entries in page_details and edge_list
edge_list <- edge_list[!duplicated(edge_list), ]
page_details <- page_details[!duplicated(page_details),] ## URL and Title both match
page_details <- page_details[!duplicated(page_details$url),] ## Only URL duplicated
```

We store these final Edge List and Page Details data frames as CSVs as a backup, for easier access in the following sections and to work with a clean environment.

``` r
setwd("code/Data")
write.csv(page_details,file="page_details.csv",row.names = FALSE)
write.csv(edge_list,file="edge_list.csv",row.names = FALSE)
```

We are finally left with 21,057 edges and 2931 nodes. We use this network data for further analysis in the following sections.

------------------------------------------------------------------------

#### Part 2: Network Stats

In this section, we use this data from the scraping exercise to create a network object and perform some basic network analysis. We look at presenting a high-level overview of the network through some of it's node level and network level statistics in order gain an understanding of the underlying structure.

Importing the saved datasets with edge list and nodal attributes.

Creating the network objects and setting the vertex attributes. When the network is created, it numbers the nodes alphabetically based on the vertex name (in this case URL). We get these and store them for future reference.

``` r
## Creating Network object
hyperphysics_network <- network(as.matrix(edge_list),matrix.type="edgelist",directed = TRUE)

node_names <- get.vertex.attribute(hyperphysics_network,"vertex.names") 
set.vertex.attribute(hyperphysics_network,"Titles",page_details$title[order(page_details$url)])

## IGraph object
hyperphysics_graph <- asIgraph(hyperphysics_network)

## Storing the Vertex URLs and Titles in the order they appear in the graph object
vertex_urls <- vertex_attr(hyperphysics_graph,"vertex.names")
vertex_titles <- vertex_attr(hyperphysics_graph,"Titles") 

## Some additional clean up of the data (remove trailing spaces and /n)
vertex_titles <- gsub("\n","",vertex_titles)
vertex_titles <- trimws(vertex_titles,which = "both")
V(hyperphysics_graph)$Titles <- vertex_titles
```

##### Sample of the dataset

``` r
head(page_details,5)
```

    ##          url                   title
    ## 1  acca.html            Acceleration
    ## 2  vel2.html                Velocity
    ## 3  vect.html Basic Vector Operations
    ## 4 units.html          Physical Units
    ## 5 deriv.html          The Derivative

``` r
head(edge_list,5)
```

    ##    from_url     to_url
    ## 1 acca.html  vel2.html
    ## 2 acca.html  vect.html
    ## 3 acca.html units.html
    ## 4 acca.html deriv.html
    ## 5 acca.html   mot.html

###### Nodes, Edges and Density

``` r
paste("No. of Nodes: ",dim(page_details)[1])
```

    ## [1] "No. of Nodes:  2931"

``` r
paste("No. of Edges: ",dim(edge_list)[1])
```

    ## [1] "No. of Edges:  21058"

``` r
network_density <- dim(edge_list)[1]/(dim(page_details)[1]*(dim(page_details)[1]-1))
paste("Density: ",round(network_density,5))
```

    ## [1] "Density:  0.00245"

###### A look at the network sample

``` r
set.seed(5)
sample_nodes <- sample(V(hyperphysics_graph),750,replace = FALSE)
sub_hyperphysics_graph <- induced_subgraph(hyperphysics_graph,sample_nodes)
plot(sub_hyperphysics_graph,edge.arrow.size=.3,vertex.size=3,vertex.label=NA)
```

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

From the plot of an induced sub-graph of 750 nodes, we can already see some clusters closely interconnected within in the network.

###### Degree Distribution

The first network statistics we look into furhter is the degree distribution. We want to verify if the distribution follows a power law as with several similar networks extracted from the web.

``` r
## Degree Distribution
in_degree <- igraph::degree(hyperphysics_graph,mode="in")
out_degree <- igraph::degree(hyperphysics_graph,mode="out")

## Avg. Degrees
paste("Mean In-Degree: ",round(mean(in_degree),4))
```

    ## [1] "Mean In-Degree:  7.1846"

``` r
paste("Mean Out-Degree: ",round(mean(out_degree),4))
```

    ## [1] "Mean Out-Degree:  7.1846"

It is a bit strange and interesting that the Avg. In and Out Degrees match perfectly.We then plot the degree distribution in the log-log scale to see if it is linear denoting a scale-free distribution.

``` r
## Plots of Degree Distribution
par(mfrow=c(1,2))
plot(log(as.numeric(row.names(table(in_degree)))),log(table(in_degree)),main="Log-log plot of In-Degree Dist.",xlab="log(In-Degree)",ylab="log(frequency)",type="p",pch=1)

plot(log(as.numeric(row.names(table(out_degree)[-1]))),log(table(out_degree)[-1]),main="Log-log plot of Out-Degree Dist.",xlab="log(Out-Degree)",ylab="log(frequency)",type="p",pch=1)
```

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-19-1.png)

We note that the degree distributions is almost perfectly linear in the log-log scale signifying preferential attachment in terms of the network structure. The out-degree distribution though starts off with a lower frequency of nodes with fewer degress. This perhaps indicates the nature of the textbook where most of the pages are linked with atleast a few other pages and very few pages have no links outward.

``` r
in_deg_est <- lm(log(table(in_degree))~log(as.numeric(row.names(table(in_degree)))))
out_deg_est <- lm(log(table(out_degree)[-1])~log(as.numeric(row.names(table(out_degree)[-1]))))

in_deg_est$coefficients[2]
```

    ## log(as.numeric(row.names(table(in_degree)))) 
    ##                                     -1.36514

``` r
out_deg_est$coefficients[2]
```

    ## log(as.numeric(row.names(table(out_degree)[-1]))) 
    ##                                         -1.551737

The estimated co-efficient or the power in the power-law form for the degree distributions are -1.365 and -1.552 for the In and Out degrees respectively. That is the in and out degrees roughly follow the following distributions.

![Degree Distribution](images/degrees.png)

###### Geodesic Distance Distribution

Next, we look at the distrbution of the Geodesic distance between nodes. This measure gives us a sense of how close the nodes are from each other and along side a scale-free degree distribution, a short average geodesic distance (or network diameter) would indicate a small-world nature of the network as defined by Watts and Strogatz (1998).

``` r
## Geodesic Distances

geodesic_distances <- geodist(hyperphysics_network,count.paths = FALSE)
avg_path_length <- average.path.length(hyperphysics_graph)
dist_path_length <- path.length.hist(hyperphysics_graph)
paste("Average Geodesic Distance: ",round(avg_path_length,3))
```

    ## [1] "Average Geodesic Distance:  5.862"

``` r
options(scipen=3)
plot(c(1:length(dist_path_length$res)),dist_path_length$res,main="Distribution of Geodesic Distances",xlab = "Geodesic Path length",ylab="Count",type='o',pch=1)
```

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-21-1.png)

We do observe that the Network Diameter small at ~5.86 and this is re-inforced by the above plot of the geodesic distance distribution.

###### Centralization

The key network and nodal measure for our analysis is the Centrality and Centralization measures.The overall network centralization is 0.236. This gives us a sense of the centralization of the network relative to a star configuration which is maximally centrlized.

``` r
## Centrality Measures
betweenness_centrality <- centr_betw(hyperphysics_graph)
paste("Network Centralization: ",round(betweenness_centrality$centralization,3))
```

    ## [1] "Network Centralization:  0.236"

``` r
page_rank_centrality <- page_rank(hyperphysics_graph,algo="prpack")
```

------------------------------------------------------------------------

#### Part 3: Extracting hierarchies

In this section, we create the functions to extract hierarchies from the network and use these on the HyperPhysics article network. Four different hierarchy extraction methods are explored, namely

1.  Betweenness Centrality based
2.  Page-Rank based
3.  Attraction Basin
4.  Eigenvector based

The specific formulae and methodology are presented in following sub-sections where we explore each of these methodologies in detail. The first three methods listed above are as defined in the Muchnik, et.al. paper. The last methodology is an independent exploration of a different way to extract hierarchy which takes into account the clusters of topics in the network and extract the hierarchy iteratively starting from the highest-level allthe way down to the leaf nodes.

``` r
## We create the adjacency matrix and an undirected edgelist which is needed for hierarchy extraction

hyperphysics_adjacency <- as_adj(hyperphysics_graph,type = "both")
hyperphysics_edgelist <- as_edgelist(hyperphysics_graph)
undirected_edgelist <- data.frame(rbind(hyperphysics_edgelist,cbind(hyperphysics_edgelist[,2],hyperphysics_edgelist[,1])))
undirected_edgelist <- undirected_edgelist[!duplicated(undirected_edgelist), ]
```

###### Overview

To understand the core idea behind extracting hierarchies from a network, we look at a regular tree to start with. From the below sample tree marked by it's corresponding betweenness centralities, we can make the following observations:

-   Greater centrality corresponds to a higher level in the hierarchy
-   Nodes at the same level of hierarchy have similar centrality
-   Large difference in centrality for nodes unconnected with by an edge in the hierarchy

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-24-1.png)

These three fundamental ideas form the basis of hierarchy extraction in each of the following methods. The overall process of hierarchy extraction can be summarized as the following set of steps

1.  Define a suitable hierarchy score for each node A hierarchy score for a node is a measure such as it's betweenness centrality which can be used to compare two nodes to determine the hierarchical relation between them. For a regular node such as the above example, we can simply use the betweenness centrality as the graph is regular. But in the case of real-world networks such as the one we are dealing with, every node need not have the same degree and hence we look at nodal measures which are scaled by their respective degrees.

2.  Compare scores for 2 neighboring nodes (in the underlying undirected network) We then compare the scores of two nodes in the underlying network and determine if one is higher in the hierarchy if the ratio of the scores is between a lower and an upper threshold and it has a higher score. This does not depend on the direction of the edge between the two nodes in the underlying network.

3.  Define the right cutoffs The choice of cutoffs affects the hierarchical network output. A high upper cutoff would create hierarchical relations between two similar nodes at the same level in the hierarchy such as two disjoint topics in physics such as Kinetics and Optics, say. A low or no lower cutoff would include a hierarchical relation between two nodes connected in the underlying network but not necessarily directly related in a hierarchical sense. It may leed to creation of triangles in the hierarchy with a node being connected to several nodes at higher levels in the hierarchy.

The paper performs the analysis on the Wikipedia article network where it's corresponding Wikipedia Category network was used to validate the results of the methodologies. This allowed the authors to optimize the cutoffs based on correct classification of the hierarchical relations with respect to the category network. In our case, we do not have a hierarchical network to validate against and hence try out different levels of cutoff. As a aprt of continuing work on this, I hope to survey educators in the Physics community to help classify the estimated hierarchical relations to validate the results.

Before going into the methodologies we define some helper functions which help create a hierarchical graph from the respective scores and also functions for plotting purposes.

``` r
get_hierarchy_graph <- function(h_scores,edgelist,upper,lower){
  
  ## Calculating Score ratio for all edges with error handling for nodes with a 0 score
  score_ratio <- h_scores[edgelist[,1]]/ifelse(h_scores[edgelist[,2]]==0,0.00001,h_scores[edgelist[,2]])
  
  ## Creating a matrix with edges, scores for each node and the ratio of scores
  hierarchy_matrix <- cbind(edgelist,h_scores[edgelist[,1]], h_scores[edgelist[,2]], score_ratio)
  colnames(hierarchy_matrix) <- c("V1","V2","Score_V1","Score_V2","Score_Ratio")
  
  ## Eliminating rows where score ratio is not between the cutoffs
  hierarchy_matrix <- hierarchy_matrix[hierarchy_matrix$Score_Ratio > lower & hierarchy_matrix$Score_Ratio <= upper,]
  
  ## Determining the higher and lower nodes based on their individual scores
  hierarchy_matrix <- cbind(hierarchy_matrix,ifelse(hierarchy_matrix[,3] > hierarchy_matrix[,4],hierarchy_matrix[,1],hierarchy_matrix[,2]))
  hierarchy_matrix <- cbind(hierarchy_matrix,ifelse(hierarchy_matrix[,3] > hierarchy_matrix[,4],hierarchy_matrix[,2],hierarchy_matrix[,1]))
  colnames(hierarchy_matrix) <- c("V1","V2","Score_V1","Score_V2","Score_Ratio","Higher_Node","Lower_Node")

  ## The Last two columns are the directional edges in the hierarchy 
  hierarchy_edgelist <- as.matrix(hierarchy_matrix[,c(6,7)])
  hierarchy_network <- network(hierarchy_edgelist,matrix.type="edgelist",directed = TRUE)
  hierarchy_graph <- asIgraph(hierarchy_network)
  return(hierarchy_graph)
}
```

get\_sub\_tree is a function to sample a graph from the hierarchy where, given a staring node, we sample *n* nodes one level lower and continue on till the given number of levels. We also define two helper functions to wrap the titles of the nodes and create a layout for a cleaner visualization.

``` r
## A function to sample a sub-graph from ther hierarchy graph.

## start
## get n samples of lower nodes
## for each of these get n samples of lower nodes
## 


get_sub_tree <- function(st,levels=1,el,n=10,cur_level=1){
  reduced_el <- matrix(NA,ncol=2)
  down_nodes <- NA
  down_nodes <- c(down_nodes,sample(el[el[,1] == st,2],min(n,length(el[el[,1] == st,2]))))
  down_nodes <- down_nodes[!is.na(down_nodes)]
  reduced_el <- rbind(reduced_el,el[which(el[,2] %in% down_nodes & el[,1] == st),])
  if(cur_level <= levels){
    for(i in seq_along(down_nodes)){
      reduced_el <- rbind(reduced_el,get_sub_tree(st=down_nodes[i],levels = levels,el = el,n = n, cur_level =(cur_level+1)))
    }
  }
  reduced_el <-  reduced_el[-1,]
  reduced_el <-  reduced_el[!duplicated(reduced_el),]
  return(reduced_el)
}

wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
                        paste(strwrap(x, width=width), collapse="\n")
                        }))
}

tree_plot_func <- function(graph) {

  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel= TRUE,size=2) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 2))
  
  print(plot)
}
```

###### Betweenness Centrality based hierarchy extraction

As the name suggests, in the method we use a simple betweenness centrality based hierarchy score to determine hierarchical relation between the nodes. As discussed above, in order to scale the score to account for the non-regular network structure, we define a hierarchy score as below:

![Betweenness Centrality Score](images/betweenness_centrality.png)

``` r
bw_hierarchy_score <- betweenness_centrality$res/(sqrt((1+in_degree)*(1+out_degree)))

upper_cutoff <- 0.8
lower_cutoff <- 0.2

bw_hierarchy_graph <- get_hierarchy_graph(bw_hierarchy_score,undirected_edgelist,upper_cutoff,lower_cutoff)
bw_hierarchy_edgelist <- as_edgelist(bw_hierarchy_graph)

bw_hierarchy_graph_centrality <- centr_betw(bw_hierarchy_graph)
max_centrality_node <- which(bw_hierarchy_graph_centrality$res == max(bw_hierarchy_graph_centrality$res),arr.ind = TRUE)


## plotting sub-tree
set.seed(1)
g <- graph_from_edgelist(get_sub_tree(max_centrality_node,2,bw_hierarchy_edgelist,n=10),directed = TRUE)
# g <- bw_hierarchy_graph
V(g)$titles <- strtrim(as.character(vertex_titles[V(g)]),25)
iso <- V(g)[igraph::degree(g)==0]
g2 <- delete_vertices(g, iso)
V(g2)$node_label = wrap_strings(V(g2)$titles, 10)
tree_plot_func(g2)
```

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-27-1.png)

###### Page-Rank based hierarchy extraction

The idea behind page-rank based hierarchy scoring is similar to betweenness based approach. Page-Rank algorithm gives us a centrality measure for a node that is weighted by the centrality measures of it's neighbours. As this measure is already weighted, we don't need to scale it by the degree of the node. Page-Rank centrality is defined as below:

![Page-Rank Centrality](images/page_rank_centrality.png)

$$PR(u) = \\sum\_{v\\in B\_u}\\frac{PR(v)}{L(v)} $$

*P**R*(*u*): Page-Rank for node *u*
*B*<sub>*u*</sub>: The set containing all nodes linking to node *u*
*L*(*v*): No. of edges from node *v*

``` r
pr_hierarchy_score <- page_rank_centrality$vector

upper_cutoff <- 0.8
lower_cutoff <- 0.2

pr_hierarchy_graph <- get_hierarchy_graph(pr_hierarchy_score,undirected_edgelist,upper_cutoff,lower_cutoff)
pr_hierarchy_edgelist <- as_edgelist(pr_hierarchy_graph)

## plotting sub-tree
set.seed(1)
g <- graph_from_edgelist(get_sub_tree(2177,2,pr_hierarchy_edgelist,n=4))
V(g)$titles <- strtrim(as.character(vertex_titles[V(g)]),30)
iso <- V(g)[igraph::degree(g)==0]
g2 <- delete_vertices(g, iso)
V(g2)$node_label = wrap_strings(V(g2)$titles, 10)
tree_plot_func(g2)
```

    ## Multiple parents. Unfolding graph

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`na.rm`)

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-28-1.png)

###### Attraction Basin based hierarchy extraction

This is a method developed by the authors of the paper. The purpose behind it was to create a measure that can be estimated locally and does not nequire for the entire network to be analysed. For example, to calculate betweenness centrality for a node, all the shortest paths between all the nodes have to be calculated. Similarly, page-rank centrality being iterative, has to be estimated for all the nodes in the network together. For large networks, such computations can be very expensive and hence there is a need for a local measure of centrality.

The authors define the score as comparision between the weighted fraction of the network from which the node can be reached to the weighted fraction of the network that can be reached from each node. In a sense, the quantity summarizes the "flow"" of information/references through a node. It takes into account local and structural properties of the network using the weighting parameter. A high value of this parameter uses more structural information of the network by using a greater range of influence for a node and low value of the parameter measures the metric locally. The idea of information flow can be better understood with the help of the below image:

![Attraction Basin (source: Muchnik, et.al. (2007))](images/attraction_basin.png)

Given this idea, the hierarchy score is defined as the following:

![Attraction-Basin hierarchy Score](images/attraction_basin_score.png)

``` r
get_ab_hierarchy_score <- function(gdist,alpha,m){
  h_plus <- rep(0,dim(gdist)[1])
  h_minus <- rep(0,dim(gdist)[1])
  
  for(j in c(1:m)){  
    N_plus <- rep(0,dim(gdist)[1])
    N_minus <- rep(0,dim(gdist)[1])
    for(i in c(1:dim(gdist)[1])){
      N_plus[i] <- max(1,sum(gdist[i,] == m))
      N_minus[i] <- sum(gdist[,i] == m)
    }
    N_plus_avg <- mean(N_plus)
    N_minus_avg <- mean(N_minus)
    h_plus <- h_plus + alpha^(-j) * (N_plus / N_plus_avg)
    h_minus <- h_minus + alpha^(-j) * (N_minus / N_minus_avg)
  }
  h_score <- h_minus / h_plus
  return(h_score)
}

ab_hierarchy_score <- get_ab_hierarchy_score(geodesic_distances$gdist,alpha=2,m=3)

upper_cutoff <- 0.8
lower_cutoff <- 0.2

ab_hierarchy_graph <- get_hierarchy_graph(ab_hierarchy_score,undirected_edgelist,upper_cutoff,lower_cutoff)
ab_hierarchy_edgelist <- as_edgelist(ab_hierarchy_graph)

## plotting sub-tree
set.seed(1)
g <- graph_from_edgelist(get_sub_tree(2177,3,ab_hierarchy_edgelist,n=3))
V(g)$titles <- strtrim(as.character(vertex_titles[V(g)]),30)
iso <- V(g)[igraph::degree(g)==0]
g2 <- delete_vertices(g, iso)
V(g2)$node_label = wrap_strings(V(g2)$titles, 10)
tree_plot_func(g2)
```

    ## Warning: The plyr::rename operation has created duplicates for the
    ## following name(s): (`na.rm`)

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-29-1.png)
