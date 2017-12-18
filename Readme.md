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
links
```

    ##  [1] NA                          "vel2.html#c1"             
    ##  [3] "vect.html#veccon"          "vect.html#vec1"           
    ##  [5] "units.html#uni4"           "deriv.html#c1"            
    ##  [7] "mot.html#mot1"             "hframe.html"              
    ##  [9] "hph.html"                  "hph.html#mechcon"         
    ## [11] "javascript:history.go(-1)"

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

``` r
url_prefix <- "http://hyperphysics.phy-astr.gsu.edu/hbase/"

## Defining a function to get all the hyperlinks in a given webpage
get_links <- function(url,prefix=url_prefix,heading){
  
  full_url <- paste0(prefix,url)
  webpage <- read_html(full_url)
  nodes <- html_nodes(webpage,'a')
  titles <- stri_encode(html_text(nodes), "", "UTF-8")
  links <- html_attr(nodes,"href")
  headings <- html_nodes(webpage,'h1')
  h_regex <- regexpr(">(.*)<",as.character(headings[1]))
  heading <- substr(as.character(headings[1]),(h_regex[1]+1),h_regex[1]+attr(h_regex,"match.length")-2)

  ## Removing any javascript or external (starting with http:// or https://) links 
  titles <- titles[!grepl("^javascript|Javascript|http://|https://|www.|mailto:",links)]
  links <- links[!grepl("^javascript|Javascript|http://|https://|www.|mailto:",links)]
  
  ## Removing multimedia links (.gif/.mp4/.jpg/.png/.mov)
  titles <- titles[!grepl(".gif|.jpg|.png|.mp4|.mov|.jpeg$",links)]
  links <- links[!grepl(".gif|.jpg|.png|.mp4|.mov|.jpeg$",links)]
  
  ## Removing any links which refer to index directory sorting
  titles <- titles[!grepl("[[:punct:]]C[[:punct:]][A-Z][[:punct:]]O[[:punct:]][A-Z]",links)]
  links <- links[!grepl("[[:punct:]]C[[:punct:]][A-Z][[:punct:]]O[[:punct:]][A-Z]",links)]
  
  ## Removing certain URLs to exclude within page references (of the form #xxxD)
  links <- sub("\\#.*","\\",links)
  
  ## Removing the Index and Main page URLs along with any null or missing cases
  excluded_urls <- c("",NA)
  titles <- titles[!links %in% excluded_urls]
  links <- links[!links %in% excluded_urls]
  
  ## If the parent url is of the form xx/yy/zz.html, we need to prefix yy/ to results of the form  xx.html
  if(grepl("^.*/",url)){
    string_to_add <- paste0(sub("\\/.*html","\\",url),"/")
    links[!grepl("^../",links)] <- paste0(string_to_add,links[!grepl("^../",links)])
  }
  
  ## Removing any links to sub-domain class as they don;t have physics content
  titles <- titles[!grepl("class/",links)]
  links <- links[!grepl("class/",links)]

  ## Trimming URLs of the form ../xxx.html and removing extra /s and spaces
  links <- sub("^../","\\",links)
  links <- gsub("//|///","/",links)
  links <- gsub("\r","",links)
  
  ## Fixing broken URLs (manual fixes during initial testing)
  links[grepl("^mechanics/vel.html$",links)] <- "vel.html"
  links[grepl("^mechanics/frict.html$",links)] <- "frict.html"
  links[grepl("^forces/particles/quark.html$",links)] <- "particles/quark.html"
  links[grepl("^magnetic/ferro.html$",links)] <- "solids/ferro.html"
  links[grepl("^nuclear/hframe.html$",links)] <- "hframe.html"
  links[grepl("^mechanics/hframe.html$",links)] <- "hframe.html"
  links[grepl("^mechanics/hph.html$",links)] <- "hph.html"
  links[grepl("^thermo/therm/entropcon.html$",links)] <- "thermo/entropcon.html"
  links[grepl("^astro/particles/hadron.html$",links)] <- "particles/hadron.html"
  links[grepl("^astro/grav.html$",links)] <- "grav.html"

  
  ## Removing the Index and Main page URLs along with any null or missing cases
  excluded_urls <- c("hframe.html","hph.html")
  titles <- titles[!links %in% excluded_urls]
  links <- links[!links %in% excluded_urls]
  links <- tolower(links)
  
  ## trimming leading and trailing white spaces and removing duplicates 
  unique_links <- trimws(links[!duplicated(links)],which="both")
  unique_titles <- trimws(titles[!duplicated(links)],which="both") 
  return(list(main_page=url,main_page_title=heading,page_links=unique_links,page_titles=unique_titles))
}

l <- get_links("acca.html",heading="Acceleration")
l
```

    ## $main_page
    ## [1] "acca.html"
    ## 
    ## $main_page_title
    ## [1] "Acceleration"
    ## 
    ## $page_links
    ## [1] "vel2.html"  "vect.html"  "units.html" "deriv.html" "mot.html"  
    ## 
    ## $page_titles
    ## [1] "velocity"                                      
    ## [2] "vector"                                        
    ## [3] "units"                                         
    ## [4] "derivative"                                    
    ## [5] "Motion equations when acceleration is constant"

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

``` r
## Creating a list to store all the URLs vsiited
visited_urls <- NA
error_urls <- NA
unvisited_urls <- c("acca.html") ## starting with the first URL
page_details <- rbind(page_details,c("acca.html","Acceleration"))
page_details <- page_details[-1,]

####################
# Function to go through a starting set of URLs and iteratively visit all linked URLs till no none remain unvisited
####################

scrape_urls <- function(urls_to_visit,visited_urls, page_details, edge_list,error_urls){
  counter <- 0
  st <- proc.time()
  while(length(urls_to_visit) > 0){
    tryCatch(
      {
        l <- get_links(urls_to_visit[1],heading=page_details$title[which(page_details$url == urls_to_visit[1])])
        page_details <- add_page_details(l,df = page_details )
        edge_list <- rbind(edge_list,get_edges(l))
      },
      error=function(cond){
        # print(unvisited_urls[1])
        error_urls <<- c(error_urls,urls_to_visit[1])
      },
      finally={
        visited_urls <- c(visited_urls,urls_to_visit[1])
        urls_to_visit <- page_details$url[!page_details$url %in% visited_urls]
        counter <- counter + 1   
      }
    )
  }
  error_urls <- error_urls[!is.na(error_urls)]
  proc.time() - st
  return(list(pd=page_details,el=edge_list,visited_urls=visited_urls,unvisited_urls=urls_to_visit,errors=error_urls))
}

## Running the scraping function

scrape <- scrape_urls(urls_to_visit=unvisited_urls,visited_urls=visited_urls, page_details=page_details, edge_list=edge_list,error_urls = error_urls)

unvisited_urls <- scrape$unvisited_urls
visited_urls <- scrape$visited_urls
page_details <- scrape$pd
edge_list <- scrape$el
error_urls <- scrape$errors
```

We find that there are 295 broken URLs. The next step is to try and fix these before scraping through them again. Through some manual inspection, we find that most broken URLs are of the form xxx/yyy/zzz.html where the correct URL is supposed to be yyy/zzz.html. We also note that some of them aren't broken and do infact work.

``` r
length(error_urls)
```

    ## [1] 295

``` r
#######################
## Fixing Broken URLs
#######################

## Removing the links which are broken because of the index directory structure
error_urls <- error_urls[!(grepl("^kinetic/imgkin",error_urls))]
error_urls <- error_urls[!(grepl("^kinetic/kinpic",error_urls))]
error_urls <- error_urls[!(grepl("kinetic/hbase/",error_urls))]
error_urls <- error_urls[!(grepl("^quantum/imgqua",error_urls))]
error_urls <- error_urls[!(grepl("^quantum/modpic",error_urls))]
error_urls <- error_urls[!(grepl("quantum/hbsae/",error_urls))]
error_urls <- error_urls[!(grepl("^thermo/heatpic",error_urls))]
error_urls <- error_urls[!(grepl("^thermo/imgheat",error_urls))]
error_urls <- error_urls[!(grepl("thermo/hbase/",error_urls))]
error_urls <- error_urls[!(grepl("^quantum/[[:punct:]]",error_urls))]
error_urls <- error_urls[!(grepl("^quantum/imgqua/[[:punct:]]",error_urls))]
error_urls <- error_urls[!(grepl("^quantum/modpic/[[:punct:]]",error_urls))]
error_urls <- error_urls[!(grepl("^kinetic/[[:punct:]]",error_urls))]

## Creating a data frame to store these URLs and their fixes
fixed_error_urls <- data.frame(matrix(ncol=2),stringsAsFactors = FALSE)
unvisited_urls <- ""

## Looping through error URLs and identifying those of the form described above
for(i in seq_along(error_urls)){
  fixed_error_urls[i,1] <- error_urls[i]
  tryCatch(
    {
      fixed_error_urls[i,2] <- substring(error_urls[i],regexpr("/",error_urls[i])[1]+1)
      temp_url <- paste0(url_prefix,fixed_error_urls[i,2])
      if(!(fixed_error_urls[i,2] %in% edge_list$from_url)){
        temp_page <- read_html(temp_url)  
      }
      ## Replace URL in Page Details
      page_details$url[which(page_details$url %in% error_urls[i])] <- fixed_error_urls[i,2]
      ## Replace URL in Edge Details
      edge_list$from_url[which(edge_list$from_url %in% error_urls[i])] <- fixed_error_urls[i,2]
      edge_list$to_url[which(edge_list$to_url %in% error_urls[i])] <- fixed_error_urls[i,2]
      ## Add to Unvisited URLs if not visited
      if(!fixed_error_urls[i,2] %in% edge_list$from_url){
        unvisited_urls <- c(unvisited_urls,fixed_error_urls[i,2])
      }
    },
    error=function(cond){
      fixed_error_urls[i,2] <<- NA
    }
  )
}

## Function to fix URLs in page details and edge list to the correct URL and adding to list of pages to visit if unvisited

fix_error_urls <- function(fixed_error_urls, page_details, edge_list,unvisited_urls){
  error_url_fixes <- fixed_error_urls[!is.na(fixed_error_urls$X2),]
  for(i in seq_along(error_url_fixes)){
    err_url <- error_url_fixes$X1[i]
    fixed_url <- error_url_fixes$X2[i]
    page_details$url[which(page_details$url == err_url)] <- fixed_url
    edge_list$from_url[which(edge_list$from_url == err_url)] <- fixed_url  
    edge_list$to_url[which(edge_list$to_url == err_url)] <- fixed_url  
    if(!fixed_url %in% edge_list$from_url){
          unvisited_urls <- c(unvisited_urls,fixed_url)
        }
  }
  fixed_error_urls <- fixed_error_urls[is.na(fixed_error_urls$X2),]
  return(list(errors=fixed_error_urls,pd=page_details,el=edge_list,unvisited=unvisited_urls))
}

fixes <- fix_error_urls(fixed_error_urls = fixed_error_urls, page_details = page_details, edge_list = edge_list, unvisited_urls = unvisited_urls)
unvisited_urls <- fixes$unvisited
page_details <- fixes$pd
edge_list <- fixes$el
fixed_error_urls <- fixes$errors

## Check if these URls are infact broken (some seem to work on manual inspection)
for(i in seq_along(fixed_error_urls$X1)){
  tryCatch(
    {
      temp_url <- paste0(url_prefix,gsub("//|///","/",fixed_error_urls[i,1]))
      temp_page <- read_html(temp_url)   
      fixed_error_urls[i,2] <- gsub("//|///","/",fixed_error_urls[i,1])
    },
    error=function(cond){
      fixed_error_urls[i,2] <<- NA
    }
  )
}

fixes <- fix_error_urls(fixed_error_urls = fixed_error_urls, page_details = page_details, edge_list = edge_list, unvisited_urls = unvisited_urls)
unvisited_urls <- fixes$unvisited
page_details <- fixes$pd
edge_list <- fixes$el
fixed_error_urls <- fixes$errors
dim(fixed_error_urls)[1]
```

    ## [1] 40

After correcting for these two cases, we are left with 40 broken URLs. Fixing these required a lot of manual checking. One of the ways of identifying these was to add the parent URL domain to check if that works and we were able to reduce the number of errors with this.

``` r
## Adding parent domain to URL to check if it works
for(i in seq_along(fixed_error_urls$X1)){
  tryCatch(
    {  
      parent_url <- edge_list$from_url[edge_list$to_url == fixed_error_urls[i,1]][2]
      parent_domain <- strsplit(parent_url,"/")[[1]][max(1,length(strsplit(parent_url,"/")[[1]])-1)]
      fixed_url <- paste0(parent_domain,"/",fixed_error_urls$X1[i])
      temp_url <- paste0(url_prefix,fixed_url)
      temp_page <- read_html(temp_url)   
      fixed_error_urls[i,2] <- fixed_url
    },
    error=function(cond){
      fixed_error_urls[i,2] <<- NA
    }
  )
}

## Other manual fixes
fixed_error_urls$X2[grep("nuclear/gphysref.html",fixed_error_urls$X1)] <-"geophys/gphysref.html"
fixed_error_urls$X2[grep("phyopt/optref.html",fixed_error_urls$X1)] <-"optics/optref.html"
fixed_error_urls$X2[grep("astref.html",fixed_error_urls$X1)] <-"astro/astref.html"
fixed_error_urls$X2[grep("relcon.html",fixed_error_urls$X1)] <-"relativ/relcon.html"
fixed_error_urls$X2[grep("music/..sound/timbre.html",fixed_error_urls$X1)] <-"sound/timbre.html"

fixes <- fix_error_urls(fixed_error_urls = fixed_error_urls, page_details = page_details, edge_list = edge_list, unvisited_urls = unvisited_urls)
unvisited_urls <- fixes$unvisited
page_details <- fixes$pd
edge_list <- fixes$el
fixed_error_urls <- fixes$errors
dim(fixed_error_urls)[1]
```

    ## [1] 20

``` r
## Ignoring 20 broken URLs for now
```

We ignore the 20 remaining cases, as it was difficult to manually identify the correct URLs for these. Finally, we visit the unvisited fixed URLs and add the information to the dataset.

``` r
## Re-running the scraping function with the fixed URLs unvisited previously
new_error_urls <- ""
scrape <- scrape_urls(urls_to_visit=unvisited_urls,visited_urls=visited_urls, page_details=page_details, edge_list=edge_list,error_urls = new_error_urls)

unvisited_urls <- scrape$unvisited_urls
visited_urls <- scrape$visited_urls
page_details <- scrape$pd
edge_list <- scrape$el
new_error_urls <- scrape$errors
```

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

    ## [1] "No. of Nodes:  2931"

    ## [1] "No. of Edges:  21058"

    ## [1] "Density:  0.00245"

###### A look at the network sample

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

From the plot of an induced sub-graph of 750 nodes, we can already see some clusters closely interconnected within in the network.

###### Degree Distribution

The first network statistics we look into furhter is the degree distribution. We want to verify if the distribution follows a power law as with several similar networks extracted from the web.

    ## [1] "Mean In-Degree:  7.1846"

    ## [1] "Mean Out-Degree:  7.1846"

It is a bit strange and interesting that the Avg. In and Out Degrees match perfectly.We then plot the degree distribution in the log-log scale to see if it is linear denoting a scale-free distribution.

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-19-1.png)

We note that the degree distributions is almost perfectly linear in the log-log scale signifying preferential attachment in terms of the network structure. The out-degree distribution though starts off with a lower frequency of nodes with fewer degress. This perhaps indicates the nature of the textbook where most of the pages are linked with atleast a few other pages and very few pages have no links outward.

    ## log(as.numeric(row.names(table(in_degree)))) 
    ##                                     -1.36514

    ## log(as.numeric(row.names(table(out_degree)[-1]))) 
    ##                                         -1.551737

The estimated co-efficient or the power in the power-law form for the degree distributions are -1.365 and -1.552 for the In and Out degrees respectively. That is the in and out degrees roughly follow the following distributions.

![Degree Distribution](images/degrees.png)

###### Geodesic Distance Distribution

Next, we look at the distrbution of the Geodesic distance between nodes. This measure gives us a sense of how close the nodes are from each other and along side a scale-free degree distribution, a short average geodesic distance (or network diameter) would indicate a small-world nature of the network as defined by Watts and Strogatz (1998).

    ## [1] "Average Geodesic Distance:  5.862"

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-21-1.png)

We do observe that the Network Diameter small at ~5.86 and this is re-inforced by the above plot of the geodesic distance distribution.

###### Centralization

The key network and nodal measure for our analysis is the Centrality and Centralization measures.The overall network centralization is 0.236. This gives us a sense of the centralization of the network relative to a star configuration which is maximally centrlized.

    ## [1] "Network Centralization:  0.236"

------------------------------------------------------------------------

#### Part 3: Extracting hierarchies

In this section, we create the functions to extract hierarchies from the network and use these on the HyperPhysics article network. Four different hierarchy extraction methods are explored, namely

1.  Betweenness Centrality based
2.  Page-Rank based
3.  Attraction Basin
4.  Eigenvector based

The specific formulae and methodology are presented in following sub-sections where we explore each of these methodologies in detail. The first three methods listed above are as defined in the Muchnik, et.al. paper. The last methodology is an independent exploration of a different way to extract hierarchy which takes into account the clusters of topics in the network and extract the hierarchy iteratively starting from the highest-level allthe way down to the leaf nodes.

###### Overview

To understand the core idea behind extracting hierarchies from a network, we look at a regular tree to start with. From the below sample tree marked by it's corresponding betweenness centralities, we can make the following observations:

-   Greater centrality corresponds to a higher level in the hierarchy
-   Nodes at the same level of hierarchy have similar centrality
-   Large difference in centrality for nodes unconnected with by an edge in the hierarchy

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-24-1.png)

These three fundamental ideas form the basis of hierarchy extraction in each of the following methods. The overall process of hierarchy extraction can be summarized as the following set of steps

1.  Define a suitable hierarchy score for each node: A hierarchy score for a node is a measure such as it's betweenness centrality which can be used to compare two nodes to determine the hierarchical relation between them. For a regular node such as the above example, we can simply use the betweenness centrality as the graph is regular. But in the case of real-world networks such as the one we are dealing with, every node need not have the same degree and hence we look at nodal measures which are scaled by their respective degrees.

2.  Compare scores for 2 neighboring nodes (in the underlying undirected network): We then compare the scores of two nodes in the underlying network and determine if one is higher in the hierarchy if the ratio of the scores is between a lower and an upper threshold and it has a higher score. This does not depend on the direction of the edge between the two nodes in the underlying network.

3.  Define the right cutoffs: The choice of cutoffs affects the hierarchical network output. A high upper cutoff would create hierarchical relations between two similar nodes at the same level in the hierarchy such as two disjoint topics in physics such as Kinetics and Optics, say. A low or no lower cutoff would include a hierarchical relation between two nodes connected in the underlying network but not necessarily directly related in a hierarchical sense. It may leed to creation of triangles in the hierarchy with a node being connected to several nodes at higher levels in the hierarchy.

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
## Recursive Depth-First search for the sub-tree given the root

get_children <- function(st,el,n=5){
  all_children <- matrix(el[el[,1]==st,],ncol=2,byrow=FALSE)
  sampled_children <- all_children[all_children[,2] %in% sample(all_children[,2],min(n,length(all_children[,2])),replace=FALSE),]
  return(matrix(sampled_children,ncol=2,byrow = FALSE))
}

get_sub_tree <- function(st,el,level=1,cur_level=1,sub_el=matrix(NA,ncol=2),n=5){
  if(cur_level > level){
    tmp_el <- matrix(NA,ncol=2)
    return(tmp_el)
  }else{
    tmp_el <- get_children(st,el,n)
    for(i in seq_along(tmp_el[,2])){
      if(!tmp_el[i,2] %in% sub_el[,1]){
        tmp_el <- rbind(tmp_el,get_sub_tree(tmp_el[i,2],el,level,(cur_level+1),tmp_el,n=n))  
      }
    }
  }
  tmp_el <- tmp_el[!duplicated(tmp_el),]
  tmp_el <- tmp_el[!is.na(tmp_el[,1]),]
  return(tmp_el)
}

wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
                        paste(strwrap(x, width=width), collapse="\n")
                        }))
}


## Function to plot a tree using ggraph
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

![Betweenness Centrality Score](images/betweenness_centrality.png) \#\#\#\#\#\# Results

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-27-1.png)

###### Page-Rank based hierarchy extraction

The idea behind page-rank based hierarchy scoring is similar to betweenness based approach. Page-Rank algorithm gives us a centrality measure for a node that is weighted by the centrality measures of it's neighbours. As this measure is already weighted, we don't need to scale it by the degree of the node. Page-Rank centrality is defined as below:

![Page-Rank Centrality](images/page_rank_centrality.png)

$$PR(u) = \\sum\_{v\\in B\_u}\\frac{PR(v)}{L(v)} $$

*P**R*(*u*): Page-Rank for node *u*
*B*<sub>*u*</sub>: The set containing all nodes linking to node *u*
*L*(*v*): No. of edges from node *v*

###### Results

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-28-1.png)

###### Attraction Basin based hierarchy extraction

This is a method developed by the authors of the paper. The purpose behind it was to create a measure that can be estimated locally and does not nequire for the entire network to be analysed. For example, to calculate betweenness centrality for a node, all the shortest paths between all the nodes have to be calculated. Similarly, page-rank centrality being iterative, has to be estimated for all the nodes in the network together. For large networks, such computations can be very expensive and hence there is a need for a local measure of centrality.

The authors define the score as comparision between the weighted fraction of the network from which the node can be reached to the weighted fraction of the network that can be reached from each node. In a sense, the quantity summarizes the "flow"" of information/references through a node. It takes into account local and structural properties of the network using the weighting parameter. A high value of this parameter uses more structural information of the network by using a greater range of influence for a node and low value of the parameter measures the metric locally. The idea of information flow can be better understood with the help of the below image:

![Attraction Basin (source: Muchnik, et.al. (2007))](images/attraction_basin.png)

Given this idea, the hierarchy score is defined as the following:

![Attraction-Basin hierarchy Score](images/attraction_basin_score.png) \#\#\#\#\#\# Results

![](Readme_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-29-1.png)
