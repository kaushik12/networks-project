Network Analysis
================

In this file, we look at the network data obtained using the code in *web\_scraping.Rmd*

``` r
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(sna)
```

    ## Loading required package: statnet.common

    ## 
    ## Attaching package: 'statnet.common'

    ## The following object is masked from 'package:base':
    ## 
    ##     order

    ## Loading required package: network

    ## network: Classes for Relational Data
    ## Version 1.13.0 created on 2015-08-31.
    ## copyright (c) 2005, Carter T. Butts, University of California-Irvine
    ##                     Mark S. Handcock, University of California -- Los Angeles
    ##                     David R. Hunter, Penn State University
    ##                     Martina Morris, University of Washington
    ##                     Skye Bender-deMoll, University of Washington
    ##  For citation information, type citation("network").
    ##  Type help("network-package") to get started.

    ## 
    ## Attaching package: 'network'

    ## The following objects are masked from 'package:igraph':
    ## 
    ##     %c%, %s%, add.edges, add.vertices, delete.edges,
    ##     delete.vertices, get.edge.attribute, get.edges,
    ##     get.vertex.attribute, is.bipartite, is.directed,
    ##     list.edge.attributes, list.vertex.attributes,
    ##     set.edge.attribute, set.vertex.attribute

    ## sna: Tools for Social Network Analysis
    ## Version 2.4 created on 2016-07-23.
    ## copyright (c) 2005, Carter T. Butts, University of California-Irvine
    ##  For citation information, type citation("sna").
    ##  Type help(package="sna") to get started.

    ## 
    ## Attaching package: 'sna'

    ## The following objects are masked from 'package:igraph':
    ## 
    ##     betweenness, bonpow, closeness, components, degree,
    ##     dyad.census, evcent, hierarchy, is.connected, neighborhood,
    ##     triad.census

``` r
## Importing Data Sets
edge_list <- read.csv("edge_list.csv",stringsAsFactors = FALSE)
page_details <- read.csv("page_details.csv",stringsAsFactors = FALSE)
```

First, we looking at some high-level Networks Stats

``` r
paste("No. of Nodes: ",dim(page_details)[1])
```

    ## [1] "No. of Nodes:  4189"

``` r
paste("No. of Edges: ",dim(edge_list)[1])
```

    ## [1] "No. of Edges:  30877"

``` r
network_density <- dim(edge_list)[1]/(dim(page_details)[1]*(dim(page_details)[1]-1))
paste("Density: ",network_density)
```

    ## [1] "Density:  0.00176002187016845"

``` r
print("Sample of Nodes:")
```

    ## [1] "Sample of Nodes:"

``` r
head(page_details,5)
```

    ##          url        title
    ## 1  acca.html acceleration
    ## 2  vel2.html     velocity
    ## 3  vect.html       vector
    ## 4 units.html        units
    ## 5 deriv.html   derivative

``` r
print("Sample of Edges:")
```

    ## [1] "Sample of Edges:"

``` r
head(edge_list,5)
```

    ##    from_url     to_url
    ## 1 acca.html  vel2.html
    ## 2 acca.html  vect.html
    ## 3 acca.html units.html
    ## 4 acca.html deriv.html
    ## 5 acca.html   mot.html
