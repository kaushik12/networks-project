Introduction
================

This project is an attempt to use methods described in the paper by Muchnik et al. for extracting hierarchies from a network. The end goal is to be able to use these techniques to create a knowledge tree from the Wikipedia English Article Network. For the purpose of this project though, we use a smaller and more specific network of articles from HyperPhysics, a HTML Textbook (<http://hyperphysics.phy-astr.gsu.edu/hbase/hframe.html>). The HyperPhysics page provides hierarchies based on content and some semantics which could be used as a reference to validate the results of the analysis.

------------------------------------------------------------------------

### Part 1: Scraping for Data

The first part of the project involves scraping through the HyperPhysics domain to extract all the articles and their hyperlinks to form the base network to be analysed. The steps involved in this are deatiled in the *web\_scraping.Rmd*. The network data from the scraping is stored as an Edge List (*edge\_list.csv*) and Vertex Attibutes (*page\_details.csv*) in the Data sub-folder. The edge-list contains a *from* and a *to* URL and page-details contains the *URL* and the *title*.

------------------------------------------------------------------------

### Part 2: Network Stats

The second part is where we use this data to create a network object and perform some basic analysis. In this part we look at presenting a high-level overview of the network through some of it's statistics. The code and results are detailed in *network\_stats.Rmd*.

------------------------------------------------------------------------

### Part 3: Extracting hierarchies

In this section, we create the functions to extract hierarchies from a network and use these on the HyperPhysics article network.
