detach(package:igraph)
library("statnet")
library("dplyr")
library(RColorBrewer)

setwd("~/pj/ONS/dat/")
net.relation <- read.csv(file="MyData.csv")
class_map <- read.csv(file="MyClass.csv",stringsAsFactors=FALSE)

net_import <- network(net.relation,
                      directed = FALSE,
                      matrix.type="edgelist")

detach(package:statnet)
library("igraph")
library(intergraph)
iCon <- asIgraph(net_import)
coreness <- graph.coreness(iCon)
table(coreness)
maxCoreness <- max(coreness)
Vname <- get.vertex.attribute(iCon,name='vertex.names',
                              index=V(iCon))
V(iCon)$name <- Vname
V(iCon)$color <- coreness + 1
op <- par(mar = rep(0,4))
plot(iCon,vettex.label.cex=0.8)
par(op)

colors <- rainbow(maxCoreness)
op <- par(mar = rep(0,4))
plot(iCon,vertex.label=coreness,
     vertex.color=colors[coreness] )
par(op)
