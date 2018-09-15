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
V(iCon)$class <- as.vector(df_c_merged[,2])
  
op <- par(mar = rep(0,4))
plot(iCon,vettex.label.cex=0.8)
par(op)

colors <- rainbow(maxCoreness)
op <- par(mar = rep(0,4))
plot(iCon,vertex.label=coreness,
     vertex.color=colors[coreness] )
par(op)

ceb <- cluster_edge_betweenness(iCon)
plot(ceb,iCon,vertex.label=NA)

ceb <- cluster_edge_betweenness(iCon)
modularity(ceb)
cle <- cluster_leading_eigen(iCon)
modularity(cle)
#### NG ####
#cfg <- cluster_fast_greedy(iCon)
#modularity(cfg)
###########
#### Best Modularity
cl <- cluster_louvain(iCon)
modularity(cl)
#########
clp <- cluster_label_prop(iCon)
modularity(clp)
ci <- cluster_infomap(iCon)
modularity(ci)
#### NG ####
#cs <- cluster_spinglass(iCon)
#modularity(cs)
###########
#### NG ####
#co <- cluster_optimal(iCon)
#modularity(co)
###########

compare(as.numeric(as.factor(V(iCon)$class)),cl,method="adjusted.rand")
compare(as.numeric(as.factor(V(iCon)$class)),ceb,method="adjusted.rand")
compare(as.numeric(as.factor(V(iCon)$class)),cle,method="adjusted.rand")
compare(as.numeric(as.factor(V(iCon)$class)),clp,method="adjusted.rand")
compare(as.numeric(as.factor(V(iCon)$class)),ci,method="adjusted.rand")

####
table(V(iCon)$class,membership(cl)) #Louvain
table(V(iCon)$class,membership(cle)) #Leading eigenvector

maxClass <- length(levels(as.factor(V(iCon)$class))) + 1 
colors <- rainbow(maxClass)
op <- par(mar = rep(0,4))
plot(cl,iCon,vertex.label=NA,
     vertex.size = rescale(betweenness(iCon),1,5) * 5,
     vertex.color=colors[as.factor(V(iCon)$class)])
par(op)

op <- par(mar = rep(0,4))
plot(cl,iCon,vertex.label=NA,
     vertex.size = rescale(degree(iCon),1,5) * 3,
     vertex.color=colors[as.factor(V(iCon)$class)])
par(op)


betweenness(iCon)
rescale(betweenness(iCon),1,10)

edge.betweenness.community(iCon)
plot(as.dendrogram(edge.betweenness.community(iCon)))
membership(cl)

cluster_df <- data_frame(
  name = names(membership(cl)),
  class = V(iCon)$class,
  cl = as.vector(membership(cl)),
  cle = as.vector(membership(cle)),
  ceb = as.vector(membership(ceb)),
  clp = as.vector(membership(clp)),
  ci = as.vector(membership(ci))
)

cluster_group <- cluster_df %>% 
  group_by(cl,cle) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

ci = as.vector(membership(ci))
class = V(iCon)$class,

length(names(membership(cl)))
length(as.vector(membership(cl)))
table(V(iCon)$name,membership(cl))
table(V(iCon)$name,membership(cle))
table(V(iCon)$name,membership(ceb))
table(V(iCon)$name,membership(clp))
table(V(iCon)$name,membership(ci))

(p <- ggplot(cluster_group, aes(cl, cle)) + geom_tile(aes(fill = n),
      colour = "white") + scale_fill_gradient(low = "white",
      high = "steelblue"))
