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
  summarise(all_n = n()) %>% 
  arrange(desc(all_n))

cluster_cl <- cluster_df %>% 
  group_by(cl) %>% 
  summarise(cl_n = n()) %>% 
  arrange(desc(cl_n))

cluster_cle <- cluster_df %>% 
  group_by(cle) %>% 
  summarise(cle_n = n()) %>% 
  arrange(desc(cle_n))

cluster_cla <- cluster_df %>% 
  group_by(class) %>% 
  summarise(cla_n = n()) %>% 
  arrange(desc(cla_n))

cluster_cl_m <- cluster_group %>%
  left_join(cluster_cl,by="cl")

cluster_m <- cluster_cl_m %>%
  left_join(cluster_cle,by="cle")

cluster_m <- cluster_m %>% mutate(cl_ratio = all_n/cl_n)
cluster_m <- cluster_m %>% mutate(cle_ratio = all_n/cle_n)
cluster_m <- cluster_m %>% mutate(all_ratio = cl_ratio + cle_ratio)
cluster_m <- cluster_m %>% mutate(label = paste(cl,"&",cle))
cluster_m %>% arrange(desc(all_ratio))

### plot
all_n_threshold = 10
all_ratio_threshold = 1
ggplot(cluster_m, aes(all_ratio, all_n)) +
  geom_point() +
  geom_text(
    data=subset(cluster_m, 
                all_ratio >  all_ratio_threshold & 
                all_n > all_n_threshold),
    aes(all_ratio, all_n,label=label)
    )
###

cluster_clacl <- cluster_df %>% 
  group_by(class,cl) %>% 
  summarise(clacl_n = n()) %>% 
  arrange(desc(clacl_n))

cluster_clacl_m <- cluster_clacl %>%
  left_join(cluster_cla,by="class")

cluster_clacl_m <- cluster_clacl_m %>%
  left_join(cluster_cl,by="cl")

cluster_clacl_m <- cluster_clacl_m %>% mutate(cla_ratio = clacl_n/cla_n)
cluster_clacl_m <- cluster_clacl_m %>% mutate(cl_ratio = clacl_n/cl_n)
cluster_clacl_m <- cluster_clacl_m %>% mutate(clacl_ratio = cla_ratio + cl_ratio)
cluster_clacl_m <- cluster_clacl_m %>% mutate(label = paste(substr(class,1,2),"&",cl))
cluster_clacl_m %>% arrange(desc(clacl_ratio))

### plot
clacl_n_threshold = 10
clacl_ratio_threshold = 1
ggplot(cluster_clacl_m, aes(clacl_ratio, clacl_n)) +
  geom_point() +
  geom_text(
    data=subset(cluster_clacl_m, 
                clacl_ratio >  clacl_ratio_threshold & 
                clacl_n > clacl_n_threshold),
    aes(clacl_ratio, clacl_n,label=label)
  )
###

cluster_3 <- cluster_df %>% 
  group_by(class,cl,cle) %>% 
  summarise(all_n = n()) %>% 
  arrange(desc(all_n))

cluster_3_m <- cluster_3 %>%
  left_join(cluster_cla,by="class")

cluster_3_m <- cluster_3_m %>%
  left_join(cluster_cle,by="cle")

cluster_3_m <- cluster_3_m %>%
  left_join(cluster_cl,by="cl")

cluster_3_m <- cluster_3_m %>% mutate(cla_ratio = all_n/cla_n)
cluster_3_m <- cluster_3_m %>% mutate(cle_ratio = all_n/cle_n)
cluster_3_m <- cluster_3_m %>% mutate(cl_ratio = all_n/cl_n)
cluster_3_m <- cluster_3_m %>% mutate(all_ratio = cla_ratio + cle_ratio + cl_ratio)
cluster_3_m <- cluster_3_m %>% mutate(label = paste(substr(class,1,2),"&",cl,"&",cle))
cluster_3_m %>% arrange(desc(all_ratio))

### plot
all_n_threshold = 10
all_ratio_threshold = 1
ggplot(cluster_3_m, aes(all_ratio, all_n)) +
  geom_point() +
  geom_text(
    data=subset(cluster_3_m, 
                all_ratio >  all_ratio_threshold & 
                all_n > all_n_threshold),
    aes(all_ratio, all_n,label=label)
  )
###

cluster_cl_m <- cluster_group %>%
  left_join(cluster_cl,by="cl")

cluster_m <- cluster_cl_m %>%
  left_join(cluster_cle,by="cle")

cluster_m <- cluster_m %>% mutate(cl_ratio = all_n/cl_n)
cluster_m <- cluster_m %>% mutate(cle_ratio = all_n/cle_n)
cluster_m <- cluster_m %>% mutate(all_ratio = cl_ratio + cle_ratio)
cluster_m <- cluster_m %>% mutate(label = paste(cl,"&",cle))
cluster_m %>% arrange(desc(all_ratio))

### plot
all_n_threshold = 10
all_ratio_threshold = 1
ggplot(cluster_m, aes(all_ratio, all_n)) +
  geom_point() +
  geom_text(
    data=subset(cluster_m, 
                all_ratio >  all_ratio_threshold & 
                  all_n > all_n_threshold),
    aes(all_ratio, all_n,label=label)
  )
###



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
