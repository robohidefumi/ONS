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
as.undirected(iCon)
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

as.numeric(as.factor(V(iCon)$class))
colors <- rainbow(length(levels(as.factor(V(iCon)$class))))
plot(iCon,vertex.label=NA,
     vertex.color=colors[as.numeric(as.factor(V(iCon)$class))] )

ceb <- cluster_edge_betweenness(iCon)
cle <- cluster_leading_eigen(iCon)
cl <- cluster_louvain(iCon)
clp <- cluster_label_prop(iCon)
ci <- cluster_infomap(iCon)

iCon_m <- iCon %>% set_vertex_attr("ceb", value = membership(ceb))
iCon_m <- iCon_m %>% set_vertex_attr("cl", value = membership(cl))

cl_levels = c(
  length(levels(as.factor(membership(cl)))),
  length(levels(as.factor(membership(cle)))),
  length(levels(as.factor(membership(ceb)))),
  length(levels(as.factor(membership(clp)))),
  length(levels(as.factor(membership(ci))))
)

cl_name = c("Louvain","Leading Eigen","Edge Betweenness","Label Prop","Infomap")

clnum <- data.frame(
  cl_name = cl_name,
  cl_levels = cl_levels
)
clnum_sort <- clnum %>% arrange(desc(cl_levels))
clnum_sort <- clnum_sort %>% mutate(algorithm = factor(cl_name,levels = cl_name))
levels(clnum_sort$algorithm)
ggplot(clnum_sort,aes(algorithm,cl_levels)) +
  geom_bar(stat="identity") +
  labs(title = "# of cluster by algorithm") 


mod_ceb <- modularity(ceb)
mod_cle <- modularity(cle)
mod_cl <- modularity(cl)
mod_clp <- modularity(clp)
mod_ci <- modularity(ci)

cl_levels = c(
  length(levels(as.factor(membership(cl)))),
  length(levels(as.factor(membership(cle)))),
  length(levels(as.factor(membership(ceb)))),
  length(levels(as.factor(membership(clp)))),
  length(levels(as.factor(membership(ci))))
)

cl_mod = c(mod_cl,mod_cle,mod_ceb,mod_clp,mod_ci)

cl_name = c("Louvain","Leading Eigen","Edge Betweenness","Label Prop","Infomap")

clbasic_df <- data_frame(
  name = cl_name,
  levels = cl_levels,
  modularity = cl_mod
)

ggplot(clbasic_df, aes(levels, modularity,colour = name)) +
  geom_point() +
  geom_text(
    aes(label=name,colour = name),
    vjust = "inward", hjust="inward",
    size = 4
  ) +
  labs(title = "# of clusters & modularity by algorithm")
  

mod_out <- data_frame(
  name = c("Edge Betweenness","Leading Eigen","Louvain","Label Prop","Infomap"),
  modularity = c(mod_ceb,mod_cle,mod_cl,mod_clp,mod_ci)
)
mod_sort <- mod_out %>% arrange(desc(modularity))
mod_sort <- mod_sort %>% mutate(algorithm = factor(name,levels = name))
levels(mod_sort$algorithm)
ggplot(mod_sort,aes(algorithm,modularity)) +
  geom_bar(stat="identity") +
  labs(title = "modularity by algorithm") +
  

plot(cl,iCon,vertex.label=NA)
plot(ceb,iCon,vertex.label=NA)


cle <- cluster_leading_eigen(iCon)
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

cla <- as.numeric(as.factor(V(iCon)$class))
ceb_cl <- compare(ceb,cl,method="adjusted.rand")
cla_cl <- compare(cla,cl,method="adjusted.rand")
cla_ceb <- compare(cla,ceb,method="adjusted.rand")

com_out <- data_frame(
  name = c("Edge Betweenness x Louvain","Class x Louvin","Class x Edge Betweenness"),
  compare = c(ceb_cl,cla_cl,cla_ceb)
)
com_sort <- com_out %>% arrange(desc(compare))
com_sort <- com_sort %>% mutate(algorithm = factor(name,levels = name))
levels(com_sort$algorithm)
ggplot(com_sort,aes(algorithm,compare)) +
  geom_bar(stat="identity") +
  labs(title = "compare by algorithm") 


compare(as.numeric(as.factor(V(iCon)$class)),cl,method="adjusted.rand")

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
  group_by(cl,ceb) %>% 
  summarise(all_n = n()) %>% 
  arrange(desc(all_n))

####
cluster_df %>% filter(cl==35 & ceb==6) %>% select(name,class,cle,clp,ci)
####

cluster_cl <- cluster_df %>% 
  group_by(cl) %>% 
  summarise(cl_n = n()) %>% 
  arrange(desc(cl_n))

cluster_ceb <- cluster_df %>% 
  group_by(ceb) %>% 
  summarise(ceb_n = n()) %>% 
  arrange(desc(ceb_n))

cluster_cla <- cluster_df %>% 
  group_by(class) %>% 
  summarise(cla_n = n()) %>% 
  arrange(desc(cla_n))

cluster_cl_m <- cluster_group %>%
  left_join(cluster_cl,by="cl")

cluster_m <- cluster_cl_m %>%
  left_join(cluster_ceb,by="ceb")

cluster_m <- cluster_m %>% mutate(cl_ratio = all_n/cl_n)
cluster_m <- cluster_m %>% mutate(ceb_ratio = all_n/ceb_n)
cluster_m <- cluster_m %>% mutate(all_ratio = cl_ratio + ceb_ratio)
cluster_m <- cluster_m %>% mutate(label = paste(cl,"&",ceb))
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

cluster_claceb <- cluster_df %>% 
  group_by(class,ceb) %>% 
  summarise(claceb_n = n()) %>% 
  arrange(desc(claceb_n))

cluster_claceb_m <- cluster_claceb %>%
  left_join(cluster_ceb,by="class")

cluster_claceb_m <- cluster_claceb_m %>%
  left_join(cluster_ceb,by="ceb")

cluster_claceb_m <- cluster_claceb_m %>% mutate(cla_ratio = claceb_n/ceb_n)
cluster_claceb_m <- cluster_claceb_m %>% mutate(ceb_ratio = claceb_n/ceb_n)
cluster_claceb_m <- cluster_claceb_m %>% mutate(claceb_ratio = cla_ratio + ceb_ratio)
cluster_claceb_m <- cluster_claceb_m %>% mutate(label = paste(substr(class,1,2),"&",ceb))

### plot
claceb_n_threshold = 10
claceb_ratio_threshold = 1
ggplot(cluster_claceb_m, aes(claceb_ratio, claceb_n)) +
  geom_point() +
  geom_text(
    data=subset(cluster_claceb_m, 
                claceb_ratio >  claceb_ratio_threshold & 
                  claceb_n > claceb_n_threshold),
    aes(claceb_ratio, claceb_n,label=label)
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

##### Important Concepts
center_df <- read.csv(file="MyCenter.csv",stringsAsFactors=FALSE)
cluster_m <- cluster_df %>% left_join(center_df, by = c("name" = "con"))
important_df <- cluster_m %>% 
  filter(cl==35 & ceb==6) %>%
  select(name,class,deg,bet)

ggplot(important_df, aes(deg,bet,fill=class,color=class)) +
  geom_point() +
  #facet_wrap(~cla) +
  labs(title = "35 & 6 Concepts measured by Deg & Bet") +
  geom_text(
    data=subset(important_df, deg > 10 | bet > 2500),
    aes(deg,bet,label=name),
    vjust = "inward", hjust="inward"
    )

library(knitr)
kable(important_df %>% arrange(desc(bet)), 
      caption = "35 & 6 Concepts Ordered by Betweenness")

con_df <- cluster_m %>% 
  filter(cl==35 & ceb==6) %>%
  select(name)
as.vector(con_df)

subg <- subgraph(as.undirected(iCon_m),which((V(iCon_m)$cl==35) & (V(iCon_m)$ceb==6)))
degree(subg)
plot(subg,vertex.label=NA,layout=layout.fruchterman.reingold)
plot(subg,vertex.label=NA,vertex.size=degree(subg))
levels(as.factor(V(iCon_m)$class)) + 1

rescale <- function(nchar, low, high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d)) / (max_d-min_d)+low
  rscl
}
my_pal <- brewer.pal(10,"Paired")
classcat <- as.factor(V(subg)$class)
plot(subg,
     label=layout.kamada.kawai, 
     vertex.label=NA,
     vertex.color=my_pal[classcat],
     legend = TRUE,
     vertex.size=rescale(betweenness(subg),1,20))
legend("bottomleft",legend=levels(as.factor(V(subg)$class)),
       col=my_pal,pch=19, pt.cex=0.5, bty="n",cex=0.5,
       title="Concept Class")
betweenness(subg)/100

delete.edges(iCon_m, which(V(iCon_m)$ceb != 35 & V(iCon_m)$cl != 6 &))

cl_subg_fil <- function(i,cl_df){
  df
}

library(knitr)
iCon_m <- iCon %>% set_vertex_attr("ceb", value = membership(ceb))
iCon_m <- iCon_m %>% set_vertex_attr("cl", value = membership(cl))


cl_subg_plot <- function(i,cl_df, all_g){
    df <- cl_df %>% 
      filter(cl == i) %>%
      select(name,class,deg,bet)
    p <- ggplot(df, aes(deg,bet,fill=class,color=class)) +
      geom_point() +
      labs(title = paste("Louvin " ,i, " Concepts measured by Deg & Bet")) +
      geom_text(
        data=subset(df, deg > 10 | bet > 2500),
        aes(deg,bet,label=name),
        vjust = "inward", hjust="inward"
      )

      k <- kable(df %>% arrange(desc(bet)), 
          caption = paste("Louvin ", i,  " Concepts Ordered by Betweenness"))

      subg <- subgraph(as.undirected(all_g),which((V(all_g)$cl==i)))
      classcat <- as.factor(V(subg)$class)
      g <- plot(subg,
           label=layout.kamada.kawai, 
           vertex.label=NA,
           vertex.color=my_pal[classcat],
           legend = TRUE,
           vertex.size=rescale(betweenness(subg),1,20))
      l <- legend("bottomleft",legend=levels(as.factor(V(subg)$class)),
             col=my_pal,pch=19, pt.cex=0.5, bty="n",cex=0.5,
             title="Concept Class")
    
        list(scatter = p, table = k, graph = g, legend = l)
}
cl_subg_plot(1,cluster_m,iCon_m)





