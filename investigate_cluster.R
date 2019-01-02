library("rlist")
library("dplyr")
setwd("~/pj/ONS/dat/")
library(statnet)
library(RColorBrewer)

#### Threshold
thr <- 25

net.relation <- read.csv(file="MyData.csv")
net_import <- network(net.relation,
                      directed = FALSE,
                      matrix.type="edgelist")
df_c_attribute <- data.frame(
  src_name = net_import %v% 'vertex.names'
)
class_map <- read.csv(file="MyClass.csv",stringsAsFactors=FALSE)
df_c_merged <- df_c_attribute %>% left_join(class_map,by="src_name")
net_import %v% 'class' <- as.vector(df_c_merged[,2])

detach(package:statnet)
library("igraph")
library(intergraph)
iCon <- asIgraph(net_import)
coreness <- graph.coreness(iCon)
#table(coreness)
maxCoreness <- max(coreness)
Vname <- get.vertex.attribute(iCon,name='vertex.names',
                              index=V(iCon))
V(iCon)$name <- Vname
V(iCon)$color <- coreness + 1
V(iCon)$class <- as.vector(df_c_merged[,2])

ceb <- cluster_edge_betweenness(iCon)
cle <- cluster_leading_eigen(iCon)
cl <- cluster_louvain(iCon)
clp <- cluster_label_prop(iCon)
ci <- cluster_infomap(iCon)

cluster_df <- data_frame(
  name = names(membership(cl)),
  class = V(iCon)$class,
  bet = betweenness(iCon),
  deg = degree(iCon),
  cl = as.vector(membership(cl)),
  cle = as.vector(membership(cle)),
  ceb = as.vector(membership(ceb)),
  clp = as.vector(membership(clp)),
  ci = as.vector(membership(ci))
)

mst <- list(
  cluster = data.frame(
    param = c("ceb","cle","cl","clp","ci"),
    name = c("Edge Betweenness","Leading Eigen","Louvain","Label Prop","Infomap")
  )
)

## すべてのclusterの頻度を出力する
library(tidyverse)
slide <- as.vector(mst$cluster$param) %>% map(function(x){
  
  cluster_count <- cluster_df %>%
    group_by_(paste(x)) %>%
    summarise(cons = n())

  cluster_n <- mst$cluster %>% filter(param==x) %>% select(name)
  cluster_t <- paste0("(",cluster_n$name,")")
  title_s <- paste("## Distribution of # of concepts",cluster_t,sep=" ")
  
  library(ggplot2)
  g <- ggplot(cluster_count, 
              aes(cons)) + 
    geom_histogram()
  
  list(title = title_s, plot=g)
})

slide_thr <- as.vector(mst$cluster$param) %>% map(function(x){
  
  cluster_count <- cluster_df %>%
    group_by_(paste(x)) %>%
    summarise(cons = n()) %>%
    filter(cons >thr)
  
  cluster_n <- mst$cluster %>% filter(param==x) %>% select(name)
  cluster_t <- paste0("(",cluster_n$name,")")
  title_s <- paste("## Distribution of # of concepts",cluster_t,sep=" ")
  
  library(ggplot2)
  g <- ggplot(cluster_count, 
              aes(cons)) + 
    geom_histogram()
  
  if(length(cluster_count$cons) > 0){
    list(title = title_s, plot=g)
  }
  
})

cl_list <- list(cluster_edge_betweenness,
                cluster_leading_eigen,
                cluster_louvain,
                cluster_label_prop,
                cluster_infomap) %>%
            map(~ .(iCon))

list_cluster <- seq_along(cl_list) %>% map(function(x){
  data.frame(
    name = names(membership(cl_list[[x]])),
    cl_no = as.vector(membership(cl_list[[x]])),
    cluster = rep(as.vector(mst$cluster$name[x]),length(membership(cl_list[[x]])))
  )
})
df_cluster <- list.rbind(list_cluster)

df_count_thr <- df_cluster %>%
#  filter(cluster == "Edge Betweenness") %>%
  group_by(cluster,cl_no) %>%
  summarise(cons = n()) %>%
  filter(cons > thr)

x <- as.vector(df_count_thr$cluster)
y <- as.vector(df_count_thr$cl_no)
list_con_thr <- map2(x,y, function(x,y){
  df_cluster %>%
    filter(cluster == x & cl_no == y) %>%
    select(cluster, cl_no, name)
})
df_con_thr <- list.rbind(list_con_thr)
df_con_thr <- df_con_thr %>% left_join(cluster_df[,c("name","class","bet","deg")],by="name")
library(knitr)
k_list <- map2(x,y, function(x,y){
  table <- df_con_thr %>%
    filter(cluster==x & cl_no == y) %>%
    select(
      name,class,bet,deg)
  table_count <- dim(table)[1]
  table_count <- paste0("(",table_count,")")
  table_caption <- paste(x,y,sep="&")
  table_caption <- paste0(table_caption,table_count)
  kable(table,caption=table_caption)
})