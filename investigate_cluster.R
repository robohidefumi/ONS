library("rlist")
library("dplyr")
setwd("~/pj/ONS/dat/")
library(statnet)
library(RColorBrewer)

#### Threshold
thr <- 25

#### Import Network Data
##### NetworkとClassデータの統合
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


#### switch from statnet to igraph
detach(package:statnet)
library("igraph")
library(intergraph)
#### creating igraph format
iCon <- asIgraph(net_import)
#### obtaining vertex.names of iCon
Vname <- get.vertex.attribute(iCon,name='vertex.names',
                              index=V(iCon))
#### setting names
V(iCon)$name <- Vname
#### setting class data 
V(iCon)$class <- as.vector(df_c_merged[,2])

#### obtaining cluster data
ceb <- cluster_edge_betweenness(iCon)
cle <- cluster_leading_eigen(iCon)
cl <- cluster_louvain(iCon)
clp <- cluster_label_prop(iCon)
ci <- cluster_infomap(iCon)

#### creating data frames
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

#### creating map master
mst <- list(
  cluster = data.frame(
    param = c("ceb","cle","cl","clp","ci"),
    name = c("Edge Betweenness","Leading Eigen","Louvain","Label Prop","Infomap")
  )
)

## すべてのcluster algorithmのcluster数を出力する
library(tidyverse)
slide <- as.vector(mst$cluster$param) %>% map(function(x){
  
  ### cluster algorithmごとのcluster毎のコンセプト数を取得
  cluster_count <- cluster_df %>%
    group_by_(paste(x)) %>%
    summarise(cons = n())

  ### スライドタイトルを取得
  ### 該当アルゴリズムのクラスタ名を取得
  cluster_n <- mst$cluster %>% filter(param==x) %>% select(name)
  cluster_t <- paste0("(",cluster_n$name,")")
  title_s <- paste("## Distribution of # of concepts",cluster_t,sep=" ")

  ### histogramを作成  
  library(ggplot2)
  g <- ggplot(cluster_count, 
              aes(cons)) + 
    geom_histogram()
  
  list(title = title_s, plot=g)
})

#### Theresholdを超えたものだけスライドにする
slide_thr <- as.vector(mst$cluster$param) %>% map(function(x){

  ### cluster algorithmごとのcluster毎のコンセプト数を取得  
  cluster_count <- cluster_df %>%
    group_by_(paste(x)) %>%
    summarise(cons = n()) %>%
    filter(cons >thr)

  ### スライドタイトルを取得
  ### 該当アルゴリズムのクラスタ名を取得
  cluster_n <- mst$cluster %>% filter(param==x) %>% select(name)
  cluster_t <- paste0("(",cluster_n$name,")")
  title_s <- paste("## Distribution of # of concepts",cluster_t,sep=" ")

  ### histogramを作成    
  library(ggplot2)
  g <- ggplot(cluster_count, 
              aes(cons)) + 
    geom_histogram()

  ### histogramを作成  
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

#### argorithm毎にconcept毎のcluster番号
list_cluster <- seq_along(cl_list) %>% map(function(x){
  data.frame(
    # concept名
    name = names(membership(cl_list[[x]])),
    # cluster番号
    cl_no = as.vector(membership(cl_list[[x]])),
    # cluster名を出力
    cluster = rep(as.vector(mst$cluster$name[x]),length(membership(cl_list[[x]])))
  )
})
### 一つのデータフレムに結合
df_cluster <- list.rbind(list_cluster)

### 閾値以上のコンセプト数が存在するクラスター番号のみのコンセプト数を出力
df_count_thr <- df_cluster %>%
#  filter(cluster == "Edge Betweenness") %>%
  group_by(cluster,cl_no) %>%
  summarise(cons = n()) %>%
  filter(cons > thr)

### 閾値以上のコンセプト数が存在するクラスター番号のみのデータフレームを作成する
x <- as.vector(df_count_thr$cluster)
y <- as.vector(df_count_thr$cl_no)
list_con_thr <- map2(x,y, function(x,y){
  df_cluster %>%
    filter(cluster == x & cl_no == y) %>%
    select(cluster, cl_no, name)
})
df_con_thr <- list.rbind(list_con_thr)
### class中心性を追加
df_con_thr <- df_con_thr %>% left_join(cluster_df[,c("name","class","bet","deg")],by="name")
library(knitr)
### 表を作成
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