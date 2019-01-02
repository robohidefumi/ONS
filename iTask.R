library("dplyr")
library(tidyverse)
library("rlist")
#detach(package:statnet)
library("igraph")
library("intergraph")
library("tikzDevice")

rescale <- function(nchar, low, high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d)) / (max_d-min_d)+low
  rscl
}

setwd("~/pj/ONS/dat/")
tc_df <- read.csv(file="task_concept.csv",stringsAsFactors=FALSE)
iTask <- graph.data.frame(tc_df, directed=FALSE)
V(iTask)$type <- V(iTask)$name %in% tc_df[,1]

shapes <- c("circle","square")
colors <- c("blue","red")

##### ラベルを貼ったら、見づらいし、処理が思い
#plot(iTask,vertex.color = colors[V(iTask)$type+1],
#     vertex.shape=shapes[V(iTask)$type+1],
#     vertex.size=10,vertex.label.degree=-pi/2,
#     vertex.label.dist=1.2,vertex.label.cex=0.9)

##### iTaskそのもののネットワーク図を作ったが、見てもあまり意味ない。

#### R markdown出力用(スライド1枚め)
cat("## Task Ontology Visualization")
cat("\n\n")
plot(iTask,vertex.color = colors[V(iTask)$type+1],
     vertex.shape=shapes[V(iTask)$type+1],
     vertex.size=10,vertex.label=NA)
cat("\n\n")

##### Conceptual Networkを作る 
iTask.pr <- bipartite.projection(iTask)
iTask.con <- iTask.pr$proj1
##### Conceptual Networkの中心性取得
V(iTask.con)$bet <- betweenness(iTask.con)
V(iTask.con)$deg <- degree(iTask.con)

#### R markdown出力用(スライド2枚め)
cat("## Conceptual Network Visualization")
cat("\n\n")
plot(iTask.con, vertex.color="red",
     vertex.shape="circle",vertex.size=rescale(V(iTask.con)$bet,1,20),
     vertex.label=NA)
cat("\n\n")

##### Task Networkを作る 
iTask.task <- iTask.pr$proj2
##### Task中心性を取得
V(iTask.task)$bet <- betweenness(iTask.task)
V(iTask.task)$deg <- degree(iTask.task)

#### R markdown出力用(スライド3枚め)
cat("## Task Network Visualization")
cat("\n\n")
plot(iTask.task, vertex.color="red",
     vertex.shape="circle",vertex.size=rescale(V(iTask.task)$bet,1,20),
     vertex.label=NA)
cat("\n\n")


