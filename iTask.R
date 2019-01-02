library("dplyr")
library(tidyverse)
library("rlist")
#detach(package:statnet)
library("igraph")
library("intergraph")

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

#### Tex出力用
library("tikzDevice")
tikz("iTask_biparate.tex",width = 5, height = 4)
plot(iTask,vertex.color = colors[V(iTask)$type+1],
     vertex.shape=shapes[V(iTask)$type+1],
     vertex.size=10,vertex.label=NA)
dev.off()

#### R markdown出力用
p1 <- plot(iTask,vertex.color = colors[V(iTask)$type+1],
     vertex.shape=shapes[V(iTask)$type+1],
     vertex.size=10,vertex.label=NA)

#### R markdown出力用
##### Conceptual Networkを作る 
iTask.pr <- bipartite.projection(iTask)
iTask.con <- iTask.pr$proj1
##### Conceptual Networkの中心性取得
V(iTask.con)$bet <- betweenness(iTask.con)
V(iTask.con)$deg <- degree(iTask.con)
##### 作図
p_con <- plot(iTask.con, vertex.color="red",
     vertex.shape="circle",vertex.size=rescale(V(iTask.con)$bet,1,20),
     vertex.label=NA)
