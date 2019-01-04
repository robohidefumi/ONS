library("dplyr")
library(tidyverse)
library("rlist")
setwd("~/pj/ONS/dat/")
library("statnet")
net.relation <- read.csv(file="MyData.csv")
class_map <- read.csv(file="MyClass.csv",stringsAsFactors=FALSE)

net_import <- network(net.relation,
                      directed = FALSE,
                      matrix.type="edgelist")
net <- network(net_import)

df.cent <- data.frame(
  name = as.character(net %v% 'vertex.names'),
  cla = net %v% 'class',
  deg = degree(net_import,gmode="graph",cmode="indegree"),
  cls = closeness(net_import,gmode="graph"),
  bet = betweenness(net_import,gmode="graph"),
  evc = evcent(net_import,gmode="graph")
)

detach(package:statnet)
library("igraph")
library("intergraph")
library("tikzDevice")

rescale <- function(nchar, low, high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d)) / (max_d-min_d)+low
  rscl
}

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

##### 中心性の高い項目をあぶり出す
# Concept
con_df <- data.frame(
  name = as.character(V(iTask.con)$name),
  bet = V(iTask.con)$bet,
  deg = V(iTask.con)$deg
)

g <- ggplot(con_df, aes(deg,bet)) +
  geom_point() +
  geom_text(
    data=subset(con_df, deg > 40 & bet > 4000),
    aes(deg,bet,label=name))

#### R markdown出力用(スライド4枚め)
cat("## Concept Network Centrality")
cat("\n\n")
print(g)
cat("\n\n")

# Task
task_df <- data.frame(
  name = V(iTask.task)$name,
  bet = V(iTask.task)$bet,
  deg = V(iTask.task)$deg
)

g <- ggplot(task_df, aes(deg,bet)) +
  geom_point() +
  geom_text(
    data=subset(task_df, deg > 50 & bet > 3000),
    aes(deg,bet,label=name))

#### R markdown出力用(スライド5枚め)
cat("## Task Network Centrality")
cat("\n\n")
print(g)
cat("\n\n")

#### concept中心性差分の取得（concept-concept linkとbiparate conceptual network linkの比較）
new_df_con <- df.cent %>% left_join(con_df,by="name")
new_df_con <- new_df_con %>% mutate(deg.diff = abs(deg.x - deg.y))
new_df_con <- new_df_con %>% mutate(bet.diff = abs(bet.x - bet.y))

g <- ggplot(new_df_con, aes(deg.y,bet.y)) +
  geom_point(aes()) + 
  geom_text(
    data=subset(new_df_con, deg.y > 40 | bet.y > 6000),
    aes(deg.y,bet.y,label=name))

#### R markdown出力用(スライド5枚め)
cat("## Curated Conceptual Network Centrality")
cat("\n\n")
print(g)
cat("\n\n")

g <- ggplot(new_df_con, aes(deg.diff,bet.diff)) +
  geom_point(aes()) + 
  geom_text(
    data=subset(new_df_con, bet.diff > 10000),
    aes(deg.diff,bet.diff,label=name))

#### R markdown出力用(スライド5枚め)
cat("## Diff of Centrality between Curated & Biparete")
cat("\n\n")
print(g)
cat("\n\n")

#### 複数ネットワークの比較
iCon <- graph.data.frame(net.relation, directed=FALSE)
iCon <- simplify(iCon) #重複するエッジをユニークにする
###不要になった
#intGraph <- graph.intersection(iCon,iTask.con)

# iTask.conに存在するiCon要素の評価
a_dup <- V(iCon)$name %in% V(iTask.con)$name
# iConに存在するiTask.con要素の評価
b_dup <- V(iTask.con)$name %in% V(iCon)$name
# ネットワークのdup値に評価結果を格納する
V(iCon)$dup <- a_dup
V(iTask.con)$dup <- b_dup


iTask_dup_df <- data.frame(
  name = V(iTask.con)$name,
  bet_dup = betweenness(iTask.con),
  deg_dup = degree(iTask.con),
  dup = V(iTask.con)$dup
)

iTask_df <- df.cent %>% left_join(iTask_dup_df,by="name")

g <- ggplot(subset(iTask_df,dup == TRUE), aes(bet_dup,deg_dup)) +
  geom_point() +
  geom_text(
    data=subset(iTask_df, deg_dup > 40 | bet_dup > 5000),
    aes(bet_dup,deg_dup,label=name))

cat("## Biparete Centrality only exisiting on Curated")
cat("\n\n")
print(g)
cat("\n\n")

iCon_dup_df <- data.frame(
  name = V(iCon)$name,
  bet_dup = betweenness(iCon),
  deg_dup = degree(iCon),
  dup = V(iCon)$dup
)

iCon_df <- df.cent %>% left_join(iCon_dup_df,by="name")

g <- ggplot(subset(iCon_df,dup == TRUE), aes(bet_dup,deg_dup)) +
  geom_point() +
  geom_text(
    data=subset(iCon_df, deg_dup > 40 | bet_dup > 5000),
    aes(bet_dup,deg_dup,label=name))

cat("## Curated Centrality only exisiting on Biparate")
cat("\n\n")
print(g)
cat("\n\n")
