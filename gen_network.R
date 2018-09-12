library("statnet")
library("dplyr")
library(RColorBrewer)

setwd("~/pj/ONS/dat/")
net.relation <- read.csv(file="MyData.csv")
class_map <- read.csv(file="MyClass.csv",stringsAsFactors=FALSE)

net_import <- network(net.relation,
                      directed = FALSE,
                      matrix.type="edgelist")

df_c_attribute <- data.frame(
  src_name = net_import %v% 'vertex.names'
)
df_c_merged <- df_c_attribute %>% left_join(class_map,by="src_name")
net_import <- set.vertex.attribute(net_import,"class",df_c_merged[,2])

### creating adjacent matrix
adjacent <- as.sociomatrix(net_import)
write.csv(adjacent, file = "adjacent.csv")

### Centrality
net <- network(net_import)
df.cent <- data.frame(
  con = net %v% 'vertex.names',
  cla = net %v% 'class',
  deg = degree(net_import,gmode="graph",cmode="indegree"),
  cls = closeness(net_import,gmode="graph"),
  bet = betweenness(net_import,gmode="graph"),
  evc = evcent(net_import,gmode="graph")
)
write.csv(df.cent, file="MyCenter.csv",row.names = FALSE)

### Visualization

#### ggplot
library(ggplot2)
# use display.brewer.all() to see all options

##### deg histgram
ggplot(df.cent, aes(deg)) + geom_histogram()
ggplot(df.cent, aes(deg)) + geom_freqpoly()

ggplot(df.cent, aes(deg, colour = cla)) + geom_freqpoly(binwidth = 0.5)
ggplot(df.cent, aes(deg, fill = cla)) + 
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~cla, ncol = 1)

##### deg & bet map
ggplot(df.cent, aes(deg,bet)) +
  geom_point(aes(colour = cla)) 
#  + scale_colour_brewer(palette="Set3")
ggplot(df.cent, aes(deg,bet)) +
  geom_point() +
  facet_wrap(~cla)

##### 

#### Network Map
rescale <- function(nchar, low, high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d)) / (max_d-min_d)+low
  rscl
}

my_pal <- brewer.pal(10,"Paired")
classcat <- as.factor(get.vertex.attribute(net_import,"class"))
op <- par(mar=c(2,0,2,0))
plot(net_import,vertex.cex=rescale(df.cent$bet,1,7),vertex.col=my_pal[classcat],main="Concepts colored by Class")
### legend_class
legend("bottomleft",legend=levels(classcat),
       col=my_pal,pch=19, pt.cex=0.5, bty="n",cex=0.5,
       title="Concept Class")
par(op)
