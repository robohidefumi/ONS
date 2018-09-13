library("statnet")
library("dplyr")
library(RColorBrewer)

rescale <- function(nchar, low, high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d)) / (max_d-min_d)+low
  rscl
}


net <- network(net_import)

df.cent <- data.frame(
  con = net %v% 'vertex.names',
  cla = net %v% 'class',
  deg = degree(net_import,gmode="graph",cmode="indegree"),
  cls = closeness(net_import,gmode="graph"),
  bet = betweenness(net_import,gmode="graph"),
  evc = evcent(net_import,gmode="graph")
)

### legend_class
my_pal <- brewer.pal(10,"Paired")
classcat <- as.factor(get.vertex.attribute(net_import,"class"))
op <- par(mar=c(2,0,2,0))
plot(net_import,vertex.cex=rescale(df.cent$bet,1,7),vertex.col=my_pal[classcat],main="Concepts colored by Class")
legend("bottomleft",legend=levels(classcat),
       col=my_pal,pch=19, pt.cex=0.5, bty="n",cex=0.5,
       title="Concept Class")
par(op)


###### misc
op <- par(mar = rep(0,4), mfrow=c(1,2))
plot(net_import,vertex.cex=rescale(deg,1,7))
par(op)

op <- par(mar = rep(0,4), mfrow=c(1,2))
plot(net_import,vertex.cex=rescale(bet,1,7))
par(op)


### label 
op <- par(mar = rep(0,4), mfrow=c(1,2))
plot(net_import,vertex.cex=rescale(bet,1,7),displaylabels=TRUE, label.cex=0.2)
par(op)


op <- par(mar = rep(0,4), mfrow=c(1,2))
plot(net_import,mode="fruchtermanreingold",vertex.cex=1.5)
par(op)

op <- par(mar = rep(0,4), mfrow=c(1,2))
plot(net_import,vertex.cex=log(deg))
par(op)

op <- par(mar = rep(0,4), mfrow=c(1,2))
plot(net_import,vertex.cex=log(bet)/10)
par(op)
