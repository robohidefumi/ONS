library("dplyr")

network.size(net_import)
den <- gden(net_import)
lgc <- component.largest(net_import,result="graph")
gd <- geodist(lgc)
dia <- max(gd$gdist)
cl_co <- gtrans(net_import,mode="graph")

all_df %>% group_by(id_concept_class) %>% summarize(count = n())
class_merged %>% group_by(class_name) %>% summarize(count = n())
edge_df %>% group_by(relationship) %>% summarize(count = n())

deg_rank <- df.cent %>% arrange(desc(deg)) %>% head(30)
bet_rank <- df.cent %>% arrange(desc(bet)) %>% head(30)
evc_rank <- df.cent %>% arrange(desc(evc)) %>% head(30)
cent_rank <- cbind(deg_rank[,c(1,2)],bet_rank[,c(1,4)],evc_rank[,c(1,5)])