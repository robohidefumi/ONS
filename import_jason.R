library("rlist")
library("statnet")

### creating ID List
url <- "http://cognitiveatlas.org/api/v-alpha/concept?"
all <- list.load(url)
all_df <- list.stack(all, fill=TRUE)
id_list <- list.map(all, id)

base <- "http://cognitiveatlas.org/api/v-alpha/concept?id="
url_list <- list.apply(id_list, function(x){paste(base,x,sep="")})

#url_list_s <- sample(url_list,10)
### get jason data on each concept
out_list <- list.apply(url_list,function(x){
  con <- list.load(x) # con stands for "concept"
  src_name <- con$name
  class_list <- list.select(con$conceptclasses,src_name,name)
  relation_list <- list.select(con$relationships,src_name,dst_name = name, relationship)
  list(relation = list.stack(relation_list),class = list.stack(class_list))
})
relation_list <- list.map(out_list,relation)
relation_df <- list.rbind(relation_list)
class_list <- list.map(out_list,class)
class_df <- list.rbind(class_list)

### class
setwd("~/work/ONS/")
class_name <- read.csv("class_name.csv",header=TRUE)
class_n_df <- all_df[,c("name","id_concept_class")]
class_merged <- class_n_df %>% left_join(class_name,by="id_concept_class")

### import jason data to network data
write.csv(relation_df[,1:2], file = "MyData.csv",
          row.names = FALSE)
write.csv(class_df, file="MyClass.csv",row.names = FALSE)


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
