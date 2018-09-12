library("dplyr")
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
setwd("~/pj/ONS/dat/")
class_name <- read.csv("class_name.csv",header=TRUE)
class_n_df <- all_df[,c("name","id_concept_class")]
class_merged <- class_n_df %>% left_join(class_name,by="id_concept_class")

### import jason data to network data
write.csv(relation_df[,1:2], file = "MyData.csv",
          row.names = FALSE)
write.csv(class_df, file="MyClass.csv",row.names = FALSE)

