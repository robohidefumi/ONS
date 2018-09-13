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

#url_list_s <- sample(url_list,30)
### get jason data on each concept
out_list <- list.apply(url_list,function(x){
  con <- list.load(x) # con stands for "concept"
  con
})
setwd("~/pj/ONS/dat/")
list.save(out_list, 'MyList.rdata')

return_list <- list.apply(out_list,function(x){
  base_df <- data.frame(
    src_name = x$name,
    relnum = length(x$relationships),
    clanum = length(x$conceptclasses)
  )
  class_list <- list.select(x$conceptclasses,x$name,name)
  relation_list <- list.select(x$relationships,x$name,dst_name = name, relationship)
  list(base = base_df, relation = list.stack(relation_list),class = list.stack(class_list))
})
base_list <- list.map(return_list,base)
base_df <- list.rbind(base_list)
relation_list <- list.map(return_list,relation)
relation_df <- list.rbind(relation_list)
class_list <- list.map(return_list,class)
class_df <- list.rbind(class_list)

### base_dfにflagをたてる
base_df <- base_df %>% mutate(relflg = 
                     case_when(
                       relnum > 0 ~ TRUE,
                       relnum == 0 ~ FALSE
                     )
                     )
### export jason data to network data
write.csv(base_df, file="MyBase.csv",row.names = FALSE)
write.csv(relation_df[,1:2], file = "MyData.csv",row.names = FALSE)
write.csv(class_df, file="MyClass.csv",row.names = FALSE)
