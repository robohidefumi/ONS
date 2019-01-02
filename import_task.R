library("dplyr")
library(tidyverse)
library("rlist")
detach(package:statnet)
library("igraph")
library("intergraph")

rescale <- function(nchar, low, high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d)) / (max_d-min_d)+low
  rscl
}

### creating ID List
url <- "http://cognitiveatlas.org/api/v-alpha/task?"
all_task <- list.load(url)
all_task_df <- list.stack(all_task, fill=TRUE)

##今日はここではまった
task_base <- "http://cognitiveatlas.org/api/v-alpha/task?id="
task_url_list <- all_task_df$id %>% map(function(x){paste(task_base,x,sep="")})
#task_url_list <- head(task_url_list,50)
task_url_vector <- unlist(task_url_list)
task_out_list <- task_url_vector %>% map(function(x){
  task <- list.load(x)
  task
})

setwd("~/pj/ONS/dat/")
list.save(task_out_list, 'MyTaskList.rdata')

load('MyTaskList.rdata')

bip_list <- list.apply(task_out_list,function(x){
  con <- list.select(x$concepts,t_name = x$name, c_name = name)
  list.stack(con)
})

tc_df <- list.rbind(bip_list)
write.csv(tc_df, file="task_concept.csv",row.names = FALSE)