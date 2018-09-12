library("rlist")
library("dplyr")

setwd("~/pj/ONS/dat/")
df.cent <- read.csv(file="MyCenter.csv")
bet_rank <- df.cent %>% arrange(desc(bet)) %>% head(15)

### creating ID List
url <- "http://cognitiveatlas.org/api/v-alpha/concept?"
all <- list.load(url)
all_df <- list.stack(all, fill=TRUE)

class_name <- read.csv("class_name.csv",header = TRUE)
class_df <- all_df[,c("name","id_concept_class")]
class_merged <- left_join(class_df,class_name,by="id_concept_class")
class_table_all <- class_merged %>% group_by(class_name) %>% summarize(count = n()) #%>% arrange(desc(count))
class_table_rel <- df_c_merged %>% group_by(name) %>% summarize(count = n()) #%>% arrange(desc(count))
class_table <- data.frame(
  class = as.vector(class_table_all[,1]),
  a_count = class_table_all[,2],
  v_count =class_table_rel[,2]
)
colnames(class_table) <- c("class","a_count","v_count")
class_table <- class_table %>% arrange(desc(a_count))
class_table <- mutate(class_table, ratio = round((v_count/a_count * 100),digits = 1))
# Insert TTL number -> need more sophisticated expression
l_a_count <- class_table %>% summarise_at(c("a_count"), sum)
l_a <- as.numeric(l_a_count)
l_v_count <- class_table %>% summarise_at(c("v_count"), sum)
l_v <- as.numeric(l_v_count)

l_row <- data.frame(
  class = "TTL",
  a_count = l_a_count,
  v_count = class_table %>% summarise_at(c("v_count"), sum),
  ratio = round((l_v/l_a *100 ), digits=1)
)
class_table <- class_table %>% bind_rows(l_row)


library(knitr)
kable(bet_rank[,c(1,2,3,5)])
kable(class_table)

library(ggplot2)
ggplot(class_merged,aes(class_name)) +
  geom_bar()
ggplot(df_c_merged,aes(name)) +
  geom_bar()