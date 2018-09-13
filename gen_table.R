library("rlist")
library("dplyr")

setwd("~/pj/ONS/dat/")
df.cent <- read.csv(file="MyCenter.csv")
bet_rank <- df.cent %>% arrange(desc(bet)) %>% head(15)

library(knitr)
kable(bet_rank[,c(1,2,3,5)])

library(ggplot2)
##### class count
base_df <- read.csv(file="MyBase.csv")
class_map <- read.csv(file="MyClass.csv",stringsAsFactors=FALSE)
base_merged <- base_df %>% left_join(class_map, by="src_name")
ggplot(base_merged,aes(name,fill = relflg)) +
  geom_bar() +
  labs(title = "# of concepts by relation_flag") +
  theme(axis.text.x = element_text(angle = -90, size = 6))
