
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
