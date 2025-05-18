
library(nicknamer)
require(data.table)

# Drawing names
#data = synthetic_name_counts()

names = fread("misc/chunk.csv")
colnames(names) = "name"
data = names[,.(n=.N),.(name)]
data = data[order(-n,name)]

# Generating list of string-proximal neighbors
neighbor_list = find_neighbors(data$name, method = "jw", ncores = 6)

# Drawing the posterior
out = draw_gibbs(data, neighbor_list)
