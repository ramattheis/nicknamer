
library(nicknamer)
require(data.table)

# Drawing names
#data = synthetic_name_counts()

names = fread("misc/chunk.csv")

names$namelast = clean_surnames(names$namelast)

colnames(names) = "name"
data = names[,.(count=.N),.(name)]
data = data[order(-count,name)]

# Generating list of string-proximal neighbors
neighbor_list = find_neighbors(data$name, method = "jw", max_dist = 0.1, ncores = 10)

# Drawing the posterior
out = draw_gibbs(data, neighbor_list, n_iter = 40000)
