
library(nicknamer)

# Drawing names
data = synthetic_name_counts()

# Generating list of string-proximal neighbors
neighbor_list = make_neighbor_list(data$name, method = "jw")

# Drawing the posterior
out = draw_gibbs(data, neighbor_list)
