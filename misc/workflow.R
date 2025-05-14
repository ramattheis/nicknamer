
library(nicknamer)

# Drawing names
data = synthetic_name_counts()

# Generating list of string-proximal neighbors
neighbor_list = make_neighbor_list(data$name, method = "jw")

# Drawing the posterior
out = draw_gibbs(data, neighbor_list)


## For testing
priors = list()
init   = list()
n_iter = 1000
burnin = 100

