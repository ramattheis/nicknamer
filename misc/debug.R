
library(data.table)
library(nicknamer)

surnames = fread("~/Downloads/us_surnames.csv")
surnames = surnames[n>100]
colnames(surnames) = c("string","count")

nb = readRDS("~/Downloads/us_names_nb.rds")

#data = surnames
#neighbor_list = nb
#lambda        = 1
#priors        = list()
#init          = list()
#n_iter        = 100

out = draw_gibbs(surnames, nb, n_iter = 100)
