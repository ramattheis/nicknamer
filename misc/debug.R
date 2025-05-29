
library(data.table)
library(nicknamer)

surnames = fread("~/Downloads/us_surnames.csv")

all_surnames = surnames$namelast

surnames = surnames[n>100]
colnames(surnames) = c("string","count")

nb = readRDS("~/Downloads/us_names_nb.rds")

post = readRDS("~/Downloads/post_names_all.rds")

us_dictionary =  make_bayes_choice_dictionary(surnames, nb, lambda = 1e-4, post, 6)


