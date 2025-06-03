
library(data.table)
library(nicknamer)

surnames = fread("~/Downloads/us_surnames.csv")

all_surnames = surnames$namelast

surnames = surnames[n>100 & namelast != ""]
colnames(surnames) = c("string","count")

nb = readRDS("~/Downloads/us_names_nb.rds")

post = readRDS("~/Downloads/post_names_all.rds")

#post$delta_samples = post$delta_samples/3

us_dictionary = make_bayes_choice_dictionary(surnames, nb, lambda = 1e-3, post, 6)

multiple = subset(us_dictionary,  duplicated(observed))$observed
multiple = subset(us_dictionary, observed %in% multiple)
multiple = multiple[order(multiple$observed),]
