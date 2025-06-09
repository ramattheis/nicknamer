
library(data.table)
library(nicknamer)

surnames = fread("~/Downloads/us_surnames.csv")

surnames = surnames[namelast != ""]
all_surnames = surnames$namelast
surnames = surnames[n>100]

colnames(surnames) = c("name","count")

nb = readRDS("~/Downloads/us_names_nb.rds")

# Debugging (temp)
data = surnames
neighbor_list = nb
lambda = 0.1
n_iter = 10
Rcpp::sourceCpp("src/gibbs_helper_temp.cpp")
library(Matrix)
priors = list()
init = list()
init = 1

post = draw_gibbs(surnames, nb, n_iter = 100)

post = readRDS("~/Downloads/post_names_all.rds")

us_dictionary = make_bayes_choice_dictionary(surnames, nb, lambda = 1e-3, post, 6)

test = subset(us_dictionary, standard == "miller") |> as.data.table()
test[, jw := stringdist(observed,"miller", method = "jw") ]
test

us_names = standardize_names(
  names = all_surnames,
  dictionary = us_dictionary,
  lambda = 1e-3,
  delta = 0.017,
  method = "jw",
  ncores = 10
  )
