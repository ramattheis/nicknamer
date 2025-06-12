
library(data.table)
library(nicknamer)

# Reloading the tabulation of all (cleaned) surnames in the census
surnames = fread("~/Downloads/us_surnames.csv")

# Removing surnames with fewer than 10 obs and no missing chars (~4.4% of individuals omitted)
all_surnames = surnames$namelast
surnames = surnames[n>10  & !grepl("\\?",namelast)]

# Renaming columns to match draw_gibbs expectations
colnames(surnames) = c("name","count")

nb = readRDS("~/Downloads/us_names_nb.rds")

post = draw_gibbs(n_obs = surnames$count,
                  D = nb$D,
                  M = nb$M,
                  n_iter = 10)

# Debugging (temp)
n_obs = surnames$count
D = nb$D
M = nb$M
n_iter = 2
delta_init   = 0.1
lambda_init  = 1.0
sd_logit     = 0.1
sd_loglam    = 0.1
alpha_dir    = 1
prior_delta  = function(d) dbeta(d, 9, 1,  log = TRUE)
prior_lambda = function(l) dgamma(l, 1, 0.1, log = TRUE)

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
