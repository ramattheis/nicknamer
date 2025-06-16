
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

post = readRDS("~/Downloads/post_names_all.rds")

Rcpp::sourceCpp("src/make_bayes_choice_dictionary_cpp.cpp")

# Attempting to build the base US-names dictionary:
us_dictionary = make_bayes_choice_dictionary_cpp(
  surnames$name,
  nb$D,
  post$p_mean,
  median(post$delta),
  median(post$lambda)
)

# descriptive stats for the posterior
post_list <- lapply(seq_len(25), function(i) {
  list(
    delta      = post$delta[((i - 1) * 1000 + 1):(i*1000)],
    lambda     = post$lambda[((i - 1) * 1000 + 1):(i*1000)],
    likelihood = post$likelihood[((i - 1) * 1000 + 1):(i*1000)],
    p5         = post$p5[((i - 1) * 1000 + 1):(i*1000), , drop = FALSE]
  )
  })

post_ess_delta = lapply(post_list, function(pl) coda::effectiveSize(pl$delta)) |> unlist() |> sum()
post_ess_lambda =  lapply(post_list, function(pl) coda::effectiveSize(pl$lambda)) |> unlist() |> sum()

rhat_delta = coda::gelman.diag(lapply(post_list, function(pl) coda::mcmc(pl$delta)))
rhat_lambda = coda::gelman.diag(lapply(post_list, function(pl) coda::mcmc(pl$lambda)))
# Inference for lambda seems to have failed... very high rhat, plots look bimodal.


us_names = standardize_names(
  names = all_surnames,
  dictionary = us_dictionary,
  lambda = 1e-3,
  delta = 0.017,
  method = "jw",
  ncores = 10
  )
