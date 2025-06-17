
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

# Attempting to build the base US-names dictionary:
# MANUALLY DEFINING DELTA AND LAMBDA FOR NOW
us_dictionary = make_bayes_choice_dictionary(
  surnames$name,
  nb$D,
  post$p_mean,
  delta = 0.1,
  lambda = 1000
)


#names = all_surnames
#dictionary = us_dictionary
#lambda = 100
#delta = 0.1
#method = "jw"
#ncores = 10

us_names = standardize_names(
  names = all_surnames,
  dictionary = us_dictionary,
  lambda = 1000,
  delta = 0.1,
  method = "jw",
  ncores = 10
  )
