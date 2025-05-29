
library(data.table)
library(nicknamer)

nicknamer::clean_surnames(c("Smith","Sm*th","&ndrew","S-----","---","O'connor","O connor","12345","Johnson****","xy","wu"))

surnames = fread("~/Downloads/us_surnames.csv")

all_surnames = surnames$namelast

surnames = surnames[n>100]
colnames(surnames) = c("string","count")

nb = readRDS("~/Downloads/us_names_nb.rds")

post = readRDS("~/Downloads/post_names_all.rds")

#post$delta_samples = post$delta_samples/3

us_dictionary =  make_bayes_choice_dictionary(surnames, nb, lambda = 1e-3, post, 6)

# Genrating a training sample
disagreements = subset(us_dictionary, standard != observed )

colnames(surnames) = c("observed","n")
disagreements = merge(disagreements, surnames, by ="observed")
disagreements = as.data.table(disagreements)
disagreements[,d := stringdist::stringdist(standard, observed, method = "jw", p = 0.2)]
