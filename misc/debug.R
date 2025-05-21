
library(data.table)
library(nicknamer)

surnames = fread("~/Downloads/us_surnames.csv")

all_surnames = surnames$namelast

surnames = surnames[n>100]
colnames(surnames) = c("string","count")

nb = readRDS("~/Downloads/us_names_nb.rds")

post = readRDS("~/Downloads/post_names_1.rds")

surnames[, p := post$p_avg]
surnames[, post := post$x_avg]

surnames$post
