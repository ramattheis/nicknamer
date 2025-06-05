
library(data.table)
library(stringdist)
library(knitr)

surnames = fread("~/Downloads/us_surnames.csv")

# Finding Rosencrantz and Guildensterns
rd = stringdist(surnames$namelast,"rosencrantz",method = "jw")
rosencrantzs = surnames[rd < 0.15]
rosencrantzs$`Jaro-Winkler Distance` <- stringdist(rosencrantzs$namelast,"rosencrantz",method = "jw")
kable(cbind(head(rosencrantzs,n=10),  tail(rosencrantzs, n=10))  )

gd = stringdist(surnames$namelast,"guildenstern",method = "jw")
guildensterns = surnames[gd < 0.15]
guildensterns$`Jaro-Winkler Distance` <- stringdist(guildensterns$namelast,"guildenstern",method = "jw")
kable(cbind(head(guildensterns,n=10),  tail(guildensterns, n=10))  )
