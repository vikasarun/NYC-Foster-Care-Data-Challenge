# Robert Frierson - Team Columbia - 3/31/17
# ********** NYC Foster Care Data Challenge **********

# set working directory
setwd("~/Documents/Columbia/NYC Foster Care Data Challenge/")

# install packages
install.packages("leaps")
install.packages("glmnet")
install.packages("ISLR")
install.packages("tree")
install.packages("MASS")
install.packages("class")
library(leaps)
library(glmnet)
library(ISLR)
library(tree)
library(MASS)
library(class)

# create OrangeJuice table
afcars <-read.csv("afcars_foster_clean.csv")

# create data frame of afcars without NA results for discharge reason
discharges <- afcars[complete.cases(afcars[, "DISREASN"]), ]

# histogram of discharge reason
hist(discharges$DISREASN)

# getting an idea of discharge reason vs. child attributes
attach(discharges)

# create data sets for each child sex
boys <- discharges[SEX == 1,]
girls <- discharges[SEX == 2,]

# create data sets for each child attribute
amiakns <- discharges[AMIAKN == 1,]
asians <- discharges[ASIAN == 1,]
blacks <- discharges[BLKAFRAM == 1,]
hawaiians <- discharges[HAWAIIPI == 1,]
whites <- discharges[WHITE == 1,]
norace <- discharges[UNTODETM == 1,]
hispanics <- discharges[HISORGIN == 1,]
nrow(amiakns) + nrow(asians) + nrow(blacks) + nrow(hawaiians) + nrow(whites) + nrow(norace) + nrow(hispanics)

# create data sets for each foster family attribute
hist(discharges$FOSFAMST)
married <- discharges[FOSFAMST == 1,]
married <- married[complete.cases(married[, "FOSFAMST"]), ]
unmarried <- discharges[FOSFAMST == 2,]
unmarried <- unmarried[complete.cases(unmarried[, "FOSFAMST"]), ]
female <- discharges[FOSFAMST == 3,]
female <- female[complete.cases(female[, "FOSFAMST"]), ]
male <- discharges[FOSFAMST == 4,]
male <- male[complete.cases(male[, "FOSFAMST"]), ]
nrow(married) + nrow(unmarried) + nrow(female) + nrow(male)

detach(discharges)

# find % w/ positive discharge outcomes for child attributes
avg_success <- nrow(discharges[discharges$DISREASN >= 1 & discharges$DISREASN <= 5 & discharges$DISREASN != 4, ]) / nrow(discharges)
boys_success <- nrow(boys[boys$DISREASN >= 1 & boys$DISREASN <= 5 & boys$DISREASN != 4, ]) / nrow(boys)
girls_success <- nrow(girls[girls$DISREASN >= 1 & girls$DISREASN <= 5 & girls$DISREASN != 4, ]) / nrow(girls)
amiakns_success <- nrow(amiakns[amiakns$DISREASN >= 1 & amiakns$DISREASN <= 5 & amiakns$DISREASN != 4, ]) / nrow(amiakns)
asians_success <- nrow(asians[asians$DISREASN >= 1 & asians$DISREASN <= 5 & asians$DISREASN != 4, ]) / nrow(asians)
blacks_success <- nrow(blacks[blacks$DISREASN >= 1 & blacks$DISREASN <= 5 & blacks$DISREASN != 4, ]) / nrow(blacks)
hawaiians_success <- nrow(hawaiians[hawaiians$DISREASN >= 1 & hawaiians$DISREASN <= 5 & hawaiians$DISREASN != 4, ]) / nrow(hawaiians)
whites_success <- nrow(whites[whites$DISREASN >= 1 & whites$DISREASN <= 5 & whites$DISREASN != 4, ]) / nrow(whites)
norace_success <- nrow(norace[norace$DISREASN >= 1 & norace$DISREASN <= 5 & norace$DISREASN != 4, ]) / nrow(norace)
hispanics_success <- nrow(hispanics[hispanics$DISREASN >= 1 & hispanics$DISREASN <= 5 & hispanics$DISREASN != 4, ]) / nrow(hispanics)

child_success <- c(avg_success, girls_success, boys_success, amiakns_success, asians_success, blacks_success, hawaiians_success, whites_success, norace_success, hispanics_success)
child_success <- setNames(success, c("avg_success", "girls_success", "boys_success", "amiakns_success", "asians_success", "blacks_success", "hawaiians_success", "whites_success", "norace_success", "hispanics_success"))

# find % w/ positive discharge outcomes for family attributes
avg_success <- nrow(discharges[discharges$DISREASN >= 1 & discharges$DISREASN <= 5 & discharges$DISREASN != 4, ]) / nrow(discharges)
