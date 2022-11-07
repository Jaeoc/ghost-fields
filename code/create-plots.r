# Project: Ghost fields
# Script purpose: Create plots
# Code: Anton Olsson Collentine (anton@olssoncollentine.com)

#******************************************************



library(data.table)

dat <- read.csv("data/ghost-field-unbiased experiment-table.csv",
skip = 6)

dat_split <- split(dat, dat$power)

# We have tree lines per power level + 100 reps
# Seems to always be hypothesis = 2 that is non-null

#Option 1) compute average for each hypothesis per power level
# Option 2) Only plot hyp = 2
#Then can plot average and grey out the reps
#And create a summary plot across powers

#First, create a wrapped plot of the averages
#i.e. one facet for each power level, 3 lines each

#take the mean at each step of the counts
dat_split2 <- lapply(dat_split, function(x){
    x[, .(hyp0 = mean(x$count.researchers.with..hypothesis...0.),
    hyp1 = mean(x$count.researchers.with..hypothesis...1.),
    hyp2 = mean(x$count.researchers.with..hypothesis...2.)),
    by = .(X.step.)]
})

test <- setDT(dat_split[[10]])
test[, .(hyp0 = mean(test$count.researchers.with..hypothesis...0.),
    hyp1 = mean(test$count.researchers.with..hypothesis...1.),
    hyp2 = mean(test$count.researchers.with..hypothesis...2.)),
    by = test$X.step.]

#Not working.
