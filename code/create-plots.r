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

test <- dat_split[[10]]
test <- setDT(dat_split[[10]])
names(test) <- gsub("\\.", "_", names(test))
names(test)[8:10] <- c("hyp0", "hyp1", "hyp2")

test[, .(mean_hyp0 = mean(hyp0),
        mean_hyp1 = mean(hyp1),
        mean_hyp2 = mean(hyp2)),
        by = X_step_]

#both work

#tidyverse
library(dplyr)
test2 <- test %>% group_by(X_step_) %>%
summarize(hyp0 = mean(hyp0),
          hyp1 = mean(hyp1))


#works
#Now apply to list

#Above section as a function
get_means <- function(x){
    x <- setDT(x)
    names(x) <- gsub("\\.", "_", names(x)) #get rid of strange .names
    names(x)[8:10] <- c("hyp0", "hyp1", "hyp2")

    x[, .(mean_hyp0 = mean(hyp0), #out
        mean_hyp1 = mean(hyp1),
        mean_hyp2 = mean(hyp2)),
        by = X_step_]
}

#apply to list
dat_split2 <- lapply(dat_split, get_means)

#recombine
dat2 <- data.table::rbindlist(dat_split2,
                              idcol = "power")

#make wide to long
dat3 <- data.table::melt(dat2,
        id = c("power", "X_step_"),
        measure.vars = c("mean_hyp0",
                        "mean_hyp1",
                        "mean_hyp2"))

#Now ready for plotting
library(ggplot2)

ggplot(dat3) +
geom_line(aes(x = X_step_, y = value,
 color = variable), alpha = 0.8) +
 facet_wrap(~power)

#zoom in
dat_few_ticks <- dat3[dat3$X_step_ < 50,]

ggplot(dat_few_ticks) +
geom_line(aes(x = X_step_, y = value,
 color = variable), alpha = 0.8) +
 facet_wrap(~power)

#zoom in more
dat_30_ticks <- dat3[dat3$X_step_ < 30,]

ggplot(dat_30_ticks) +
geom_line(aes(x = X_step_, y = value,
 color = variable), alpha = 0.3) +
 facet_wrap(~power) +
 xlab("Rounds of studies") +
 ylab("Researcher per hypothesis") +
 ggtitle("Researchers per topic for different true effect size")


#Need to think about whether the mean is really a good summary
# basically, any row with 0 doesn't exist, so the mean
#Becomes whatever did survive longer
#I would need to set all remaining rows to zero for runs
#that did not go further than some tick below 200

#Hmm, this will still end up in a value larger than zero
