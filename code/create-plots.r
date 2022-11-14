# Project: Ghost fields
# Script purpose: Create plots
# Code: Anton Olsson Collentine (anton@olssoncollentine.com)

#******************************************************



library(data.table)

dat <- read.csv("data/2-hyps-1-null-reluctant.csv",
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

# Summary for two hypotheses
#***************************

#We will use only the non-null, this summarizes well the rest
improve_names <- function(x){
    x <- setDT(x)
    names(x) <- gsub("\\.", "_", names(x)) #get rid of strange .names
    names(x)[10:11] <- c("hyp0", "hyp1")
    x
}

#apply to list
dat_split2 <- lapply(dat_split, improve_names)

#create list of means
#recombine
dat2 <- data.table::rbindlist(dat_split2)

rep_means <- dat2[,.(Null = mean(hyp0),
                    Non_null = mean(hyp1)),
                  by = .(power, X_step_)]


#make wide to long
rep_means_long <- data.table::melt(rep_means,
        id = c("power", "X_step_"),
        measure.vars = c("Null",
                        "Non_null"),
        variable.name = "Effect_size")


library(ggplot2)

ggplot(dat2) +
geom_line(aes(x = X_step_, y = hyp0, #raw curves, hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, #raw curves, hyp1!
             group = X_run_number_),
          alpha = 0.01) +
          ylab("Researcher studying non-null") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long, #means
        aes(x = X_step_, y = value, color = Effect_size)) + #hyp1
        facet_wrap(~power) +
        theme_bw() +
        ggtitle("One of two effects is non-null with a power of..")

ggsave("figures/2-hyp-1-non-null.png")





# Summary for three hypotheses
#***************************
library(data.table)

dat <- read.csv("data/3-hyps-1-non-null.csv",
skip = 6)

dat_split <- split(dat, dat$power)


improve_names <- function(x){
    x <- setDT(x)
    names(x) <- gsub("\\.", "_", names(x)) #get rid of strange .names
    #EDIT: check indices below when updating dataset!
    names(x)[9:11] <- c("hyp0", "hyp1", "hyp2") #for multiple hyps
    x
}

#apply to list
dat_split2 <- lapply(dat_split, improve_names)

#create list of means
#recombine
dat2 <- data.table::rbindlist(dat_split2)

rep_means <- dat2[,.(Null_1 = mean(hyp0),
                    Null_2 = mean(hyp1),
                    Non_null = mean(hyp2)),
                  by = .(power, X_step_)]


#make wide to long
rep_means_long <- data.table::melt(rep_means,
        id = c("power", "X_step_"),
        measure.vars = c("Null_1",
                        "Null_2",
                        "Non_null"),
        variable.name = "Effect_size")

library(ggplot2)

ggplot(dat2) +
geom_line(aes(x = X_step_, y = hyp0, # hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, # hyp1!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp2, # hyp2!
          group = X_run_number_),
          alpha = 0.01) +
          ylab("Researcher studying non-null") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long, #means
        aes(x = X_step_, y = value, color = Effect_size)) +
        facet_wrap(~power) +
        theme_bw() +
        ggtitle("One of three effects is non-null with a power of..")

ggsave("figures/3-hyp-1-non-null.png")




# Summary for three hypotheses, 2 equal non-null
#***************************
dat <- read.csv("data/3-hyps-2-non-null-table.csv",
skip = 6)

dat_split <- split(dat, dat$power)


improve_names <- function(x){
    x <- setDT(x)
    names(x) <- gsub("\\.", "_", names(x)) #get rid of strange .names
    #EDIT: check indices below when updating dataset!
    names(x)[9:11] <- c("hyp0", "hyp1", "hyp2") #for multiple hyps
    x
}

#apply to list
dat_split2 <- lapply(dat_split, improve_names)

#create list of means
#recombine
dat2 <- data.table::rbindlist(dat_split2)

rep_means <- dat2[,.(Null = mean(hyp0),
                    Non_null_1 = mean(hyp1),
                    Non_null_2 = mean(hyp2)),
                  by = .(power, X_step_)]


#make wide to long
rep_means_long <- data.table::melt(rep_means,
        id = c("power", "X_step_"),
        measure.vars = c("Null",
                        "Non_null_1",
                        "Non_null_2"),
        variable.name = "Effect_size")


#Plot
ggplot(dat2) +
geom_line(aes(x = X_step_, y = hyp0, # hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, # hyp1!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp2, # hyp2!
          group = X_run_number_),
          alpha = 0.01) +
          ylab("Researcher studying non-null") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long, #means
        aes(x = X_step_, y = value, color = Effect_size)) +
        facet_wrap(~power) +
        theme_bw() +
        ggtitle("Two of three effects is non-null with a power of..")

ggsave("figures/3-hyp-2-non-null.png")


# Summary for three hypotheses, 2 varying non-null
#***************************
dat <- read.csv("data/3-hyps-2-varying-non-null.csv",
skip = 6)

dat_split <- split(dat, dat$power)

improve_names <- function(x){
    x <- setDT(x)
    names(x) <- gsub("\\.", "_", names(x)) #get rid of strange .names
    names(x)[10:12] <- c("hyp0", "hyp1", "hyp2") #for multiple hyps
    x
}

#apply to list
dat_split2 <- lapply(dat_split, improve_names)

#create list of means
#recombine
dat2 <- data.table::rbindlist(dat_split2)

## How to summarize this in a nice way?
## Maybe just show all the lines?

rep_means <- dat2[,.(Null = mean(hyp0),
                    Power_var = mean(hyp1),
                    Power_0.8 = mean(hyp2)),
                  by = .(power, X_step_)]

#make wide to long
rep_means_long <- data.table::melt(rep_means,
        id = c("power", "X_step_"),
        measure.vars = c("Null",
                        "Power_var",
                        "Power_0.8"))
#Plot must be different then, can't split by power anymore

ggplot(dat2) +
geom_line(aes(x = X_step_, y = hyp0, # hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, # hyp1!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp2, # hyp2!
             group = X_run_number_),
          alpha = 0.01) +
          ylab("Researchers studying null") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long, #means
        aes(x = X_step_, y = value, color = variable)) +
        facet_wrap(~power) +
        theme_bw() +
        ggtitle("Two of three effects is non-null with a power of..")


ggsave("figures/3-hyp-2-varying-non-null.png")

# some tests*************************************
test <- dat_split[[10]]
test <- setDT(dat_split[[10]])
names(test) <- gsub("\\.", "_", names(test))
names(test)[9:11] <- c("hyp0", "hyp1", "hyp2")

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
    names(x)[9:11] <- c("hyp0", "hyp1", "hyp2")

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


#************************************
