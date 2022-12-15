# Project: Ghost fields
# Script purpose: Create plots
# Code: Anton Olsson Collentine (anton@olssoncollentine.com)

#******************************************************



library(data.table)


# We have tree lines per power level + 100 reps
# Seems to always be hypothesis = 2 that is non-null

#Option 1) compute average for each hypothesis per power level
# Option 2) Only plot hyp = 2
#Then can plot average and grey out the reps
#And create a summary plot across powers

#First, create a wrapped plot of the averages
#i.e. one facet for each power level, 3 lines each

#take the mean at each step of the counts

# Functions for two hypotheses
#***************************
improve_names <- function(x){
    x <- setDT(x)
    to_replace <- grep("hypothesis", names(dat))

    if (length(to_replace) == 2){
        names(x)[to_replace] <- c("hyp0", "hyp1")
    } else if(length(to_replace) == 3){
        names(x)[to_replace] <- c("hyp0", "hyp1", "hyp2")
    }

    names(x) <- gsub("\\.", "_", names(x)) #also get rid of strange .names
    x
}

clean_hyps <- function(dat){
    dat_split <- split(dat, dat$power)
    dat_split2 <- lapply(dat_split, improve_names)
    #recombine
    data.table::rbindlist(dat_split2)
}

create_means <- function(dat2, summary_var = c("power", "journal_publication_bias", "proportion_biased_researchers",
"p_hacking_success")){
    n_hyps <- grep("^hyp", names(dat2))
    if (length(n_hyps) == 2){
        cols_to_average <- paste0("hyp", 0:1)
    } else if (length(n_hyps) == 3){
        cols_to_average <- paste0("hyp", 0:2)
    }

    #average by group
    if(summary_var == "power"){
    rep_means <- dat2[,lapply(.SD, mean),
                  by = .(power, X_step_),
                  .SDcols = cols_to_average] # See Section 2f:https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
    }else if(summary_var == "journal_publication_bias"){

    rep_means <- dat2[,lapply(.SD, mean),
                  by = .(journal_publication_bias, X_step_),
                  .SDcols = cols_to_average]

    }else if(summary_var == "proportion_biased_researchers"){

    rep_means <- dat2[,lapply(.SD, mean),
                  by = .(proportion_biased_researchers, X_step_),
                  .SDcols = cols_to_average]

    } else if(summary_var == "p_hacking_success"){

    rep_means <- dat2[,lapply(.SD, mean),
                  by = .(p_hacking_success, X_step_),
                  .SDcols = cols_to_average]

    }


    #make wide to long
   rep_means_long <-  data.table::melt(rep_means,
            id = c(summary_var, "X_step_"),
            #NB! when measure.vars missing, melts all non-id vars
            #Hence, doesn't matter if two or three columns
            variable.name = "Effect_size")
}


#********************************
#2 hyps
dat <- read.csv("data/2-hyps-1-null.csv",
skip = 6)
dat <- read.csv("data/2-hyps-1-null-rational.csv",
skip = 6)

dat2 <- clean_hyps(dat)
rep_means_long <- create_means(dat2, summary_var = "power")
levels(rep_means_long$Effect_size) <- c("Null", "Non-null")

#3 hyps, 1 non-null
dat <- read.csv("data/3-hyps-1-non-null.csv",
skip = 6)
dat2 <- clean_hyps(dat)
rep_means_long <- create_means(dat2, summary_var = "power")
levels(rep_means_long$Effect_size) <- c("Null_1", "Null_2", "Non_null")

# 3 hyps, 2 equal non-null
dat <- read.csv("data/3-hyps-2-non-null-table.csv",
skip = 6)
dat2 <- clean_hyps(dat)
rep_means_long <- create_means(dat2, summary_var = "power")
levels(rep_means_long$Effect_size) <- c("Null", "Non-Null_1",
 "non-Null_2")

#3 hyps, one fixed, one varying non-null
dat <- read.csv("data/3-hyps-2-varying-non-null.csv",
skip = 6)
dat2 <- clean_hyps(dat)
rep_means_long <- create_means(dat2, summary_var = "power")
levels(rep_means_long$Effect_size) <- c("Null", "var_power", "power_0.8")

#3 hyps, one fixed, one varying non-null, rational agents
dat <- read.csv("data/3-hyps-2-varying-non-null-rational.csv",
skip = 6)
dat2 <- clean_hyps(dat)
rep_means_long <- create_means(dat2, summary_var = "power")
levels(rep_means_long$Effect_size) <- c("Null", "var_power", "power_0.8")

#Publication bias
dat <- read.csv("data/3-hyps-publication-bias-30-power.csv",
skip = 6)
dat2 <- clean_hyps(dat)
rep_means_long <- create_means(dat2, summary_var = "journal_publication_bias")
levels(rep_means_long$Effect_size) <- c("Null", "power_0.3", "power_0.8")

# p-hacking plot, varying proportion biased
dat <- read.csv("data/3-hyps-0.3-0.5-p-hacking-table.csv",
skip = 6)
dat2 <- clean_hyps(dat)
rep_means_long <- create_means(dat2, summary_var = "proportion_biased_researchers")
levels(rep_means_long$Effect_size) <- c("Null", "power_0.3", "power_0.5")

# p-hacking plot, all biased, varying success rate
dat <- read.csv("data/p-hacking-0.3-0.5-power-all-biased-table.csv",
skip = 6)
dat2 <- clean_hyps(dat)
rep_means_long <- create_means(dat2, summary_var = "p_hacking_success")
levels(rep_means_long$Effect_size) <- c("Null", "power_0.3", "power_0.5")

#***************************
#Plot
#***************************
library(ggplot2)

#2 hyps
ggplot(dat2[X_step_ <= 50,]) +
geom_line(aes(x = X_step_, y = hyp0, #raw curves, hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, #raw curves, hyp1!
             group = X_run_number_),
          alpha = 0.01) +
          ylab("Researchers per hypothesis") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long[X_step_ <= 50,], #means
        aes(x = X_step_, y = value, color = Effect_size)) + #hyp1
        facet_wrap(~power) +
        theme_bw()

# ggsave("figures/2-hyp-1-null.png")
# ggsave("figures/2-hyp-1-null-reluctant.png")
# ggsave("figures/2-hyp-1-null-rational.png")

#3 hyps
ggplot(dat2) +
geom_line(aes(x = X_step_, y = hyp0, #raw curves, hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, #raw curves, hyp1!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp2, #raw curves, hyp2!
             group = X_run_number_),
          alpha = 0.01) +
          ylab("Researchers per hypothesis") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long, #means
        aes(x = X_step_, y = value, color = Effect_size)) + #hyp1
        facet_wrap(~power) +
        theme_bw()



# ggsave("figures/3-hyp-1-non-null.png")
# ggsave("figures/3-hyp-2-non-null.png")
ggsave("figures/3-hyp-2-varying-non-null.png", width = 1.6*6, height = 6)
# ggsave("figures/3-hyp-2-varying-non-null-rational.png", width = 1.6*6, height = 6)


# Publication bias plot
ggplot(dat2) +
geom_line(aes(x = X_step_, y = hyp0, #raw curves, hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, #raw curves, hyp1!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp2, #raw curves, hyp2!
             group = X_run_number_),
          alpha = 0.01) +
          ylab("Researchers per hypothesis") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long, #means
        aes(x = X_step_, y = value, color = Effect_size)) + #hyp1
        facet_wrap(~journal_publication_bias) +
        theme_bw()


 ggsave("figures/3-hyp-pub-bias-power-0.3.png", width = 1.6*6, height = 6)

# p-hacking plot, varying proportion biased
ggplot(dat2) +
geom_line(aes(x = X_step_, y = hyp0, #raw curves, hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, #raw curves, hyp1!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp2, #raw curves, hyp2!
             group = X_run_number_),
          alpha = 0.01) +
          ylab("Researchers per hypothesis") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long, #means
        aes(x = X_step_, y = value, color = Effect_size)) +
        facet_wrap(~proportion_biased_researchers) +
        theme_bw()

 ggsave("figures/p-hacking.png", width = 1.6*6, height = 6)

# p-hacking plot, all biased
ggplot(dat2[X_step_ <= 50,]) +
geom_line(aes(x = X_step_, y = hyp0, #raw curves, hyp0!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp1, #raw curves, hyp1!
             group = X_run_number_),
          alpha = 0.01) +
geom_line(aes(x = X_step_, y = hyp2, #raw curves, hyp2!
             group = X_run_number_),
          alpha = 0.01) +
          ylab("Researchers per hypothesis") +
          xlab("Study round") +
          ylim(c(0, 100)) +
geom_line(data = rep_means_long[X_step_ <= 50,], #means
        aes(x = X_step_, y = value, color = Effect_size)) +
        facet_wrap(~p_hacking_success) +
        theme_bw()

 ggsave("figures/p-hacking-all-biased.png", width = 1.6*6, height = 6)
