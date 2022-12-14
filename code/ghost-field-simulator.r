# Project: ghost fields
# Code purpose: Translate netlogo code into R
# Author: Anton Olsson Collentine (anton@olssoncollentine.com)

#****************************************
# General structure

#1) dataframe with researcher characteristics
# ID, hypothesis, result, continue, published
# 2) dataframe with memory for researchers
# ID, newest, second, third, fourth, fifth

#3) dataframe with topic characteristics
# topic_ID, ES, positive, negative, prop

source("./code/functions.r")
# *********************************************************
#Setup: parameter values
# *********************************************************
replications <- 50
ticks <- 50 #number of ticks
num_researchers <- 100 #number of researchers
num_hyps <- 3
length_memory  <- 5
journal_publication_bias <- 0
file_drawer_effect <- 0
vote_counter <- c(1, 0.95, 0.8, 0.5, 0.2, 0) #memory-probs
memory_curve <- vote_counter

effect_sizes <- c(hyp_1 = 0.05, #null effect size
                hyp_2 = 0.3,
                hyp_3 = 0.8)


# *********************************************************
# Setup: creating researchers and topics
# *********************************************************

researcher_dat <- data.frame(ID = 1:num_researchers,
                             hypothesis = factor((1:num_researchers %% num_hyps) + 1), #factor so that we don't drop levels in "update_evidence"
                             result = 1,
                             continue = 1,
                             published = 1)

#%% is modulo. Above we assign each researcher to a hypothesis with as equal amounts as possible for each hyp (100/3 cannot be completely equal)
#I add +1 to make it easier to use 1:num_hyps in other places

memory_dat <- matrix(1, #start with positive values everywhere
                    nrow = num_researchers,
                    ncol = length_memory)
memory_dat <- as.data.frame(memory_dat)

topic_dat <- data.frame(hyp = 1:num_hyps,
                        ES = effect_sizes,
                        positive = 0,
                        negative = 0,
                        prop = 0)

store <- matrix(nrow = num_researchers, ncol = ticks + 1)
#+1 column because first column is starting values
store[,1] <- researcher_dat[, "hypothesis"]

empty <-matrix(0:50, nrow = (1+ticks), ncol = 1+ num_hyps)
colnames(empty) <- c("run", paste0("hyp", seq_len(num_hyps)))
store_summary <- vector("list",
                    length = replications)
store_summary <- lapply(store_summary, function(x) x  <- empty)
# *********************************************************
# Running the system
# *********************************************************
options(warn=2)

for(reps in 1:replications){

# to go
for(t in 1:ticks){ #for each tick

    for (i in 1:num_researchers) { #for each researcher

        #Run a study
        hyp <- researcher_dat[i,"hypothesis"]
        prob_of_sig <- topic_dat[hyp, "ES"]
        researcher_dat[i,"result"] <- rbinom(1, 1, prob_of_sig)

        #Update memory (drop oldest study, add newest result)
        memory_dat[i,] <- cbind(memory_dat[i,-1],researcher_dat[i,"result"])

        #File drawer?
        # Researchers always attempt to publish sign, non-sig are submitted to publication with probability equal to file_drawer_effect
        researcher_dat[i,"published"] <- ifelse(researcher_dat[i,"result"] == 1,
                                        1,
                                        rbinom(1, 1, 1 - file_drawer_effect))
        # note that prob  is 1 - file_drawer_effect

        #Try to publish
        # significant are always published, non-sig are published with probability equal to journal_publication bias
        researcher_dat[i,"published"] <- ifelse(researcher_dat[i,"result"] == 1,
                                        1,
                                        rbinom(1, 1, 1 - journal_publication_bias))

        #study same topic?
        n_non_sig <- length_memory - sum(memory_dat[i,])

        if(n_non_sig == 0){
            p_continue <-memory_curve[1]
        }else if(n_non_sig == 1){
            p_continue <-memory_curve[2]
        }else if(n_non_sig == 2){
            p_continue <-memory_curve[3]
        }else if(n_non_sig == 3){
            p_continue <-memory_curve[4]
        }else if(n_non_sig == 4){
            p_continue <-memory_curve[5]
        }else if(n_non_sig == 5){
            p_continue <-memory_curve[6]
        }

        researcher_dat[i, "continue"] <- rbinom(1, 1, prob = p_continue)
    } #end of new data collection and decisions

    #Update evidence for each topic

    new_evidence <- collect_evidence(researcher_dat)
    topic_dat <- update_evidence(topic_dat, new_evidence)

    #Update topic for researchers with continue = 0
    topic_selection_probs <- compute_normalized_probs(topic_dat, num_hyps)

    for(i in 1:num_researchers){
        if(researcher_dat[i, "continue"] == 1) {
            next
            } #do nothing if researcher has continue = 1
         #else if continue = 0 update hyp

        researcher_dat[i, "hypothesis"] <- update_hyp(i,
                                              researcher_dat,
                                              topic_selection_probs)

        #reset memory for these researchers to zero negative results
        memory_dat[i,] <- 1

    }

store[,t+1] <- researcher_dat[, "hypothesis"] #t+1 because first column is starting values

#this below is probably more efficient to have in the outer loop,, but can't figure out how to do it..
if(num_hyps != 3) {stop("store_summary is hardcoded for 3 hyps!")}
summary_res <- as.data.frame(table(store[,t+1]))
hyp1_res <- summary_res[summary_res$Var1 == 1, "Freq"]
hyp2_res <- summary_res[summary_res$Var1 == 2, "Freq"]
hyp3_res <- summary_res[summary_res$Var1 == 3, "Freq"]

store_summary[[reps]][t+1, "hyp1"] <-ifelse(length(hyp1_res) == 0, 0, hyp1_res)
store_summary[[reps]][t+1, "hyp2"] <-ifelse(length(hyp2_res) == 0, 0, hyp2_res)
store_summary[[reps]][t+1, "hyp3"] <-ifelse(length(hyp3_res) == 0, 0, hyp3_res)

#tick
cat(paste0(t, "\n"))
}
cat(paste0("# run", reps+1, "\n"))
}

#Surprisingly annoying... Below is not complete
one <- apply(store, 2, function(hyp) as.data.frame(table(hyp)))
two <- dplyr::bind_rows(one, .id = "tick")
two$tick  <- two$tick -1

#
write.csv(store_summary, "data/test_data_r_code.csv")
