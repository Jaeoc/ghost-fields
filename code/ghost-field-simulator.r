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


# *********************************************************
#Setup: parameter values
# *********************************************************

ticks <- 200 #number of ticks
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

# *********************************************************
# Running the system
# *********************************************************
options(warn=2)
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
    #base version
    new_sig <- aggregate(researcher_dat$result,
                    list(researcher_dat$hypothesis),
                    FUN = sum,
                    drop = FALSE) #'group.1" = hypotheses, x = sum results
    new_sig[is.na(new_sig)] <- 0 #drop = FALSE gives NAs
    number_published_studies <- aggregate(researcher_dat$published,
                    list(researcher_dat$hypothesis),
                    FUN = sum,
                    drop = FALSE) #keeps a hyp even if no researchers for it
    number_published_studies[is.na(number_published_studies)] <- 0 #drop = FALSE gives NAs

    new_non_sig <- number_published_studies #same formatting for consistency
    new_non_sig$x <-number_published_studies$x - new_sig$x

    # #data.table version
    # researcher_dat <- as.data.table(researcher_dat)
    # researcher_dat[, .(num_sig = sum(result)), by = hypothesis]

    topic_dat$positive <- topic_dat$positive + new_sig$x

    topic_dat$negative <- topic_dat$negative + new_non_sig$x
    topic_dat$prop <- ifelse(topic_dat$positive + topic_dat$negative == 0, #can be zero if everyone who runs a study is affected by publication bias
                             0,
                             topic_dat$positive / (topic_dat$positive + topic_dat$negative))

    #Update topic for researchers with continue = 0

    #First get probabilities for each new topic
    topic_selection_probs <- vector("list", length = num_hyps)
    for (h in 1:num_hyps){
        total <- sum(topic_dat[-h,"prop"])
        normalized_probs <- topic_dat[-h,"prop"] / total
        names(normalized_probs) <- topic_dat[-h, "hyp"]
        topic_selection_probs[[h]] <- normalized_probs
    }


    #next update topic for researcher with continue = 0
    for(i in 1:num_researchers){
        if(researcher_dat[i, "continue"] == 1) {
            next
            } #do nothing if researcher has continue = 1
         #if continue = 0

        hyp <- researcher_dat[i, "hypothesis"]
        new_p <- topic_selection_probs[[hyp]]
        new_h <- sample(names(new_p), 1, prob = new_p)
        researcher_dat[i, "hypothesis"] <- as.numeric(new_h)

        #reset memory for researchers with new topic to zero negative results
        memory_dat[i,] <- 1

    }

store[,t+1] <- researcher_dat[, "hypothesis"] #t+1 because first column is starting values

#tick
print(t)
}
