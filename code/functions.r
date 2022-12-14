# Project: Ghost fields
# Code purpose: Functions to run simulation
# Code: Anton Olsson-Collentine

#************************************************



collect_evidence <- function(researcher_data){
    new_sig <- aggregate(researcher_data$result,
                    list(researcher_data$hypothesis),
                    FUN = sum,
                    drop = FALSE) #'group.1" = hypotheses, x = sum results
    new_sig[is.na(new_sig)] <- 0 #drop = FALSE gives NAs

    number_published_studies <- aggregate(researcher_data$published,
                    list(researcher_data$hypothesis),
                    FUN = sum,
                    drop = FALSE) #keeps a hyp even if no researchers for it
    number_published_studies[is.na(number_published_studies)] <- 0 #drop = FALSE gives NAs

    new_non_sig <- number_published_studies #same formatting for consistency
    new_non_sig$x <-number_published_studies$x - new_sig$x

    #out
    out <- list(new_sig = new_sig,
                new_non_sig = new_non_sig,
                number_published_studies = number_published_studies)
    colnames <- c("hypothesis", "num_studies")
    lapply(out, setNames, colnames) #returns

}
    # #data.table version
    # researcher_dat <- as.data.table(researcher_dat)
    # researcher_dat[, .(num_sig = sum(result)), by = hypothesis]

update_evidence <- function(topic_info, new_evidence){

        topic_info$positive <- topic_info$positive + new_evidence$new_sig$num_studies

        topic_info$negative <- topic_info$negative + new_evidence$new_non_sig$num_studies

        topic_info$prop <- ifelse(topic_info$positive + topic_info$negative == 0, #can be zero if everyone who runs a study is affected by publication bias
        0,
        topic_info$positive / (topic_info$positive + topic_info$negative))

        #out
        topic_info
}

compute_normalized_probs <- function(topic_info, num_hyps){

    topic_selection_probs <- vector("list", length = num_hyps)

    for (h in 1:num_hyps){
         #computes normalized probability to select one of the *other* hypotheses
        total <- sum(topic_info[-h,"prop"]) #total prop sig of non-focus hyp
        normalized_probs <- topic_info[-h,"prop"] / total

        #cleanup
        names(normalized_probs) <- topic_info[-h, "hyp"]
        topic_selection_probs[[h]] <- normalized_probs

    }

    #out
    topic_selection_probs
}


update_hyp <- function(researcher_ID, researcher_data, topic_selection_probs){
        #Get the probabilities to select one of the different hypotheses
        hyp <- researcher_data[researcher_ID, "hypothesis"]
        new_p <- topic_selection_probs[[hyp]]

        #Selecte new hypotheses amongst other hypothesis based on their prob
        new_h <- sample(names(new_p), 1, prob = new_p)

        #out
        as.numeric(new_h)
}
