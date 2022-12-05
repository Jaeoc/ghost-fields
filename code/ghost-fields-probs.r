




p_stay <- c(no = .4, yes = .6)
p_newh <- c(.35, .55, .1) #

# COnditionals
p_newh_0 <- c(.35, .55, .1) #
p_newh_1 <- c(.3, .6, .1) #
store <- NULL

for(i in 1:1e5){
    stay <- sample(c(0,1), 1, prob = p_stay)
    if(stay == 0){
        newh <- sample(c(0, 1, 2), 1, prob = p_newh_2)
    }
    if (stay == 1) {
        newh <- sample(c(0, 1, 2), 1, prob = p_newh_1)
    }
    store[i] <- newh
}

table(store)/1e5

# 1 cycle: what happens? -------------------------------------------------------

n <- 1e5

# Closed form version of number of people in each topic
n * p_stay["no"] * p_newh

# Simulation version
store <- rep("stay", n)
for (i in 1:n) {
    stay <- sample(c(0, 1), 1, prob = p_stay)
    if (stay == 0) {
        store[i] <- sample(c("h1", "h2", "h3"), 1, prob = p_newh)
    }
}

cbind(
    table(store)[-4],
    n * p_stay["no"] * p_newh
) # Closed form version of number of people in each topic

# Cycles ----------------------------------------------------------------------

n <- 1e5
nc <- 5
p_stay <- c(no = .4, yes = .6)
p_newh <- c(.35, .55, .1) #
store <- matrix("stay", nrow = n, ncol = nc)

# Closed form version of number of people in each topic
n * p_stay["no"] * p_newh

# Simulation version
for(r in 1:nc){
    for (i in 1:n) {
        stay <- sample(c(0, 1), 1, prob = p_stay)
        if (stay == 0) {
            store[i, r] <- sample(c("h1", "h2", "h3"), 1, prob = p_newh)
        }
    }
}

data.frame(store)

store[, ]

sum(rowSums(store  == "stay") == nc) / n

sum(rowSums(store != "stay") > 3) / n

colMeans(store == "h1")

mean(colMeans(store == "h1"))

mean(store == "h3")

table(store)

n <- 1e4
nc <- 5

# Closed form probability of each hp
p_stay["no"] * p_newh
#Anton: gives us the proportion out of the total that
#will switch to each topic
# If we take the remaining proportion (i.e., p_stay["yes"])
#and multiply by 1/3 we get the proportion people that will stay in
#each topic. If we add this the the proportion switchers
#we end up with the total proportion for each topic.
p_stay["yes"] *n

# Number of
n * nc * (n * p_stay["no"] * p_newh) / n


# conditional Cycles ----------------------------------------------------------------------
# COnditionals
p_0_sig <- c(0.9, 0.1) #
p_1_sig <- c(0.5, .5)
p_2_sig <- c(0.1, 0.9)

n <- 1e5
nc <- 5

store <- matrix("stay", nrow = n, ncol = nc)

# Simulation version
for(r in 1:nc){
    for (i in 1:n) {
        n_sig <- sample(0:2, 1)
        if(n_sig == 0){
        p_stay <-p_0_sig
        }
        if(n_sig == 1){
        p_stay <-p_1_sig
        }
        if(n_sig == 2){
        p_stay <-p_2_sig
        }

        stay <- sample(c(0, 1), 1, prob = p_stay)
        if (stay == 0) {
            store[i, r] <- sample(c("h1", "h2", "h3"), 1, prob = p_newh)
        }
    }
}

table(store)
#Prob to get zero sig and switch
1/3 * p_0_sig[1] #just get number of sig based on chance (i.e., 1/3)
#Prob to get 1 sig and switch
1/3 * p_1_sig[1]
#Prob to get 2 sig and switch
1/3 * p_2_sig[1]

#Proportion participant that will get "no" and switch for each number of sig
q <- 1/3 * c(zero = p_0_sig[1], one = p_1_sig[1], two = p_2_sig[1])

#Prob to switch and select each of the hyps depending on num sig
#Corresponds to p_stay["no"] *p_newh
dat <- matrix(q * rep(p_newh, each = 3), ncol = 3, byrow = TRUE)
colnames(dat) <- c("zero", "one", "two")
rownames(dat) <- c("h1", "h2", "h3")

dat
#proportion of total that will switch and select each of the hyps
rowSums(dat)

#compare with simulation results
table(store)
rowSums(dat) * n *nc #matches!

#add proportion that will not switch to each
stay  <- 1 - sum(q)

stay * 1/3
#= proportion that stays on each topic

#totals proportion of participants on each topic
rowSums(dat) + stay * 1/3

sum(.Last.value) #sums to 1 as it should

#-------------
#Summary
# Expected proportion on each topic is
#1) multiply the probability of getting X significant results
# with the probability of switching given X sig results
# 2) Multiply each with the probability of selecting each topic
# 3) sum together the proportions for each newly selected topic
# This is the proportion/total that will a) switch and b) select each topic
#4) Add the non-switchers. (1 - q) * distribution gives us the
#proportion/total that stays on each topic
#5) add 3 + 4 to get the total proportion on each topic

#Note: The bayesian formula gives us the probability of getting
#X significant results given a certain power.
#For vote-counters, we then just multiply this by their vote-counting probabilities

# For publication bias: this only affects the normalized probabilities
# Proportion non-sig should be:
# 1 - (1 - beta) * pub.bias (= prob that a non-sig. gets published)
# = beta * pub.bias (= prob that a non-significant makes it into literature)
# proportion significant is then 1 - (beta *pub.bias)

#---------------------------
# Calculate the expected values under common scenarios
#----------------------------

#Prob of 0 - 5 (non)sig is a binomial distribution
n  <- 5
k  <- 0:5
power = c(0.05, 0.30, 0.50, 0.80, 0.95, 1)
beta <- 1 - power #defines success as non-sig

calc_likelihood <- function(beta){
    dbinom(x = k, size = n, prob = beta)
}
a <- lapply(beta, calc_likelihood)
b <- do.call(rbind, a)
row.names(b) <- power
colnames(b) <- c("zero non-sig",
                 "one non-sig",
                 "two non-sig",
                 "three non-sig",
                 "four non-sig",
                 "five non-sig")
#------------
#Prob to find X non-sig based on Y power
b

#b times the vote-counting probabilities gives the
#prob that any vote-counting will leave (prob of X non-sig times corresponding prob)
#vote_counting <- c(1, 0.95, 0.8, 0.5, 0.2, 0) #= prob to stay
vote_counting <- c(0, 0.05, 0.2, 0.5, 0.8, 1) #= prob to leave

q <- apply(b, 1, function(x) x * vote_counting)
q <- t(q) #= prob that people will leave

#Prob to select each topic
h_power <- c(h0 = 0.05,
             h1 = 0.3,
             h2 = 0.8) #given 3 hyps with this power
p_newh <- h_power / sum(h_power)

# Prob to achieve X non-sig and select topic H0-H2
#for each level of power (topic)
q_selected <- q[c(1, 2, 4),]

dat <- vector("list", length = 3)
for (r in 1:nrow(q_selected)){
  dat[[r]]  <-  matrix(q_selected[r,] * rep(p_newh, each = 6), ncol = 6, byrow = TRUE)
colnames(dat[[r]]) <- c("zero", "one", "two", "three", "four", "five")
rownames(dat[[r]]) <- c("h0", "h1", "h2")
}
names(dat) <-c("power_0.05", "power_0.3", "power_0.8")

# Sum the proportions that will select each topic
h_select <- lapply(dat, rowSums)
h_select <- do.call(rbind, h_select)

#h_select = proportion of people studying each hypothesis that will switch
#and what they will switch to

#Proportion of total that will switch and select each hyp, across hyps
#1/3 of participants studying each hyp so
h_select_tot <- h_select*1/3
h_select_tot <- colSums(h_select_tot)
h_select_tot

#4) Proportion staying
stay  <- 1 - rowSums(q_selected) # proportion within each hyp staying
stay <- stay *(1/3) #1/3 on each hyp, proportion of total staying
#stay_tot <- sum(stay) #proportion of total that stays
#stay_tot <- 1 - sum(h_select_tot) #equivalent

#5)
#total proportion of participants on each topic
h_select_tot + stay
sum(.Last.value) #sums to 1, so seems like should be correct

#unfortunately, does not match my simulations..


#------------------------------
#For bayesian researchers


calc_rational <- function(power, beta){
numerator <- power ^ k * beta ^(n -k)
denom <- numerator + alpha^k * (1 -alpha)^(n-k)
numerator / denom

}


n <- 5
k <- 0:5
power = c(0.05, 0.30, 0.50, 0.80, 0.95, 1)
beta = 1 - power
alpha = 0.05

a <- Map(calc_rational, power, beta)
b <- do.call(rbind, a)
#b is a table of correct probabilities for 5 studies with
#0 - 5 significant results. Prob of true effect.
row.names(b) <- power
colnames(b) <- c("zero sig",
                 "one sig",
                 "two sig",
                 "three sig",
                 "four sig",
                 "five sig")

#Prob of H given D
b
