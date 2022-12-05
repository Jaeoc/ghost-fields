# Project: Ghost fields
# Script purpose: checking rational equation


#olmo's example
power = 0.5
beta = 1- power
alpha = 0.05
n = 4
k = 2

numerator <- power ^ k * beta ^(n -k)
denom <- numerator + alpha^k * (1 -alpha)^(n-k)

p <- numerator / denom

k = 0:4
#Gives correct result

#example 2
power = 0.8
beta = 1- power
alpha = 0.05
n = 4
k = 0:4

#Matches the curves in Figure 1


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
#I will use this table for comparison to see that my netlogo is producing the
#correct results

#EDIT: step by step, the netlogo code seems to produce the correct result
# So it is not the implementation of the equation itself that is strange
