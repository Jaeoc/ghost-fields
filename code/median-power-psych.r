#Estimate median power in psychology

#median sample size per group: 24
#Median effect size: r = 0.16
#equal to d = 0.32 according to https://www.escal.site/

reps <- 1e4
res <- vector(length = reps)

for (r in 1:reps){
n = 24
a <- data.frame(control =  rnorm(n, mean = 0, sd = 1), treat = rnorm(n, mean = 0.32, sd = 1))

res[r] <- t.test(a)$p.value
}
mean(res)
