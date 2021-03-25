library(data.table)

d <- data.table(
  a0 = c(rep(0, 4), rep(1, 4)),
  z1 = c(0, 0, 1, 1, 0, 0, 1, 1),
  a1 = c(0, 1, 0, 1, 0, 1, 0, 1),
  n  = c(816733, 22062, 69386, 2280, 46814, 100017, 6003, 17321)
)

pr_a0_1 <- d[a0 == 1, sum(n)] / d[, sum(n)]
pr_a1_1 <- d[a1 == 1, sum(n)] / d[, sum(n)]

cond_prob <- function(dt, z, a) {
  
  dt[a1 == 1 & z1 == z & a0 == a, sum(n)] / dt[z1 == z & a0 == a, sum(n)]
  
}

c00 <- cond_prob(d, z = 0, a = 0)
c10 <- cond_prob(d, z = 1, a = 0)
c01 <- cond_prob(d, z = 0, a = 1)
c11 <- cond_prob(d, z = 1, a = 1)


d[a0 == 0 & a1 == 0 & z1 == 0, sw := ((1 - pr_a0_1)*(1- pr_a1_1)) / ((1 - c00) * (1 - pr_a0_1))]
d[a0 == 0 & a1 == 0 & z1 == 1, sw := ((1 - pr_a0_1)*(1- pr_a1_1)) / ((1 - c10) * (1 - pr_a0_1))]

d[a0 == 1 & a1 == 0 & z1 == 0, sw := (pr_a0_1*(1- pr_a1_1)) / ((1 - c01) * pr_a0_1)]
d[a0 == 1 & a1 == 0 & z1 == 1, sw := (pr_a0_1*(1- pr_a1_1)) / ((1 - c11) * pr_a0_1)]

d[a0 == 0 & a1 == 1 & z1 == 0, sw := (pr_a1_1*(1- pr_a0_1)) / (c00 * (1 - pr_a0_1))]
d[a0 == 0 & a1 == 1 & z1 == 1, sw := (pr_a1_1*(1- pr_a0_1)) / (c10 * (1 - pr_a0_1))]
  
d[a0 == 1 & a1 == 1 & z1 == 0, sw := (pr_a1_1*pr_a0_1) / (c01 * pr_a0_1)]
d[a0 == 1 & a1 == 1 & z1 == 1, sw := (pr_a1_1*pr_a0_1) / (c11 * pr_a0_1)]
  
d