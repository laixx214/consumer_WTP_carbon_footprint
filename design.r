library(idefix)
rm(list = ls())

set.seed(1201202)
at.lvls <- c(4, 3, 3, 4)

### attribute levels
# 1: price; 9, 12, 15, 19
# 3: location: global, EU, UK
# 3: certification; None, NGO, UK
# 4: project: forest, renewable, storage, appliances

c.type <- c("C", "D", "D", "D")
cs <- Profiles(
    lvls = at.lvls,
    coding = c.type,
    c.lvls = list(c(9, 12, 15, 19))
)

mu0 <- -1
sigma0 <- diag(length(mu0))
M0 <- MASS::mvrnorm(n = 1000, mu = mu0, Sigma = sigma0)

mu1 <- c(-0.1, 0.1, 0.2, 0.1, 0.2, -0.1, -0.3, -0.2)
sigma1 <- diag(length(mu1))
M1 <- MASS::mvrnorm(n = 1000, mu = mu1, Sigma = sigma1)

D <- Modfed(
    cand.set = cs,
    n.sets = 10,
    n.alts = 3,
    alt.cte = c(0, 0, 1),
    par.draws = list(M0, M1),
    no.choice = TRUE
)
D

save.image("design.RData")
