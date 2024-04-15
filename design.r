library(idefix)
library(dplyr)
library(support.CEs)
library(ExpertChoice)
library(DoE.base)
library(openxlsx)

rm(list = ls())
set.seed(12012023)

at.lvls <- c(3, 3, 3, 4)

### attribute levels
# 1: price; 10, 15, 20
# 3: location: global, EU, UK
# 3: certification; None, NGO, UK
# 4: project: forest, renewable, storage, appliances

################################################################################
### Price as continuous variable
################################################################################

c.type <- c("C", "D", "D", "D")
cs <- Profiles(
    lvls = at.lvls,
    coding = c.type,
    c.lvls = list(c(10, 15, 20))
)

mu0 <- 0
sigma0 <- diag(length(mu0))
M0 <- MASS::mvrnorm(n = 1000, mu = mu0, Sigma = sigma0)

# mu1 <- c(-1, 5, 5, 5, 5, 0, -2, -2)
mu1 <- rep(0, 8)
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

save.image("./design/design_continuous_price.RData")
# load("design.RData")

################################################################################
### Price as categorical variable
################################################################################

c.type <- c("D", "D", "D", "D")
cs <- Profiles(
    lvls = at.lvls,
    coding = c.type
)

mu0 <- 0
sigma0 <- diag(length(mu0))
M0 <- MASS::mvrnorm(n = 1000, mu = mu0, Sigma = sigma0)

# mu1 <- c(-1, 5, 5, 5, 5, 0, -2, -2)
mu1 <- rep(0, 9)
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

save.image("./design/design_discrete_price.RData")

design <-
    data.frame(
        I = D$design[, 1],
        price = 10 * (D$design[, 2] == 0 & D$design[, 3] == 0 & D$design[, 1] != 1) + 
            15 * (D$design[, 2] == 1 & D$design[, 1] == 0) +
            20 * (D$design[, 3] == 1 & D$design[, 1] == 0),
        location_EU = D$design[, 4],
        location_UK = D$design[, 5],
        certificate_NGO = D$design[, 6],
        certificate_UK = D$design[, 7],
        project_renewable = D$design[, 8],
        project_storage = D$design[, 9],
        project_appliances = D$design[, 10]
    )

write.csv(design, "./csv/design.csv", row.names = FALSE)

################################################################################
### convert to choice experiment scenarios
################################################################################

design <- read.csv("./csv/design.csv")

design <- 
    design %>%
    mutate(
        location = case_when(
            location_EU == 1 & location_UK == 0 ~ "EU",
            location_EU == 0 & location_UK == 1 ~ "UK",
            location_EU == 0 & location_UK == 0 ~ "Global"
        ),
        certificate = case_when(
            certificate_NGO == 1 & certificate_UK == 0 ~ "NGO",
            certificate_NGO == 0 & certificate_UK == 1 ~ "UK",
            certificate_NGO == 0 & certificate_UK == 0 ~ "None"
        ),
        project = case_when(
            project_renewable == 1 ~ "Renewable Energy",
            project_storage == 1 ~ "Carbon Storage",
            project_appliances == 1 ~ "Energy Efficient Home Appliances",
            project_renewable == 0 & project_storage == 0 & project_appliances == 0 ~ "Planting Trees"
        )
    )

# write to excel
write.xlsx(design, "./csv/choice_scenarios.xlsx")