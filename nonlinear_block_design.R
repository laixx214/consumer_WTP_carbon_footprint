library("data.table")
library("tictoc")
library("doParallel")
library("foreach")
library("parallel")
library("matrixcalc")
library("Matrix")
library("numDeriv")
library("acebayes")
library("AlgDesign")
library("dplyr")
library("Matrix")
library("rbenchmark")

rm(list = ls())
setwd("./")
set.seed(1234)

####################################
### Define experiment
####################################

n_cs <- 8
blocks <- 1
n_sample <- 1000
n_alt <- 2
opt_out <- TRUE
price <- c(9, 10, 16, 19)
location <- c(1, 2, 3)
certification <- c(1, 2, 3)
projects <- c(1, 2, 3, 4)

### generate initial design
full_fact <- expand.grid(price, location, certification, projects)
names(full_fact) <- c("price", "location", "certification", "projects")
full_fact$location <- as.factor(full_fact$location)
full_fact$certification <- as.factor(full_fact$certification)
full_fact$projects <- as.factor(full_fact$projects)
full_fact <- model.matrix(~., data = full_fact)
colnames(full_fact)[1] <- "I"

b <- c(
  I = 1,
  price = -0.1,
  location2 = 0,
  location3 = 0,
  certification2 = 0,
  certification3 = 0,
  projects2 = 0,
  projects3 = 0,
  projects4 = 0
)

names <- colnames(full_fact)
k <- length(names) # number of parameters (including intercept)

####################################
### Define information matrix
####################################

eval_zpz <- function(d, b) { # evaluate variance matrix of 1 conventional CE design

  choice_sets <- d$choice_sets
  d <- d[, c(names), with = F]
  l <- ncol(d)
  ooo <- t(rep(1, l))
  if (opt_out) {
    ooo1 <- rep(1, n_alt + 1)
  } else {
    ooo1 <- rep(1, n_alt)
  }
  beta <- b

  zpz <- lapply(unique(choice_sets), function(x) {
    exp_xb <- exp(as.matrix(d[choice_sets == x]) %*% beta)
    prob_temp <- exp_xb / (sum(exp_xb))
    d_temp <- as.matrix(d[choice_sets == x, ])

    z <- (d_temp - (ooo1 %*% (t(prob_temp) %*% d_temp)))
    pz <- (prob_temp %*% ooo) * z
    zpz <- t(z) %*% pz
    return(zpz)
  })

  zpz <- Reduce("+", zpz)
  return(zpz)
}

eval_D <- function(d, b) { # evaluate D efficiency of 1 best-worst CE design
  zpz <- eval_zpz(d, b) + eval_zpz(d, -b)

  # omega <- solve(zpz)
  # D <- ((det(omega)^(1/k)))^(-1)

  D <- det(zpz)^(1 / k)
  return(D)
}

eval_Dp <- function(DE, b) { # evaluate Dp efficiency of b blocks
  Dp <- lapply(DE, function(d) {
    dp <- eval_D(d, b)
    return(log(dp))
  })
  Dp <- Reduce("+", Dp)
  return(Dp)
}

get_design <- function(cs) {
  DE <- lapply(cs, function(x) {
    return(full_set[choice_sets %in% x, ])
  })
  return(DE)
}

####################################
### Drop dominant scenarios
####################################

get_full_set <-
  function(full_fact,
           b,
           n_alt,
           add_opt_out = opt_out) {
    index <- combn(1:nrow(full_fact), n_alt)
    valid_cs <- t(index)

    # get full set of valid scenarios
    full_set <- lapply(
      1:nrow(valid_cs),
      function(x) {
        r <- valid_cs[x, 1:n_alt]
        a <- full_fact[r, ]

        if (add_opt_out) {
          a <- rbind(a, rep(0, ncol(a)))
        }

        return(a)
      }
    )
    full_set <- do.call("rbind", full_set)

    # generates scenarios ids
    choice_sets <- lapply(
      1:dim(valid_cs)[1],
      function(x) {
        if (add_opt_out) {
          rep(x, n_alt + 1)
        } else {
          rep(x, n_alt)
        }
      }
    )

    choice_sets <- unlist(choice_sets)
    full_set <- data.table(
      cbind(
        full_set,
        choice_sets
      )
    )

    return(full_set)
  }

full_set <-
  get_full_set(
    full_fact,
    b,
    n_alt
  )
####################################
### run Fedorov
####################################

get_DE0 <- function(full_set, n_blocks = 1) {
  blocks <- rep(list(NA), n_blocks)
  for (i in 1:n_blocks) {
    blocks[[i]] <- sample(
      setdiff(
        unique(full_set$choice_sets),
        unlist(blocks)
      ),
      n_cs,
      replace = F
    )
  }
  return(blocks)
}
cs <- get_DE0(full_set)
DE <- get_design(cs)

CS_list <- list(NA)
D_list <- c(NA)

# benchmark(eval_Dp(DE,b),
#           eval_zpz(DE[[1]],b),
#           DE <- get_design(cs),
#           replications = 1000)

# benchmark(DE <- get_design(cs),
#           D <- eval_Dp(DE,b),
#           replications = 1000)

D <- eval_Dp(DE, b)
D_ <- -1000
a <- 1

while (D - D_ >1e-6 & a < 5000) {

  a <- a+1
  D_ <- D
  
  tic()
  for (i in 1:length(cs)) {
    for (j in 1:length(cs[[i]])) {

      # tic() #timer start
      candidates <- setdiff(unique(full_set$choice_sets),
                            unlist(cs))
      
      D_temp <- mclapply(
        candidates,
        function(x) {
          library(data.table)

          D_temp <- tryCatch(
            {
              cs[[i]][j] <- x
              DE <- get_design(cs)
              D <- eval_Dp(DE, b)
              return(D)
            },
            error = function(x) {
              return(-10000)
            }
          )

          return(D_temp)
        },
        mc.cores = detectCores()
      )

      D_temp <- unlist(D_temp)
      cs[[i]][j] <- candidates[which.max(D_temp)]
      # toc() #timer end
    }
  }
  toc()

  DE <- get_design(cs)
  D <- eval_Dp(DE,b)

  print(cs)
  print(a)

  CS_list[[a]] <- cs
  D_list[[a]] <- D

  save.image(file = "design.RData")
}

load("G:/Google Drive/working paper/farm animal study with Albert/design/design.RData")
DE_final <- get_design(CS_list[[length(CS_list) - 1]])

# save.image(file = "design.RData")
