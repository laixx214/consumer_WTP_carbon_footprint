library(data.table)
library(dplyr)
library(tidyverse)
library(list)

rm(list = ls())
set.seed(1201202)

################################################################################
### Read in data
################################################################################
load("./image/sensitive_statements.RData")
dt_count <- fread("csv/dt_counts.csv")

################################################################################
### run list experiment
################################################################################
est_intercept <-
    lapply(
        1:4,
        function(x) {
            ictreg(
                count ~ 1,
                data = dt_count %>%
                    filter(
                        list_id == x
                    ),
                treat = "treatment",
                J = 4,
                method = "lm",
                constrained = TRUE
            )
        }
    )

### assemble parameter and se into a table
tbl_intercept <-
    lapply(
        est_intercept,
        function(x) {
            data.frame(
                intercept = unlist(x$par.treat),
                se = unlist(x$se.treat)
            )
        }
    ) %>%
    bind_rows() %>%
    mutate(
        control_list = unlist(lapply(1:4, function(x) rep(paste0("Control_list_", x), 4))),
        sensitive_statement = rep(sensitive_statements, 4)
    ) %>%
    ### order control ist and sensitive statement before estimates
    select(
        control_list,
        sensitive_statement,
        intercept,
        se
    )

### add overall estimates
tbl_intercept_overall <-
    tbl_intercept %>%
    group_by(
        sensitive_statement
    ) %>%
    summarise(
        intercept = mean(intercept),
        se = sqrt(mean(se^2))
    ) %>%
    mutate(
        control_list = "Overall"
    )

### merge overall estimates with list estimates
tbl_intercept <-
    bind_rows(
        tbl_intercept,
        tbl_intercept_overall
    ) %>%
    ### delete \n
    mutate(
        sensitive_statement = gsub("\n", " ", sensitive_statement)
    )
    

est <-
    ictreg(
        count ~ as.factor(list_id) - 1,
        data = dt_count,
        treat = "treatment",
        J = 4,
        method = "lm",
        constrained = TRUE
    )