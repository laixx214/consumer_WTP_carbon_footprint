library(data.table)
library(dplyr)
library(tidyverse)
library(list)
library(car)
library(boot)
library(openxlsx)

rm(list = ls())
set.seed(1201202)

date <- gsub("-", "", as.character(Sys.Date()))
### sink message
my_log <- file(paste("run_list", date, ".log", sep = ""))
sink(my_log, append = TRUE, type = "output")
sink(my_log, append = TRUE, type = "message")

################################################################################
### Read in data
################################################################################
message("Reading in data...")
load("./image/sensitive_statements.RData")
dt_count <- fread("csv/dt_counts.csv")

### treatment reference
statement_reference <-
    dt_count %>%
    select(
        treatment,
        statement
    ) %>%
    distinct() %>%
    arrange(treatment)
    
### number of control lists
n_cl <- dt_count %>%
    select(list_id) %>%
    distinct() %>%
    nrow()

### n control statements
n_cs <- 4

### number of sensitive statements
n_ss <- dt_count %>%
    select(statement) %>%
    distinct() %>%
    filter(statement != "") %>%
    nrow()

################################################################################
### run list experiment, intercept only
################################################################################
message("Running list experiment, intercept only...")
ict_fit_intercept <-
    ictreg(
        count ~ as.factor(list_id) - 1,
        data = dt_count,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )

### make table
tbl_intercept <-
    lapply(
        1:n_ss,
        function(i) {
            par <- ict_fit_intercept$par.treat[[i]]
            se <- ict_fit_intercept$se.treat[[i]]
            control <- paste0("Control List ", 1:4)
            
            names(par) <- NULL
            names(se) <- NULL
            names(control) <- NULL

            data.frame(
                control = control,
                coefficient = par,
                SE = se
            ) %>%
                mutate(
                    treatment = i
                )
        }
    )
    
tbl_intercept <-
    do.call(rbind, tbl_intercept) %>%
    as.data.frame() %>%
    left_join(
        statement_reference
    ) %>%
    mutate(
        Prob. = inv.logit(coefficient)
    ) %>%
    select(
        statement,
        control,
        Prob.,
        coefficient,
        SE
    )

### use delta method to estimate the average treatment effect across control lists
# construct formulas
delta_f <- 
    lapply(
        1:n_ss,
        function(i) {
            formula <-
                paste0(
                    "`sensitive.",
                    i,
                    ".as.factor(list_id)",
                    1:n_cl,
                    "`"
                )
            formula <- paste0(formula, collapse = " + ")
            formula <- paste0("(", formula, ") / ", n_cl)
            return(formula)
        }
    )

# estimate average treatment effect
tbl_ate <-
    lapply(
        1:length(delta_f),
        function(i) {
            statement <-
                statement_reference$statement[
                    statement_reference$treatment == i
                ]

            f <- delta_f[[i]]

            ate_est <-
                deltaMethod(
                    ict_fit_intercept,
                    f
                )

            return(
                data.frame(
                    statement = statement,
                    control = "Average",
                    coefficient = ate_est$Estimate,
                    SE = ate_est$SE
                ) %>%
                    mutate(
                        Prob. = inv.logit(coefficient)
                    ) %>%
                    select(
                        statement,
                        control,
                        Prob.,
                        coefficient,
                        SE
                    )
            )
        }
    )
tbl_ate <- do.call(rbind, tbl_ate)

### combine tables
tbl_intercept <-
    rbind(
        tbl_intercept,
        tbl_ate
    )
    
### delta method for testing if there is a variation in treatment effect across control lists
# construct formulas
design_effect <-
    lapply(
        1:n_ss,
        function(x) {
            effects <-
                paste0(
                    "`sensitive.",
                    x,
                    ".as.factor(list_id)",
                    1:n_cl,
                    "`"
                )

            formula <-
                lapply(
                    1:n_cl,
                    function(y) {
                        ### create formula for each permutation
                        paste0(
                            effects[y],
                            " - ",
                            effects[-y]
                        )
                    }
                )

            ### matrix to store delta method Estimates
            delta_estimates <-
                matrix(
                    NA,
                    nrow = n_cl,
                    ncol = n_cl
                )

            ### matrix to store delta method SE
            delta_se <-
                matrix(
                    NA,
                    nrow = n_cl,
                    ncol = n_cl
                )

            ### matrix to store delta method p-value
            delta_p <-
                matrix(
                    NA,
                    nrow = n_cl,
                    ncol = n_cl
                )

            for (i in 1:n_cl) {
                for (j in 1:n_cl) {
                    if (i < j) { ### upper triangle
                        f <- formula[[i]][j - 1]

                        delta_est <-
                            deltaMethod(
                                ict_fit_intercept,
                                f
                            )

                        delta_estimates[i, j] <- delta_est$Estimate
                        delta_se[i, j] <- delta_est$SE
                        delta_p[i, j] <- 2 * (1 - pnorm(abs(delta_est$Estimate / delta_est$SE)))
                    } else {
                        if (i == j) { ### diagonal
                            delta_estimates[i, j] <- 0
                            delta_se[i, j] <- 0
                            delta_p[i, j] <- 1
                        } else { ### lower triangle
                            f <- formula[[i]][j]

                            delta_est <-
                                deltaMethod(
                                    ict_fit_intercept,
                                    f
                                )

                            delta_estimates[i, j] <- delta_est$Estimate
                            delta_se[i, j] <- delta_est$SE
                            delta_p[i, j] <- 2 * (1 - pnorm(abs(delta_est$Estimate / delta_est$SE)))
                        }
                    }

                    ### combine into a data frame
                    colnames(delta_estimates) <- paste0("vs. Control List ", 1:n_cl, ", Estimate")
                    colnames(delta_se) <- paste0("vs. Control List ", 1:n_cl, ", SE")
                    colnames(delta_p) <- paste0("vs. Control List ", 1:n_cl, ", p-value")
                }
            }

            return(
                data.frame(
                    statement =
                        statement_reference$statement[
                            statement_reference$treatment == x
                        ],
                    control_list = paste0("Control List ", 1:n_cl)
                ) %>%
                    cbind(
                        as.data.frame(delta_estimates),
                        as.data.frame(delta_se),
                        as.data.frame(delta_p)
                    )
            )
        }
    )
design_effect <- do.call(rbind, design_effect)
    
################################################################################
### run list experiment, with demographics
################################################################################
message("Running list experiment, with demographics...")
dt_demo <- fread("csv/dt_demo.csv")

### merge demographics with counts
dt_count <-
    dt_count %>%
    left_join(
        dt_demo
    )

### fit model using information treatment
ict_fit_demo_info <-
    ictreg(
        count ~ as.factor(list_id) - 1 +
        framing_effect +
        co2_value,
        data = dt_count,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )

### fit model using information treatment + attitudes
ict_fit_demo_info_att <-
    ictreg(
        count ~ as.factor(list_id) - 1 +
        framing_effect +
        co2_value +
        climate_important+
        know_climate +
        climate_cause,
        data = dt_count,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )

### fit model using information treatment + attitudes + demographics
ict_fit_demo_info_att_demo <-
    ictreg(
        count ~ as.factor(list_id) - 1 +
        framing_effect +
        co2_value +
        climate_important+
        know_climate +
        climate_cause +
        location +
        where_live +
        age +
        gender +
        education +
        diet +
        meat_frequeny +
        income,
        data = dt_count,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )

################################################################################
### save output
################################################################################
message("Save output...")
save.image(
    file = paste0("./image/est_", date, ".RData")
)

write.xlsx(
    list(
        "intercept" = tbl_intercept,
        "Design Effect Test" = design_effect
    ),
    file = "./tables/tbl_est.xlsx"
)

################################################################################
print("Done!")
sink()
sink(type = "message")
closeAllConnections()