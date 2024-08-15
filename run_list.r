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
my_log <- file(paste("run_list_", date, ".log", sep = ""))
sink(my_log, append = TRUE, type = "output")
sink(my_log, append = TRUE, type = "message")

################################################################################
### Define functions
################################################################################

### make summary table
make_summary <-
    function(est) {
        tbl_temp <-
            lapply(
                1:n_ss,
                function(i) {
                    par <- est$par.treat[[i]]
                    se <- est$se.treat[[i]]
                    p <- 2 * (1 - pnorm(abs(par / se)))
                    star <- ifelse(
                        p < 0.01,
                        "***",
                        ifelse(
                            p < 0.05,
                            "**",
                            ifelse(p < 0.1, "*", "")
                        )
                    )
                    variable <- names(par)

                    names(par) <- NULL
                    names(se) <- NULL
                    names(p) <- NULL
                    names(star) <- NULL

                    return(
                        data.frame(
                            statement = statement_reference$statement[statement_reference$treatment == i],
                            variable = variable,
                            coefficient = par,
                            SE = se,
                            p = p,
                            star = star
                        )
                    )
                }
            )
        tbl_temp <- do.call(rbind, tbl_temp)
        return(tbl_temp)
    }

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
dt_demo <- fread("csv/dt_demo.csv")
message("Sensitive statements:")
print(sensitive_statements)

message("Running list experiment, with demographics...")
### merge demographics with counts
dt_est <-
    dt_count %>%
    left_join(
        dt_demo
    ) %>%
    mutate(
        climate_important = case_when(
            climate_important >= 4 ~ "yes",
            .default = "no"
        ),
        know_climate = case_when(
            know_climate >= 4 ~ "yes",
            .default = "no"
        ),
        climate_cause = case_when(
            climate_cause %in% c(
                "noclimatechange",
                "notknow"
            ) ~ "Others",
            .default = climate_cause
        ),
        is_man = ifelse(
            gender == "Man",
            "yes",
            "no"
        ),
        education = case_when(
            education == "None" ~ "Other",
            .default = education
        ),
        diet = case_when(
            diet %in% c(
                "Vegan",
                "Vegetarian"
            ) ~ "vegetarian",
            diet %in% c(
                "Pescatarian",
                "Flexitarian"
            ) ~ "Pescatarian",
            diet %in% c(
                "Omnivorous",
                "others"
            ) ~ "Omnivorous"
        ),
        age = case_when(
            age %in% c(
                "18_24",
                "25_34"
            ) ~ "18_34",
            age %in% c(
                "35_44",
                "45_54"
            ) ~ "35_54",
            age %in% c(
                "55_64",
                "65_"
            ) ~ "55_",
            .default = age
        ),
        higher_education = case_when(
            education %in% c(
                "Degree",
                "Postgraduate"
            ) ~ "yes",
            .default = "no"
        ),
        income = case_when(
            income %in% c(
                "0_10k",
                "10_20k"
            ) ~ "0_20k",
            income %in% c(
                "40_50k",
                "60k_"
            ) ~ "40k_",
            .default = income
        ),
        across(
            starts_with("Q12_"),
            ~ case_when(
                .x %in% c(
                    "Stronglyagree",
                    "Mildlyagree"
                ) ~ "Agree",
                .default = "Unsure_or_disagree"
            )
        )
    ) %>%
    mutate(
        climate_important = factor(climate_important, levels = c("yes", "no")),
        know_climate = factor(know_climate, levels = c("yes", "no")),
        where_live = factor(where_live, levels = c("Townorsuburb", "Ruralarea", "Citycentre")),
        age = relevel(as.factor(age), ref = "35_54"),
        education = relevel(as.factor(education), ref = "Degree"),
        meat_frequency = relevel(as.factor(meat_frequency), ref = "3_5weekly"),
        income = relevel(as.factor(income), ref = "20_30k")
    )

### fit model using information treatment
message("Fitting model using information treatment...")
ict_fit_info <-
    ictreg(
        count ~ as.factor(list_id) +
            framing_effect +
            co2_value,
        data = dt_est,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )
make_summary(ict_fit_info)

### fit model using attitudes only
message("Fitting model using climate attitudes...")
ict_fit_att <-
    ictreg(
        count ~ as.factor(list_id) +
            climate_important +
            know_climate +
            climate_cause +
            Q12_1 +
            Q12_2 +
            Q12_3 +
            Q12_4 +
            Q12_5 +
            Q12_6,
        data = dt_est,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )
make_summary(ict_fit_att)

### fit model using information treatment + climate attitudes
message("Fitting model using information treatment + climate attitudes...")
ict_fit_info_att <-
    ictreg(
        count ~ as.factor(list_id) +
            framing_effect +
            co2_value +
            climate_important +
            know_climate +
            climate_cause +
            Q12_1 +
            Q12_2 +
            Q12_3 +
            Q12_4 +
            Q12_5 +
            Q12_6,
        data = dt_est,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )
make_summary(ict_fit_info_att)

### fit model using demographics
message("Fitting model using demographics...")
ict_fit_demo <-
    ictreg(
        count ~ as.factor(list_id) +
            where_live +
            diet +
            age +
            is_man +
            higher_education +
            income,
        data = dt_est,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )
make_summary(ict_fit_demo)

### fit model using information treatment + demographics
message("Fitting model using information treatment + demographics...")
ict_fit_info_demo <-
    ictreg(
        count ~ as.factor(list_id) +
            framing_effect +
            co2_value +
            where_live +
            diet +
            age +
            is_man +
            higher_education +
            income,
        data = dt_est,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )
make_summary(ict_fit_info_demo)

### fit model using climate attitudes + demographics
message("Fitting model using climate attitudes + demographics...")
ict_fit_att_demo <-
    ictreg(
        count ~ as.factor(list_id) +
            climate_important +
            know_climate +
            climate_cause +
                Q12_1 +
                Q12_2 +
                Q12_3 +
                Q12_4 +
                Q12_5 +
                Q12_6 +
            where_live +
            diet +
            age +
            is_man +
            higher_education +
            income,
        data = dt_est,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )
make_summary(ict_fit_att_demo)

### fit model using information treatment + climate attitudes + demographics
message("Fitting model using information treatment + climate attitudes + demographics...")
ict_fit_info_att_demo <-
    ictreg(
        count ~ as.factor(list_id) +
            framing_effect +
            co2_value +
            climate_important +
            know_climate +
            climate_cause +
                Q12_1 +
                Q12_2 +
                Q12_3 +
                Q12_4 +
                Q12_5 +
                Q12_6 +
            where_live +
            diet +
            age +
            is_man +
            higher_education +
            income,
        data = dt_est,
        treat = "treatment",
        J = n_cs,
        constrained = TRUE
    )
make_summary(ict_fit_info_att_demo)

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