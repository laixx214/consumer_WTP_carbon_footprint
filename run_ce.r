# install.packages(
#     "https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz",
#     repos = NULL, type = "source"
# )

library(data.table)
library(dplyr)
library(stringr)
library(gtools)
library(gmnl)
library(mlogit)
library(ordPens)
library(nnet)

rm(list = ls())
set.seed(1201202)
load("./output/dt_ce.RData")

date <- gsub("-", "", as.character(Sys.Date()))
### sink message
my_log <-
    file(
        paste(
            "./consumer_WTP_carbon_footprint/run_ce_",
            date,
            ".log",
            sep = ""
        )
    )
sink(my_log, append = TRUE, type = "output")
sink(my_log, append = TRUE, type = "message")

################################################################################
### basic logit
################################################################################
message("Running basic logit...")
### mlogit
f <- formula(
    paste0(
        "y ~ -1 +",
        paste0(
            names(dt)[5:13],
            collapse = " + "
        )
    )
)

logit_bsc <- gmnl(
    f,
    data = dt,
    model = "mnl"
)
summary(logit_bsc)

### mixed logit
randpar <- c(
    location_EU = "n",
    location_UK = "n",
    certificate_NGO = "n",
    certificate_UK = "n",
    project_renewable = "n",
    project_landfill = "n",
    project_manure = "n"
)

mlogit_bsc <- gmnl(
    f,
    data = dt,
    model = "mixl",
    ranp = randpar,
    R = 2000,
    panel = T,
    haltons = NA,
    method = "bhhh",
    iterlim = 5000
)
summary(mlogit_bsc)
save.image("./output/ce_est.RData")
################################################################################
### mixed logit + co2 consumption + framing effect
################################################################################
message("Running mixed logit with co2 consumption and framing effect...")
### mixed logit with controls
f <- formula(
    paste0(
        paste0(
            "y ~ -1 +",
            paste0(
                names(dt)[5:13],
                collapse = " + "
            )
        ),
        " | 0 | 0 |
        co2_value + 
        framing_effectconsequence +
        framing_effectMetOffice +
        framing_effectUN +
        - 1"
    )
)

### define interactions
mvarlist_1 <- list(
    location_EU = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN"),
    location_UK = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN"),
    certificate_NGO = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN"),
    certificate_UK = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN"),
    project_renewable = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN"),
    project_landfill = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN"),
    project_manure = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN")
)

### run mixed logit
mlogit_ctrl_1 <-
    gmnl(
        f,
        data = dt,
        model = "mixl",
        ranp = randpar,
        mvar = mvarlist_1,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_1)
save.image("./output/ce_est.RData")
################################################################################
### mixed logit + co2 consumption + framing effect + principle components
################################################################################
message("Running mixed logit with co2 consumption, framing effect, and principle components...")
### mixed logit with controls
f <- formula(
    paste0(
        paste0(
            "y ~ -1 +",
            paste0(
                names(dt)[5:13],
                collapse = " + "
            )
        ),
        " | 0 | 0 |
        co2_value + 
        framing_effectconsequence +
        framing_effectMetOffice +
        framing_effectUN +
        Q9_PC1 +
        Q9_PC2 +
        Q10_PC1 +
        Q10_PC2 +
        - 1"
    )
)

### define interactions
mvarlist_2 <- list(
    location_EU = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN", "Q9_PC1", "Q9_PC2", "Q10_PC1", "Q10_PC2"),
    location_UK = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN", "Q9_PC1", "Q9_PC2", "Q10_PC1", "Q10_PC2"),
    certificate_NGO = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN", "Q9_PC1", "Q9_PC2", "Q10_PC1", "Q10_PC2"),
    certificate_UK = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN", "Q9_PC1", "Q9_PC2", "Q10_PC1", "Q10_PC2"),
    project_renewable = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN", "Q9_PC1", "Q9_PC2", "Q10_PC1", "Q10_PC2"),
    project_landfill = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN", "Q9_PC1", "Q9_PC2", "Q10_PC1", "Q10_PC2"),
    project_manure = c("co2_value", "framing_effectconsequence", "framing_effectMetOffice", "framing_effectUN", "Q9_PC1", "Q9_PC2", "Q10_PC1", "Q10_PC2")
)

### run mixed logit
mlogit_ctrl_2 <-
    gmnl(
        f,
        data = dt,
        model = "mixl",
        ranp = randpar,
        mvar = mvarlist_2,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_2)
save.image("./output/ce_est.RData")

################################################################################
### latent class model + mixed logit
################################################################################

### without PCA as control
f1 <- formula(
    y ~ 
    -1 +
    I +
    price + price:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN) +
    location_EU + location_EU:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN) +
    location_UK + location_UK:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN) +
    certificate_NGO + certificate_NGO:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN) +
    certificate_UK + certificate_UK:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN) +
    project_renewable + project_renewable:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN) +
    project_landfill + project_landfill:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN) +
    project_manure + project_manure:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN)
    | 0 | 0 | 0 | 1
)

### with PCA as control
f2 <- formula(
    y ~ 
    -1 +
    I +
    price + price:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN + Q9_PC1 + Q9_PC2 + Q10_PC1 + Q10_PC2) +
    location_EU + location_EU:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN + Q9_PC1 + Q9_PC2 + Q10_PC1 + Q10_PC2) +
    location_UK + location_UK:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN + Q9_PC1 + Q9_PC2 + Q10_PC1 + Q10_PC2) +
    certificate_NGO + certificate_NGO:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN + Q9_PC1 + Q9_PC2 + Q10_PC1 + Q10_PC2) +
    certificate_UK + certificate_UK:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN + Q9_PC1 + Q9_PC2 + Q10_PC1 + Q10_PC2) +
    project_renewable + project_renewable:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN + Q9_PC1 + Q9_PC2 + Q10_PC1 + Q10_PC2) +
    project_landfill + project_landfill:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN + Q9_PC1 + Q9_PC2 + Q10_PC1 + Q10_PC2) +
    project_manure + project_manure:(co2_value + framing_effectconsequence + framing_effectMetOffice + framing_effectUN + Q9_PC1 + Q9_PC2 + Q10_PC1 + Q10_PC2)
    | 0 | 0 | 0 | 1
)

### Latent class model from 2 to 5 classes, without PCA
lc_est_1 <-
    lapply(
        2:5,
        function(q) {
            ### run latent class model
            lc_ctrl <-
                tryCatch(
                    {
                        gmnl(
                            f1,
                            data = dt,
                            model = "lc",
                            Q = q,
                            panel = TRUE,
                            method = "bhhh",
                            iterlim = 5000
                        )
                    },
                    error = function(e) {
                        "Failed"
                    }
                )
            summary(lc_ctrl)
            
            ### if successful, estimate effect of demographics
            if (class(lc_ctrl) == "character") {
                Y <- lc_ctrl$Qir
                X <- as.matrix(
                    data %>%
                        select(
                            ResponseId,
                            age_group35_54,
                            age_group55_,
                            is_women,
                            diet_typeFlexitarian,
                            diet_typeVegan_Vegetarian,
                            education_levelDegree,
                            education_levelPostgraduate,
                            hh_size,
                            income_level30_50k,
                            income_level50_,
                            n_children,
                            is_shopper,
                            where_liveRuralarea,
                            where_liveTownorsuburb
                        ) %>%
                        distinct() %>%
                        select(
                            -ResponseId
                        )
                )

                lc_ctrl_demo <-
                    multinom(
                        Y ~ X
                    )
                summary(lc_ctrl_demo)
            } else {
                lc_ctrl_demo <- "Failed"
            }
            
            return(
                list(
                    lc_est = lc_ctrl,
                    lc_demo = lc_ctrl_demo
                )
            )
        }
    )

### 2 classes, without PCA
lc_est_2 <-
    lapply(
        2:5,
        function(q) {
            ### run latent class model
            lc_ctrl <-
                tryCatch(
                    {
                        gmnl(
                            f2,
                            data = dt,
                            model = "lc",
                            Q = q,
                            panel = TRUE,
                            method = "bhhh",
                            iterlim = 5000
                        )
                    },
                    error = function(e) {
                        "Failed"
                    }
                )
            summary(lc_ctrl)

            ### if successful, estimate effect of demographics
            if (class(lc_ctrl) == "character") {
                Y <- lc_ctrl$Qir
                X <- as.matrix(
                    data %>%
                        select(
                            ResponseId,
                            age_group35_54,
                            age_group55_,
                            is_women,
                            diet_typeFlexitarian,
                            diet_typeVegan_Vegetarian,
                            education_levelDegree,
                            education_levelPostgraduate,
                            hh_size,
                            income_level30_50k,
                            income_level50_,
                            n_children,
                            is_shopper,
                            where_liveRuralarea,
                            where_liveTownorsuburb
                        ) %>%
                        distinct() %>%
                        select(
                            -ResponseId
                        )
                ) else {
                    lc_ctrl_demo <- "Failed"
                }

                lc_ctrl_demo <-
                    multinom(
                        Y ~ X
                    )
                summary(lc_ctrl_demo)
            }

            return(
                list(
                    lc_est = lc_ctrl,
                    lc_demo = lc_ctrl_demo
                )
            )
        }
    )
save.image("./output/ce_est.RData")

################################################################################
print("Done!")
sink()
sink(type = "message")
closeAllConnections()