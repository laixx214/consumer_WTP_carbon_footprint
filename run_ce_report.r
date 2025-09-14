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
library(broom)

rm(list = ls())
set.seed(1201202)
load("./output/dt_ce.RData")

date <- gsub("-", "", as.character(Sys.Date()))
### sink message
my_log <-
    file(
        paste(
            "./run_ce_report_",
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
### mixed logit
message("Running mixed logit...")

f <- formula(
    paste0(
        "y ~ -1 +",
        paste0(
            names(dt)[5:13],
            collapse = " + "
        )
    )
)

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
save.image("./output/ce_est_report.RData")

################################################################################
### mixed logit + co2 consumption
################################################################################
message("Running mixed logit with co2 consumption...")
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
        " | 0 | 0 | co2_value - 1"
    )
)

### define interactions
mvarlist_ctr_e <- list(
    location_EU = c("co2_value"),
    location_UK = c("co2_value"),
    certificate_NGO = c("co2_value"),
    certificate_UK = c("co2_value"),
    project_renewable = c("co2_value"),
    project_landfill = c("co2_value"),
    project_manure = c("co2_value")
)

### run mixed logit
mlogit_ctrl_e <-
    gmnl(
        f,
        data = dt,
        model = "mixl",
        ranp = randpar,
        mvar = mvarlist_ctr_e,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
)
summary(mlogit_ctrl_e)
save.image("./output/ce_est_report.RData")

################################################################################
### mixed logit + framing effect
################################################################################
message("Running mixed logit with framing effect...")
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
        " | 0 | 0 | framing_effectconsequence + framing_effectMetOffice + framing_effectUN - 1"
    )
)

interactions <- c(
    "framing_effectconsequence",
    "framing_effectMetOffice",
    "framing_effectUN"
)
### define interactions
mvarlist_ctrl_f <- list(
    location_EU = interactions,
    location_UK = interactions,
    certificate_NGO = interactions,
    certificate_UK = interactions,
    project_renewable = interactions,
    project_landfill = interactions,
    project_manure = interactions
)

### run mixed logit
mlogit_ctrl_f <-
    gmnl(
        f,
        data = dt,
        model = "mixl",
        ranp = randpar,
        mvar = mvarlist_ctrl_f,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_f)
save.image("./output/ce_est_report.RData")

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

interactions <- c(
    "co2_value",
    "framing_effectconsequence",
    "framing_effectMetOffice",
    "framing_effectUN"
)
### define interactions
mvarlist_ctrl_ef <- list(
    location_EU = interactions,
    location_UK = interactions,
    certificate_NGO = interactions,
    certificate_UK = interactions,
    project_renewable = interactions,
    project_landfill = interactions,
    project_manure = interactions
)

### run mixed logit
mlogit_ctrl_ef <-
    gmnl(
        f,
        data = dt,
        model = "mixl",
        ranp = randpar,
        mvar = mvarlist_ctrl_ef,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_ef)
save.image("./output/ce_est_report.RData")

################################################################################
### mixed logit + climate_important
################################################################################
message("Running mixed logit with climate_important...")
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
        climate_important +
        -1"
    )
)

interactions <- c(
    "co2_value",
    "framing_effectconsequence",
    "framing_effectMetOffice",
    "framing_effectUN",
    "climate_important"
)

### define interactions
mvarlist_ctrl_ci <- list(
    location_EU = interactions,
    location_UK = interactions,
    certificate_NGO = interactions,
    certificate_UK = interactions,
    project_renewable = interactions,
    project_landfill = interactions,
    project_manure = interactions
)

### run mixed logit
mlogit_ctrl_ci <-
    gmnl(
        f,
        data = dt,
        model = "mixl",
        ranp = randpar,
        mvar = mvarlist_ctrl_ci,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_ci)
save.image("./output/ce_est_report.RData")
################################################################################
### mixed logit + 1 principle component for Q12 only
################################################################################
message("Running mixed logit with 1 principle component for Q12 only...")
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
        Q12_PC1 +
        -1"
    )
)

interactions <- c(
    "co2_value",
    "framing_effectconsequence",
    "framing_effectMetOffice",
    "framing_effectUN",
    "Q12_PC1"
)

### define interactions
mvarlist_p1rd <- list(
    location_EU = interactions,
    location_UK = interactions,
    certificate_NGO = interactions,
    certificate_UK = interactions,
    project_renewable = interactions,
    project_landfill = interactions,
    project_manure = interactions
)

### run mixed logit
mlogit_ctrl_p1_12 <-
    gmnl(
        f,
        data = dt,
        model = "mixl",
        ranp = randpar,
        mvar = mvarlist_p1rd,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_p1_12)
save.image("./output/ce_est_report.RData")

################################################################################
### mixed logit + 1 principle component for Q12 only + reduced demographic controls
################################################################################
message("Running mixed logit with 1 principle component for Q12 only and reduced demographic controls...")
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
        Q12_PC1 +
        age_group35_54 +
        age_group55_ +
        is_women +
        income_level30_50k +
        income_level50_ +
        -1"
    )
)

interactions <- c(
    "co2_value",
    "framing_effectconsequence",
    "framing_effectMetOffice",
    "framing_effectUN",
    "Q12_PC1",
    "age_group35_54",
    "age_group55_",
    "is_women",
    "income_level30_50k",
    "income_level50_"
)

### define interactions
mvarlist_p1rd <- list(
    location_EU = interactions,
    location_UK = interactions,
    certificate_NGO = interactions,
    certificate_UK = interactions,
    project_renewable = interactions,
    project_landfill = interactions,
    project_manure = interactions
)

### run mixed logit
mlogit_ctrl_p1_12_rd <-
    gmnl(
        f,
        data = dt,
        model = "mixl",
        ranp = randpar,
        mvar = mvarlist_p1rd,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_p1_12_rd)

################################################################################
### effect of attitude on WTP
################################################################################

### try for each of the 6 attitudes if predicts CO2 consumption
var_list <-
    paste0(
        "Q12_",
        1:6
    )

summary_tbl <-
    lapply(
        var_list,
        function(x) {
            
            dt_demo[[x]] <- factor(
                dt_demo[[x]],
                levels = c(
                    "Unsure",
                    "Stronglydisagree",
                    "Mildlydisagree",
                    "Mildlyagree",
                    "Stronglyagree"
                )
            )
            
            lm_fit <-
                lm(
                    formula(paste0("co2_value ~ ", x)),
                    data = dt_demo
                )

            summary_tbl <-
                tidy(lm_fit) %>%
                mutate(
                    variable = gsub(
                        "Q12_",
                        "Q",
                        x
                    ),
                    term = gsub(
                        x,
                        "",
                        term
                    )
                ) %>%
                select(
                    variable,
                    term,
                    estimate,
                    std.error
                )
        }
    )
summary_tbl <- do.call(rbind, summary_tbl)

################################################################################
### save image
################################################################################

save.image("./output/ce_est_report.RData")

################################################################################
print("Done!")
sink()
sink(type = "message")
closeAllConnections()