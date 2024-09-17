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

rm(list = ls())
set.seed(1201202)

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
### Read in data
################################################################################
data <- fread("./csv/data_bw.csv")
data[, cid := .GRP, by = .(ResponseId, scenario)]

### merge in demographics 
dt_demo <- fread("csv/dt_demo.csv")

### select useful columns
dt_ctrl <- 
    dt_demo %>%
    select(
        ResponseId,
        location,
        age,
        gender,
        diet,
        education,
        hh_size,
        income,
        n_children,
        shopper,
        income,
        where_live,
        co2_value,
        framing_effect
    )

### principle component analysis for Q9, 10, 12
# define the sequence of lambda
lambda_seq <- exp(seq(5, -10, by = -0.1))
# for Q9
dt_q9 <- 
    dt_demo %>%
    select(starts_with("Q9")) %>%
    mutate(
        across(
            starts_with("Q9_"),
            ~ case_when(
                .x == "Stronglyagree" ~ 1,
                .x == "Agree" ~ 2,
                .x == "Neutral" ~ 3,
                .x == "Disagree" ~ 4,
                .x == "Stronglydisagree" ~ 5
            )
        )
    )

pc_fit_q9 <-
    dt_q9 %>%
        ordPCA(
            p = 2,
            lambda = lambda_seq,
            CV = TRUE,
            k = 10,
            CVfit = FALSE
        )
l_star <- lambda_seq[which.max(
    apply(pc_fit_q9$VAFtest, 2, mean)
)]

pc_fit_q9_star <-
    dt_q9 %>%
    ordPCA(
        p = 2,
        lambda = l_star
    )

pc_q9_mat <- pc_fit_q9_star$X
colnames(pc_q9_mat) <- paste0("Q9_PC", 1:2)

#for Q10
dt_q10 <- 
    dt_demo %>%
    select(starts_with("Q10")) %>%
    mutate(
        across(
            starts_with("Q10_"),
            ~ case_when(
                .x == "AlmostAlways" ~ 1,
                .x == "Often" ~ 2,
                .x == "Sometimes" ~ 3,
                .x == "Rarely" ~ 4,
                .x == "Never" ~ 5
            )
        )
    )

pc_fit_q10 <-
    dt_q10 %>%
        ordPCA(
            p = 2,
            lambda = lambda_seq,
            CV = TRUE,
            k = 10,
            CVfit = FALSE
        )

l_star <- lambda_seq[which.max(
    apply(pc_fit_q10$VAFtest, 2, mean)
)]

pc_fit_q10_star <-
    dt_q10 %>%
    ordPCA(
        p = 2,
        lambda = l_star
    )

pc_q10_mat <- pc_fit_q10_star$X
colnames(pc_q10_mat) <- paste0("Q10_PC", 1:2)

### merge in the principle components
dt_ctrl <-
    cbind(
        dt_ctrl,
        pc_q9_mat,
        pc_q10_mat
    )

### create dummy variables
dt_ctrl <-
    cbind(
        dt_ctrl,
        model.matrix(
            ~ -1 + framing_effect,
            data = dt_ctrl
        )
    )

### combine demographics data with the main data
data <- merge(data, dt_ctrl, by = "ResponseId")

### define discrete choice data
dt <- mlogit.data(
    data,
    choice = "y",
    shape = "long",
    alt.var = "alt",
    chid.var = "cid",
    id.var = "ResponseId"
)

################################################################################
### basic logit
################################################################################

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
    price = "n",
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

################################################################################
### mixed logit + co2 consumption + framing effect
################################################################################

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

### define random parameters
randpar <- c(
    price = "n",
    location_EU = "n",
    location_UK = "n",
    certificate_NGO = "n",
    certificate_UK = "n",
    project_renewable = "n",
    project_landfill = "n",
    project_manure = "n"
)

### define interactions
mvarlist <- list(
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
        mvar = mvarlist,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_1)

################################################################################
### mixed logit + co2 consumption + framing effect + principle components
################################################################################

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

### define random parameters
randpar <- c(
    price = "n",
    location_EU = "n",
    location_UK = "n",
    certificate_NGO = "n",
    certificate_UK = "n",
    project_renewable = "n",
    project_landfill = "n",
    project_manure = "n"
)

### define interactions
mvarlist <- list(
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
        mvar = mvarlist,
        R = 2000,
        panel = T,
        haltons = NA,
        method = "bhhh",
        iterlim = 5000
    )
summary(mlogit_ctrl_2)

################################################################################
### latent class model
################################################################################
f <- 
    formula(
        paste0(
            "y ~ -1 +",
            paste0(
                names(dt)[5:13],
                collapse = " + | 0 | 0 | 0 | 1"
            )
        )
    )

### 2 classes
lc2 <- gmnl(
    f,
    data = dt,
    model = "lc",
    Q = 2,
    panel = T,
    method = "bhhh"
)
summary(lc2)

### 3 classes
lc3 <- gmnl(
    f,
    data = dt,
    model = "lc",
    Q = 3,
    panel = T,
    method = "bhhh"
)
summary(lc3)

### 4 classes
lc4 <- gmnl(
    f,
    data = dt,
    model = "lc",
    Q = 4,
    panel = T,
    method = "bhhh"
)
summary(lc4)

### 5 classes
lc5 <- gmnl(
    f,
    data = dt,
    model = "lc",
    Q = 5,
    panel = T,
    method = "bhhh"
)
summary(lc5)

################################################################################
### save image
################################################################################
save.image("./output/ce_est.RData")

################################################################################
print("Done!")
sink()
sink(type = "message")
closeAllConnections()