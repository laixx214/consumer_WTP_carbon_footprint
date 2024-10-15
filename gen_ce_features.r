library(data.table)
library(dplyr)
library(stringr)
library(gtools)
library(gmnl)
library(mlogit)
library(ordPens)

rm(list = ls())
set.seed(1201202)

################################################################################
### Read in data
################################################################################
message("Reading in data...")
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
        framing_effect,
        Framing_assigned
    )

### principle component analysis for Q9, 10, 12
message("Principle component analysis...")
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
    ) %>%
    mutate(
        age_group = case_when(
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
            ) ~ "55_"
        ),
        is_women = as.numeric(gender == "Woman"),
        diet_type = relevel(
            as.factor(
                case_when(
                    diet %in% c(
                        "Vegan",
                        "Vegetarian"
                    ) ~ "Vegan_Vegetarian",
                    diet %in% c(
                        "Flexitarian",
                        "others",
                        "Pescatarian"
                    ) ~ "Flexitarian",
                    .default = "Omnivorous"
                )
            ),
            ref = "Omnivorous"
        ),
        education_level = relevel(
            as.factor(
                case_when(
                    education %in% c(
                        "GCSE",
                        "None",
                        "Other",
                        "A_Level",
                        "Vocational"
                    ) ~ "Others",
                    .default = education
                )
            ),
            ref = "Others"
        ),
        income_level = case_when(
            income %in% c(
                "0_10k",
                "10_20k",
                "20_30k",
                "not_specified"
            ) ~ "0_30k",
            income %in% c(
                "30_40k",
                "40_50k"
            ) ~ "30_50k",
            income %in% c(
                "50_60k",
                "60k_"
            ) ~ "50_"
        ),
        is_shopper = as.numeric(shopper == "Yes")
    )

### create dummy variables
dt_ctrl <-
    cbind(
        dt_ctrl %>%
            select(
                -is_women,
                -hh_size,
                -n_children,
                -is_shopper
            ),
        model.matrix(
            ~ framing_effect +
            age_group +
            is_women +
            diet_type +
            education_level +
            hh_size +
            income_level +
            n_children +
            is_shopper +
            where_live,
            data = dt_ctrl
        )
    )

### combine demographics data with the main data
data <- merge(data, dt_ctrl, by = "ResponseId")

### define discrete choice data
message("Defining discrete choice data...")
dt <- mlogit.data(
    data,
    choice = "y",
    shape = "long",
    alt.var = "alt",
    chid.var = "cid",
    id.var = "ResponseId"
)
save.image("./output/dt_ce.RData")