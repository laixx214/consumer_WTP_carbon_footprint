library(data.table)
library(dplyr)
library(tidyverse)

rm(list = ls())
set.seed(1201202)

################################################################################
### Read in data
################################################################################
data <- fread("csv/qual5353-23-01-grocery_March 27, 2024_15.04 2.csv")

### drop row 2 and 3, save data and re-read
data <- data[-c(1, 2)]
fwrite(data, "csv/list_data.csv")
data <- fread("csv/list_data.csv")

### rename Set*_Agree to count*
setnames(
    data,
    c("Set1_Agree", "Set2_Agree", "Set3_Agree", "Set4_Agree"),
    c("count1", "count2", "count3", "count4")
)

### rename Statement* to statement*
setnames(
    data,
    c("Statement1", "Statement2", "Statement3", "Statement4"),
    c("statement1", "statement2", "statement3", "statement4")
)

### subset data
id <- "ResponseId"
count_col <-
    c(
        "count1",
        "count2",
        "count3",
        "count4"
    )

sensitive_col <-
    c(
        "statement1",
        "statement2",
        "statement3",
        "statement4"
    )

sensitive_statements <-
    c(
        "ss1" = unique(data$Statement1_text),
        "ss2" = unique(data$Statement2_text),
        "ss3" = unique(data$Statement3_text),
        "ss4" = unique(data$Statement4_text)
    )

### reshape Set and Statement colnames from wide to long
dt_counts <-
    melt(
        data[,
            .SD,
            .SDcols = c(id, count_col, sensitive_col)
        ],
        measure.vars = list(count_col, sensitive_col),
        value.name = c("count", "statement"),
    ) %>%
    mutate(
        treatment = case_when(
            statement == sensitive_statements[1] ~ 1,
            statement == sensitive_statements[2] ~ 2,
            statement == sensitive_statements[3] ~ 3,
            statement == sensitive_statements[4] ~ 4,
            statement == "" ~ 0,
            .default = NA
        )
    ) %>%
    rename(
        list_id = variable
    )

### save dt_counts
fwrite(dt_counts, "./csv/dt_counts.csv")

# save sensitive_statements
save(sensitive_statements, file = "./image/sensitive_statements.RData")

################################################################################
### demographic data
################################################################################