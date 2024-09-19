library(data.table)
library(dplyr)
library(tidyverse)
library(stringr)
library(openxlsx)

rm(list = ls())
set.seed(1201202)

################################################################################
### demographic data
################################################################################
dt_demo <-
    fread("csv/list_data.csv") %>%
    select(
        ResponseId,
        Q1:Q8,
        starts_with("Q9_"),
        starts_with("Q10_"),
        Q11_1:Q11_2,
        Q12_1:Q12_6,
        Q15:Q20,
        Framing,
        Framing_assigned,
        CO2eq,
        value
    ) %>%
    # rename columns
    rename(
        location = Q1,
        age = Q2,
        gender = Q3,
        diet = Q4,
        climate_important = Q5,
        know_climate = Q6,
        climate_cause = Q7,
        meat_frequency = Q8,
        env_friendly_consumer = Q11_1,
        concerned_environment = Q11_2,
        education = Q15,
        hh_size = Q16,
        n_children = Q17,
        shopper = Q18,
        income = Q19,
        where_live = Q20,
        co2_value = value
    ) %>%
    # recode values
    mutate(
        location = gsub(" ", "", location),
        age = case_when(
            grepl("18-24", age) ~ "18_24",
            grepl("25-34", age) ~ "25_34",
            grepl("35-44", age) ~ "35_44",
            grepl("45-54", age) ~ "45_54",
            grepl("55-64", age) ~ "55_64",
            grepl("65+", age) ~ "65_"
        ),
        gender = case_when(
            gender == "Genderqueer or non-binary" ~ "others",
            gender == "I do not wish to specify" ~ "others",
            .default = gender
        ),
        diet = case_when(
            grepl("Flexitarian", diet, ignore.case = TRUE) ~ "Flexitarian",
            grepl("Omnivorous", diet, ignore.case = TRUE) ~ "Omnivorous",
            grepl("Pescatarian", diet, ignore.case = TRUE) ~ "Pescatarian",
            grepl("Vegan", diet, ignore.case = TRUE) ~ "Vegan",
            grepl("Vegetarian", diet, ignore.case = TRUE) ~ "Vegetarian",
            .default = "others"
        ),
        climate_important = as.factor(
            substr(climate_important, 1, 1)
        ),
        know_climate = as.factor(
            substr(know_climate, 1, 1)
        ),
        climate_cause = case_when(
            climate_cause == "Climate change is caused by both natural processes and human activity" ~ "both",
            climate_cause == "Climate change is caused only by human activity" ~ "human",
            climate_cause == "Climate change is caused only by natural processes" ~ "natural",
            grepl("know what is causing climate change", climate_cause) ~ "notknow",
            grepl("no such thing as climate change", climate_cause) ~ "noclimatechange"
        ),
        meat_frequency = case_when(
            meat_frequency == "2 times weekly" ~ "2weekly",
            meat_frequency == "3- 5 times weekly" ~ "3_5weekly",
            grepl("Never", meat_frequency) ~ "Never",
            meat_frequency == "Once a day" ~ "1daily",
            meat_frequency == "Twice a day or more" ~ "2daily"
        ),
        env_friendly_consumer = gsub(" ", "", env_friendly_consumer),
        concerned_environment = gsub(" ", "", concerned_environment),
        education = case_when(
            education == "A-Level/Higher/BTEC" ~ "A_Level",
            education == "Degree or equivalent" ~ "Degree",
            education == "GCSE/O-Level" ~ "GCSE",
            education == "No formal qualifications" ~ "None",
            education == "Postgraduate qualification" ~ "Postgraduate",
            grepl("Vocational", education) ~ "Vocational",
            .default = "Other"
        ),
        hh_size = case_when(
            as.integer(hh_size) < 1 ~ as.integer(0),
            .default = as.integer(hh_size)
        ),
        n_children = as.integer(n_children),
        shopper = case_when(
            shopper == "I have joint responsibility" ~ "joint",
            shopper == "No" ~ "No",
            shopper == "Yes" ~ "Yes"
        ),
        income = case_when(
            grepl("< £10,000", income) ~ "0_10k",
            grepl("£10,001 - £20,000", income) ~ "10_20k",
            grepl("£20,001 - £30,000", income) ~ "20_30k",
            grepl("£30,001 - £40,000", income) ~ "30_40k",
            grepl("£40,001 - £50,000", income) ~ "40_50k",
            grepl("£50,001 - £60, 000", income) ~ "50_60k",
            grepl("> £60,001", income) ~ "60k_",
            .default = "not_specified"
        ),
        where_live = gsub(" ", "", where_live),
        co2_value = as.numeric(co2_value),
        framing_effect = case_when(
            Framing_assigned == 1 ~ "baseline",
            Framing_assigned == 2 ~ "consequence",
            Framing_assigned == 3 ~ "MetOffice",
            Framing_assigned == 4 ~ "MetOffice",
            Framing_assigned == 5 ~ "UN"
        ),
        across(
            starts_with("Q9_"),
            ~ str_replace_all(.x, " ", "")
        ),
        across(
            starts_with("Q10_"),
            ~ str_replace_all(.x, " ", "")
        ),
        across(
            starts_with("Q12_"),
            ~ str_replace_all(.x, " ", "")
        )
    )

### tabulate all categorical variables
lapply(
    dt_demo %>% select(-co2_value, -CO2eq, -ResponseId),
    function(x) table(x)
)

lapply(fread("csv/list_data.csv") %>% select(
    Q1:Q8,
    Q11_1:Q11_2,
    Q12_1:Q12_6,
    Q15:Q20
), table)

### save data
fwrite(dt_demo, "./csv/dt_demo.csv")

################################################################################
### summary of demographic data
################################################################################
dt_demo_raw <-
    fread("csv/list_data.csv")

# variables to summarize
vars <- 
    c(
        "location",
        "age",
        "gender",
        "diet",
        "meat_frequency",
        "climate_important",
        "know_climate",
        "climate_cause",
        "env_friendly_consumer",
        "concerned_environment",
        "education",
        "hh_size",
        "n_children",
        "shopper",
        "income",
        "where_live",
        "framing_effect"
    )
# the corresponding variable ids
var_id <- 
    c(
        "Q1",
        "Q2",
        "Q3",
        "Q4",
        "Q8",
        "Q5",
        "Q6",
        "Q7",
        "Q11_1",
        "Q11_2",
        "Q15",
        "Q16",
        "Q17",
        "Q18",
        "Q19",
        "Q20",
        "Framing"
    )

### for location
tbl_location <- 
    dt_demo_raw %>%
    rename(
        Group = Q1
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Location"
    ) %>%
    arrange(Group) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for age
tbl_age <- 
    dt_demo_raw %>%
    rename(
        Group = Q2
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Age"
    ) %>%
    arrange(Group) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for gender
tbl_gender <- 
    dt_demo_raw %>%
    rename(
        Group = Q3
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Gender"
    ) %>%
    arrange(Group) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)
    
### for diet
tbl_diet <- 
    dt_demo_raw %>%
    rename(
        Group = Q4
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Diet"
    ) %>%
    arrange(Group) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for meat_frequency
tbl_meat_frequency <- 
    dt_demo_raw %>%
    mutate(
        Q8 = case_when(
            Q8 == "2 times weekly" ~ "2 times weekly",
            Q8 == "3- 5 times weekly" ~ "3-5 times weekly",
            grepl("Never", Q8) ~ "Never",
            Q8 == "Once a day" ~ "Once a day",
            Q8 == "Twice a day or more" ~ "Twice a day or more"
        )
    ) %>%
    rename(
        Group = Q8
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Meat Consumption Frequency"
    ) %>%
    arrange(
        match(Group, c(
            "Twice a day or more",
            "Once a day",
            "2 times weekly",
            "3-5 times weekly",
            "Never"
        ))
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for climate_important
tbl_climate_important <- 
    dt_demo_raw %>%
    rename(
        Group = Q5
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Agreed Climate is Important"
    ) %>%
    arrange(Group) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for know_climate
tbl_know_climate <- 
    dt_demo_raw %>%
    rename(
        Group = Q6
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Familiar with Climate Change"
    ) %>%
    arrange(Group) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for climate_cause
tbl_climate_cause <- 
    dt_demo_raw %>%
    rename(
        Group = Q7
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Climate Change Cause"
    ) %>%
    arrange(Group) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)
    
### for env_friendly_consumer
tbl_env_friendly_consumer <- 
    dt_demo_raw %>%
    rename(
        Group = Q11_1
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Think of myself as an environmentally friendly consumer"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        match(Group, c(
            "Strongly agree",
            "Agree",
            "Neutral",
            "Disagree",
            "Strongly disagree"
        ))
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)
    
### for concerned_environment
tbl_concerned_environment <- 
    dt_demo_raw %>%
    rename(
        Group = Q11_2
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "I am very concerned with environmental issues"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        match(Group, c(
            "Strongly agree",
            "Agree",
            "Neutral",
            "Disagree",
            "Strongly disagree"
        ))
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for Q12_*
tbl_q12 <- 
    dt_demo_raw %>%
    select(
        ResponseId,
        Q12_1:Q12_6
    ) %>%
    pivot_longer(
        cols = Q12_1:Q12_6,
        names_to = "Variable",
        values_to = "Group"
    ) %>%
    group_by(Group, Variable) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        Variable,
        match(
            Group,
            c(
                "Strongly agree",
                "Mildly agree",
                "Unsure",
                "Mildly disagree",
                "Strongly disagree"
            )
        )
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for education
tbl_education <- 
    dt_demo_raw %>%
    rename(
        Group = Q15
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Education"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        Group
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for hh_size
tbl_hh_size <- 
    dt_demo_raw %>%
    rename(
        Group = Q16
    ) %>%
    group_by(Group) %>%
    mutate(
        Group = ifelse(Group < 1, 0, Group)
    ) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Household Size"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        Group
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)
    
### for n_children
tbl_n_children <- 
    dt_demo_raw %>%
    rename(
        Group = Q17
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Household Size with Children"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        Group
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for shopper
tbl_shopper <- 
    dt_demo_raw %>%
    rename(
        Group = Q18
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Is the main shopper"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        Group
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for income
tbl_income <- 
    dt_demo_raw %>%
    rename(
        Group = Q19
    ) %>%
    mutate(
        Group = trimws(Group),
    ) %>%
    mutate(
        Group = gsub("60, 000", "60,000", Group)
    ) %>%
    group_by(Group) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Income"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        match(
            Group,
            c(
                "< £10,000",
                "£10,001 - £20,000",
                "£20,001 - £30,000",
                "£30,001 - £40,000",
                "£40,001 - £50,000",
                "£50,001 - £60,000",
                "> £60,001",
                "I do not wish to specify"
            )
        )
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for where_live
tbl_where_live <- 
    dt_demo_raw %>%
    group_by(Q20) %>%
    rename(
        Group = Q20
    ) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Where do you live"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        Group
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### for framing_effect
tbl_framing_effect <- 
    dt_demo_raw %>%
    group_by(Framing) %>%
    rename(
        Group = Framing
    ) %>%
    summarise(
        Frequency = n_distinct(ResponseId)
    ) %>%
    ungroup() %>%
    mutate(
        Percentage = Frequency / sum(Frequency) * 100,
        Variable = "Framing Effect"
    ) %>%
    ### order from Strongly Agree to Strongly Disagree
    arrange(
        Group
    ) %>%
    ### rearrange columns
    select(Variable, Group, Frequency, Percentage)

### write the list of tables to excel file
tbl_demo <- 
    do.call(
        rbind,
        list(
            tbl_location,
            tbl_age,
            tbl_gender,
            tbl_diet,
            tbl_meat_frequency,
            tbl_climate_important,
            tbl_know_climate,
            tbl_climate_cause,
            tbl_env_friendly_consumer,
            tbl_concerned_environment,
            tbl_education,
            tbl_hh_size,
            tbl_n_children,
            tbl_shopper,
            tbl_income,
            tbl_where_live,
            tbl_framing_effect,
            tbl_q12
        )
    )

### write to excel
write.xlsx(tbl_demo, "tables/tbl_demo.xlsx")