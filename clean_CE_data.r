library(data.table)
library(dplyr)
library(stringr)
library(gtools)

rm(list = ls())
set.seed(1201202)

################################################################################
### Read in data
################################################################################
data <- fread("csv/qual5353-23-01-grocery_March 27, 2024_15.04 2.csv")

### subset data to id and choice columns
data <-
    data[-c(1:2)] %>%
    select(
        c(
            ResponseId,
            starts_with("CT_")
        )
    ) %>%
    as.data.table()

### design
design <- fread("design/design.csv")

### figure out some global data
n_scenarios <-  length(unique(design$scenario))
n_opts <- length(unique(design$opt))

################################################################################
### Expand design into best-worst scaling
################################################################################

perm <- permutations(3, 2)

################################################################################
### construct data for the best-worst estimation
################################################################################

i <- 1

colnames <- paste0(
    paste0(
        "CT_",
        i
    ),
    c("_Pref", "_Least")
)

dt_temp <-
    data[,
        .SD,
        .SDcols = colnames
    ]

### recode selections
dt_temp[,
    c(colnames) := lapply(
        .SD,
        function(x) {
            x[x == "No offsetting"] <- "C"
            x <- gsub("Option ", "", x)
            return(x)
        }
    ),
    .SDcols = colnames
]