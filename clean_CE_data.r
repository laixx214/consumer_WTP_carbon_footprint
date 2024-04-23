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
n_scenarios <- length(unique(design$scenario))
n_opts <- length(unique(design$opt))

################################################################################
### Expand design into best-worst scaling
################################################################################

perm <- permutations(3, 2)
x_vars <-
    names(design)[3:length(names(design))]

choice_scenarios <-
    lapply(
        unique(design$scenario),
        function(i) {
            design_temp <- design[scenario == i]

            x1 <-
                as.matrix(
                    design_temp[perm[, 1], .SD, .SDcols = x_vars]
                )

            x2 <-
                as.matrix(
                    design_temp[perm[, 2], .SD, .SDcols = x_vars]
                )

            x <- x1 - x2

            best <- c(LETTERS[perm[, 1]])
            worst <- c(LETTERS[perm[, 2]])

            dt_return <- cbind(
                data.table(
                    scenario = i,
                    best = best,
                    worst = worst,
                    alt = c(1:nrow(x))
                ),
                as.data.table(x)
            )

            return(
                dt_return
            )
        }
    )
choice_scenarios <- rbindlist(choice_scenarios)

################################################################################
### construct data for the best-worst estimation
################################################################################

### reshape best choices to long format
# subset data to best choices
data_best <-
    data[,
        .SD,
        .SDcols = c(
            "ResponseId",
            paste0(
                paste0("CT_", 1:n_scenarios),
                "_Pref"
            )
        )
    ]

# melt data
data_best <- melt(data_best,
    id.vars = "ResponseId",
    variable.name = "scenario",
    value.name = "y_best"
)

# clean up values in scenario and y_best
data_best$scenario <- str_replace(data_best$scenario, "_Pref", "")
data_best$scenario <- str_replace(data_best$scenario, "CT_", "")
data_best$scenario <- as.integer(data_best$scenario)
data_best$y_best <- str_replace(data_best$y_best, "Option ", "")
data_best[y_best == "No offsetting", y_best := "C"]

### reshape worst choices to long format
# subset data to worst choices
data_worst <-
    data[,
        .SD,
        .SDcols = c(
            "ResponseId",
            paste0(
                paste0("CT_", 1:n_scenarios),
                "_Least"
            )
        )
    ]

# melt data
data_worst <- melt(data_worst,
    id.vars = "ResponseId",
    variable.name = "scenario",
    value.name = "y_worst"
)

# clean up values in scenario and y_worst
data_worst$scenario <- str_replace(data_worst$scenario, "_Least", "")
data_worst$scenario <- str_replace(data_worst$scenario, "CT_", "")
data_worst$scenario <- as.integer(data_worst$scenario)
data_worst$y_worst <- str_replace(data_worst$y_worst, "Option ", "")
data_worst[y_worst == "No offsetting", y_worst := "C"]

### merge best and worst data by scenario
data_bw <- merge(
    data_best,
    data_worst,
    by = c("ResponseId", "scenario")
)

### merge with choice scenarios
data_bw <- merge(
    data_bw,
    choice_scenarios,
    by = "scenario",
    all.x = TRUE,
    allow.cartesian = TRUE
)

### define outcome index
data_bw[, y := as.numeric(y_best == best & y_worst == worst)]

### verify outcome is unique
table(
    data_bw[,
        .(sum_y = sum(y)),
        by = .(ResponseId, scenario)
    ]$sum_y
)

################################################################################
### sample exclusion
################################################################################

### exclude respondents ever choosing the same option as best and worst
data_bw <-
    data_bw %>%
    group_by(
        ResponseId
    ) %>%
    mutate(
        exclude = sum(y_best == y_worst)
    ) %>%
    filter(
        exclude == 0
    ) %>%
    select(
        ResponseId,
        scenario,
        alt,
        y,
        all_of(
            x_vars
        )
    )

### save a copy of the data
fwrite(data_bw, "./csv/data_bw.csv")