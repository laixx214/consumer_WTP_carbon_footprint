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
### save image
################################################################################
save.image("./output/ce_est.RData")

################################################################################
print("Done!")
sink()
sink(type = "message")
closeAllConnections()