library(data.table)
library(dplyr)
library(stringr)
library(gtools)
library(gmnl)
library(mlogit)

rm(list = ls())

################################################################################
### load data & define functions
################################################################################

### load data
load("./output/ce_est_report.RData")

### summarize results
get_res <- function(est) {
    
    res <- summary(est)$CoefTable
    var <- rownames(res)
    res <- as.data.table(res)
    res[, term := var]
    names(res) <- c("estimate", "std.error", "statistic", "p.value", "term")
    res <- res[, .(term, estimate, std.error, p.value)]
    return(res)
}

### get willingness to pay
get_wtp <-
    function(est) {
        est$coefficients["price"] <- est$coefficients["price"] * -1
        wtp_tbl <- wtp.gmnl(est, wrt = "price")
        var <- rownames(wtp_tbl)
        wtp_tbl <- as.data.table(wtp_tbl)
        wtp_tbl[, term := var]
        names(wtp_tbl) <- c("estimate", "std.error", "statistic", "p.value", "term")
        wtp_tbl <- wtp_tbl[, .(term, estimate, std.error, p.value)]

        wtp_tbl <- wtp_tbl[!grepl("I|sd.", var), ]
        return(
            wtp_tbl
        )
    }

################################################################################
### write coefficient tables to docx
################################################################################
res_bsc <- get_res(mlogit_bsc)
res_bsc_tbl <- nice_table(res_bsc)
flextable::save_as_docx(res_bsc_tbl, path = "./tables/res_bsc.docx")


################################################################################
### write willingness to pay to docx
################################################################################
wtp_bsc <- get_wtp(mlogit_bsc)
wtp_bsc_tbl <- nice_table(wtp_bsc)
flextable::save_as_docx(wtp_bsc_tbl, path = "./tables/wtp_bsc.docx")
