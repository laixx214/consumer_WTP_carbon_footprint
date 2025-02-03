library(data.table)
library(dplyr)
library(stringr)
library(gtools)
library(gmnl)
library(mlogit)
library(stringr)

rm(list = ls())

################################################################################
### load data & define functions
################################################################################

### load data
load("./output/ce_est_report.RData")

### summarize results
get_res <- function(est, select = NULL) {
    res <- summary(est)$CoefTable
    var <- rownames(res)
    res <- as.data.table(res)
    res[, term := var]
    names(res) <- c("estimate", "std.error", "statistic", "p.value", "term")
    if (!is.null(select)) {
        res <- res[, select, with = FALSE]
    } else {
        res <- res[, .(term, estimate, std.error, p.value)]
    }
    return(res)
}

### get willingness to pay
get_wtp <-
    function(est, select = NULL) {
        est$coefficients["price"] <- est$coefficients["price"] * -1
        wtp_tbl <- wtp.gmnl(est, wrt = "price")
        var <- rownames(wtp_tbl)
        wtp_tbl <- as.data.table(wtp_tbl)
        wtp_tbl[, term := var]
        names(wtp_tbl) <- c("estimate", "std.error", "statistic", "p.value", "term")

        if (!is.null(select)) {
            wtp_tbl <- wtp_tbl[, select, with = FALSE]
        } else {
            wtp_tbl <- wtp_tbl[, .(term, estimate, std.error, p.value)]
        }

        wtp_tbl <- wtp_tbl[!grepl("I|sd.", var), ]
        return(
            wtp_tbl
        )
    }

### nice table
nice_table <-
    function(res_all, add_std = TRUE) {
        var <- res_all$term
        est <- res_all %>%
            select(contains("estimate"))
        models <- str_replace_all(names(est), ".estimate", "")

        # if with standard errors, round to 3 significant digits
        # if without standard errors, leave as blank
        std <- res_all %>%
            select(contains("std.error")) %>%
            mutate(
                across(
                    everything(),
                    ~ case_when(
                        !is.na(.) ~ paste(
                            "(",
                            signif(.x, 3),
                            ")",
                            sep = ""
                        ),
                        .default = ""
                    )
                )
            ) %>%
            as.matrix()

        # add stars
        stars <- res_all %>%
            select(contains("p.value")) %>%
            mutate(
                across(
                    everything(),
                    ~ case_when(
                        .x < 0.001 ~ "***",
                        .x < 0.01 & .x >= 0.001 ~ "**",
                        .x < 0.05 & .x >= 0.01 ~ "*",
                        .default = ""
                    )
                )
            )

        # for each estimate, keep 3 digit significant figures and add stars
        est_stars <- matrix(NA, nrow = nrow(est), ncol = ncol(est))
        for (i in 1:ncol(est)) {
            est_stars[, i] <- paste(
                ifelse(
                    !is.na(est[[i]]),
                    signif(est[[i]], 3),
                    ""
                ),
                stars[[i]],
                sep = ""
            )
        }
        colnames(est_stars) <- models

        if (add_std) {
            # slide the standard errors to the bottom of the estimates
            tbl <- matrix(NA, nrow = nrow(est) * 2, ncol = ncol(est))
            for (i in 1:nrow(est)) {
                idx <- (i - 1) * 2 + 1
                tbl[idx, ] <- est_stars[i, ]
                tbl[idx + 1, ] <- std[i, ]
            }
            var_ <- rep(NA, 2 * length(var))
            for (i in 1:length(var)) {
                idx <- (i - 1) * 2 + 1
                var_[idx] <- var[i]
                var_[idx + 1] <- ""
            }

            tbl <- cbind(
                var_,
                tbl
            ) %>%
                as.data.frame()
            names(tbl) <- c("Variable", models)
        } else {
            tbl <- cbind(var, est_stars) %>%
                as.data.frame()
            names(tbl) <- c("Variable", models)
        }

        tbl <-
            tbl %>%
            mutate(
                Variable =
                    str_replace_all(
                        Variable,
                        "framing_effect|project_|_value",
                        ""
                    )
            ) %>%
            mutate(
                Variable = str_replace_all(Variable, "_", " ")
            ) %>%
            mutate(
                Variable = str_replace_all(Variable, "\\.", " * ")
            ) %>%
            mutate(
                Variable = str_replace_all(Variable, "co2", "CO2")
            ) %>%
            mutate(
                Variable = str_replace_all(Variable, "sd * ", "sd. ")
            ) %>%
            mutate(
                Variable = str_replace_all(Variable, "age group35 54", "age 35~54"),
                Variable = str_replace_all(Variable, "age group55", "age 55+"),
                Variable = str_replace_all(Variable, "income level30 50k", "income 30~50k"),
                Variable = str_replace_all(Variable, "income level50k", "income 50k+"),
                Variable = str_replace_all(Variable, "is women", "women")
            ) %>%
            mutate(
                Variable = case_when(
                    Variable == "I" ~ "Intercept",
                    .default = Variable
                )
            )

        return(tbl)
    }

################################################################################
### write coefficient tables to csv
################################################################################
to_report <- c("term", "estimate", "std.error", "p.value")

### basic model
res_bsc <- get_res(mlogit_bsc, select = to_report)
names(res_bsc) <- c("term", paste("BSC", to_report[-1], sep = "."))

### co2
res_e <- get_res(mlogit_ctrl_e, select = to_report)
names(res_e) <- c("term", paste("E", to_report[-1], sep = "."))

### framing
res_f <- get_res(mlogit_ctrl_f, select = to_report)
names(res_f) <- c("term", paste("F", to_report[-1], sep = "."))

### co2 + framing
res_ef <- get_res(mlogit_ctrl_ef, select = to_report)
names(res_ef) <- c("term", paste("EF", to_report[-1], sep = "."))

### co2 + framing + Q12
res_ef_q12 <- get_res(mlogit_ctrl_p1_12, select = to_report)
names(res_ef_q12) <- c("term", paste("EFQ12", to_report[-1], sep = "."))

### co2 + framing + Q12 + demographics
res_ef_q12_dem <- get_res(mlogit_ctrl_p1_12_rd, select = to_report)
names(res_ef_q12_dem) <- c("term", paste("EFQ12RD", to_report[-1], sep = "."))

### combine results
res_all <-
    res_bsc %>%
    full_join(res_e, by = "term") %>%
    full_join(res_f, by = "term") %>%
    full_join(res_ef, by = "term") %>%
    full_join(res_ef_q12, by = "term") %>%
    full_join(res_ef_q12_dem, by = "term")

write.csv(nice_table(res_all, add_std = FALSE),
    "./output/ce_est_report_nostd.csv",
    row.names = FALSE
)

write.csv(nice_table(res_all, add_std = TRUE),
    "./output/ce_est_report.csv",
    row.names = FALSE
)

################################################################################
### write willingness to pay to csv
################################################################################
to_report_wtp <- c("term", "estimate", "std.error", "p.value")

### basic model
wtp_bsc <- get_wtp(mlogit_bsc, select = to_report_wtp)
names(wtp_bsc) <- c("term", paste("BSC", to_report_wtp[-1], sep = "."))

### co2
wtp_e <- get_wtp(mlogit_ctrl_e, select = to_report_wtp)
names(wtp_e) <- c("term", paste("E", to_report_wtp[-1], sep = "."))

### framing
wtp_f <- get_wtp(mlogit_ctrl_f, select = to_report_wtp)
names(wtp_f) <- c("term", paste("F", to_report_wtp[-1], sep = "."))

### co2 + framing
wtp_ef <- get_wtp(mlogit_ctrl_ef, select = to_report_wtp)
names(wtp_ef) <- c("term", paste("EF", to_report_wtp[-1], sep = "."))

### co2 + framing + Q12
wtp_ef_q12 <- get_wtp(mlogit_ctrl_p1_12, select = to_report_wtp)
names(wtp_ef_q12) <- c("term", paste("EFQ12", to_report_wtp[-1], sep = "."))

### co2 + framing + Q12 + demographics
wtp_ef_q12_dem <- get_wtp(mlogit_ctrl_p1_12_rd, select = to_report_wtp)
names(wtp_ef_q12_dem) <- c("term", paste("EFQ12RD", to_report_wtp[-1], sep = "."))
wtp_ef_q12_dem <- wtp_ef_q12_dem[!grepl("I|sd.", wtp_ef_q12_dem$term), ]

### combine results
wtp_all <-
    wtp_bsc %>%
    full_join(wtp_e, by = "term") %>%
    full_join(wtp_f, by = "term") %>%
    full_join(wtp_ef, by = "term") %>%
    full_join(wtp_ef_q12, by = "term") %>%
    full_join(wtp_ef_q12_dem, by = "term")

write.csv(nice_table(wtp_all, add_std = FALSE),
    "./output/ce_wtp_report_nostd.csv",
    row.names = FALSE
)

write.csv(nice_table(wtp_all, add_std = TRUE),
    "./output/ce_wtp_report.csv",
    row.names = FALSE
)

################################################################################
### WTP Plot, Main Effects and Interactive Effects with Co2 Consumption and Framing Effect
################################################################################
mlogit_ctrl_p1_12_rd$coefficients["price"] <- mlogit_ctrl_p1_12_rd$coefficients["price"] * -1
wtp <- wtp.gmnl(mlogit_ctrl_p1_12_rd, wrt = "price")

### plot WTP for framing effect and co2 consumption
wtp_tbl <- wtp[, c(1, 2)] %>%
    as.data.frame() %>%
    mutate(
        lb = Estimate - 1.96 * `Std. Error`,
        ub = Estimate + 1.96 * `Std. Error`
    )
wtp_tbl$var <- rownames(wtp_tbl)
wtp_tbl <- 
    wtp_tbl %>%
    filter(
        grepl("co2_value|framing_effect", var) &
            !grepl("I.", var)
    ) %>%
    mutate(
        var = 
        str_replace_all(
            var,
            "framing_effect|project_|_value",
            ""
        )
    ) %>%
    mutate(
        var = str_replace_all(var, "_", " ")
    ) %>%
    mutate(
        var = str_replace_all(var, "\\.", " * ")
    ) %>%
    mutate(
        var = str_replace_all(var, "co2", "CO2")
    )

wtp_tbl$var <- factor(wtp_tbl$var, levels = wtp_tbl$var)

### main effects
wtp %>%
    as.data.frame() %>%
    mutate(
        lb = Estimate - 1.96 * `Std. Error`,
        ub = Estimate + 1.96 * `Std. Error`
    ) %>%
    mutate(
        var = rownames(wtp)
    ) %>%
    filter(
        var %in% c(
            "location_EU",
            "location_UK",
            "certificate_NGO",
            "certificate_UK",
            "project_renewable",
            "project_landfill",
            "project_manure"
        )
    ) %>%
    mutate(
        var = str_replace_all(var, "_", " ")
    ) %>%
    mutate(
        var = factor(var, levels = unique(var))
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate
        )
    ) +
    geom_point() +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        color = "red",
        width = .2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    # rotate x-axis labels 90 degrees
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("WTP") +
    xlab("Main Effects of Carbon Offsetting Attributes")

### consequence message
wtp_tbl%>%
    filter(
        grepl(" consequence", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate
        )
    ) +
    geom_point() +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        color = "red",
        width = .2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    # rotate x-axis labels 90 degrees
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("WTP") +
    xlab("Interavtive Effects with Consequence Message")

### endorsement
wtp_tbl %>%
    filter(
        grepl(" MetOffice| UN", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate
        )
    ) +
    geom_point() +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        color = "red",
        width = .2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    # rotate x-axis labels 90 degrees
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("WTP") +
    xlab("Interavtive Effects with Endorsement")

### CO2 consumption
wtp_tbl %>%
    filter(
        grepl(" CO2", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate
        )
    ) +
    geom_point() +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        color = "red",
        width = .2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    # rotate x-axis labels 90 degrees
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("WTP") +
    xlab("Interavtive Effects with CO2 Consumption")
