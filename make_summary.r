library(data.table)
library(dplyr)
library(stringr)
library(gtools)
library(gmnl)
library(mlogit)
library(stringr)
library(ggplot2)
library(ggpubr)

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

### co2 + framing + climate_important
res_ef_ci <- get_res(mlogit_ctrl_ci, select = to_report)
names(res_ef_ci) <- c("term", paste("EFCI", to_report[-1], sep = "."))

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
    full_join(res_ef_ci, by = "term") %>%
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

### co2 + framing + climate_important
wtp_ef_ci <- get_wtp(mlogit_ctrl_ci, select = to_report_wtp)
names(wtp_ef_ci) <- c("term", paste("EFCI", to_report_wtp[-1], sep = "."))

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
    full_join(wtp_ef_ci, by = "term") %>%
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
### Figure 2
################################################################################
mlogit_ctrl_e$coefficients["price"] <- mlogit_ctrl_e$coefficients["price"] * -1
wtp_co2 <- 
    wtp.gmnl(mlogit_ctrl_e, wrt = "price") %>%
    as.data.frame() %>%
    mutate(
        lb = Estimate - 1.96 * `Std. Error`,
        ub = Estimate + 1.96 * `Std. Error`,
        var = rownames(.)
    ) %>%
    mutate(
        var = str_replace_all(var, "co2", "CO2"),
        var = str_replace_all(var, "I", "Intercept"),
        var = str_replace_all(var, "project_", ""),
        var = str_replace_all(var, "_value", ""),
        var = str_replace_all(var, "_", " "),
        var = str_replace_all(var, "\\.", " * ")
    )

### main effects
plot_main_effects <-
    wtp_co2 %>%
    filter(
        var %in% c(
            "location EU",
            "location UK",
            "certificate NGO",
            "certificate UK",
            "renewable",
            "landfill",
            "manure"
        )
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
    # rotate x-axis labels 60 degrees and shift down not to overlap
    # also, make text align with the x-axis.
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ylab("WTP") +
    xlab("A: Main Effects") +
    # increase x label size
    theme(axis.title.x = element_text(size = 14))

### CO2 consumption
plot_co2 <-
    wtp_co2 %>%
    filter(
        grepl(" CO2", var)
    ) %>%
    # order the levels by location EU, location UK, certificate NGO, certificate UK, renewable, landfill, manure
    mutate(
        var = factor(var, levels = c(
            "location EU * CO2",
            "location UK * CO2",
            "certificate NGO * CO2",
            "certificate UK * CO2",
            "renewable * CO2",
            "landfill * CO2",
            "manure * CO2"
        ))
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
    # rotate x-axis labels 60 degrees
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ylab("WTP") +
    xlab(expression("B: Interactive Effects of " * CO[2])) +
    # increase x label size
    theme(axis.title.x = element_text(size = 14))

### combine main effects and co2 consumption
ggarrange(
    plot_main_effects,
    plot_co2,
    ncol = 2,
    nrow = 1
)

ggsave(
    "./plots/ce_wtp_main_effects.png",
    width = 12,
    height = 6
)

################################################################################
### Figure 3
################################################################################
# change the sign of price coefficient for WTP calculation
mlogit_ctrl_ef$coefficients["price"] <- mlogit_ctrl_ef$coefficients["price"] * -1
wtp_no_att <- wtp.gmnl(mlogit_ctrl_ef, wrt = "price")

wtp_frame <- 
    wtp_no_att[, c(1, 2)] %>%
    as.data.frame() %>%
    mutate(
        lb = Estimate - 1.96 * `Std. Error`,
        ub = Estimate + 1.96 * `Std. Error`,
        var = rownames(.)
    ) %>%
    mutate(
        var = str_replace_all(var, "framing_effect", ""),
        var = str_replace_all(var, "co2", "CO2"),
        var = str_replace_all(var, "I", "Intercept"),
        var = str_replace_all(var, "project_", ""),
        var = str_replace_all(var, "_value", ""),
        var = str_replace_all(var, "_", " "),
        var = str_replace_all(var, "\\.", " * ")
    )

### effect of consequences
plot_consequence <-
    wtp_frame %>%
    filter(
        grepl(" consequence", var)
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
    # rotate x-axis labels 60 degrees and shift down not to overlap
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
    ylab("WTP") +
    xlab("A: Consequence") +
    # increase x label size
    theme(axis.title.x = element_text(size = 14))

### effect of endorsement
plot_endorsement <-
    wtp_frame %>%
    filter(
        grepl(" MetOffice| UN", var)
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
    # rotate x-axis labels 60 degrees and shift down not to overlap
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ylab("WTP") +
    xlab("B: Endorsement") +
    # increase x label size
    theme(axis.title.x = element_text(size = 14))

### combine consequence and endorsement
ggarrange(
    plot_consequence,
    plot_endorsement,
    ncol = 2,
    nrow = 1
)

ggsave(
    "./plots/ce_wtp_framing_effects.png",
    width = 12,
    height = 6
)

################################################################################
### Figure 4 with climate importance
################################################################################
mlogit_ctrl_ci$coefficients["price"] <- mlogit_ctrl_ci$coefficients["price"] * -1
wtp_with_ci <- wtp.gmnl(mlogit_ctrl_ci, wrt = "price")

### plot WTP for framing effect and co2 consumption
wtp_tbl <- wtp_no_att[, c(1, 2)] %>%
    as.data.frame() %>%
    mutate(
        lb = Estimate - 1.96 * `Std. Error`,
        ub = Estimate + 1.96 * `Std. Error`,
        climate_is_important = FALSE,
        var = rownames(.)
    ) %>%
    rbind(
        wtp_with_ci[, c(1, 2)] %>%
            as.data.frame() %>%
            mutate(
                lb = Estimate - 1.96 * `Std. Error`,
                ub = Estimate + 1.96 * `Std. Error`,
                climate_is_important = TRUE,
                var = rownames(.)
            )
    )

### clean up variable names
wtp_tbl <-
    wtp_tbl %>%
    mutate(
        var = str_replace_all(var, "framing_effect", ""),
        var = str_replace_all(var, "co2", "CO2"),
        var = str_replace_all(var, "I", "Intercept"),
        var = str_replace_all(var, "project_", ""),
        var = str_replace_all(var, "_value", ""),
        var = str_replace_all(var, "_", " "),
        var = str_replace_all(var, "\\.", " * ")
    )

wtp_tbl$var <- factor(wtp_tbl$var, levels = unique(wtp_tbl$var))

### plot effect of CO2 on WTP by if control for climate importance
plot_wtp_co2 <-
    wtp_tbl %>%
    filter(
        grepl(" CO2", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate,
            color = climate_is_important,
            group = climate_is_important
        )
    ) +
    geom_point(
        position = position_dodge(width = 0.5), # Dodge points to avoid overlap
        size = 3
    ) +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        position = position_dodge(width = 0.5), # Dodge error bars to match points
        width = 0.2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis labels
        axis.title.x = element_text(size = 14) # Increase x-axis label size
    ) +
    ylab("WTP") +
    xlab(expression("A: " * CO[2] * "")) +
    scale_color_manual(
        values = c("red", "blue"),
        labels = c("No", "Yes"),
        name = "Control Climate Importance"
    )

### plot effect of consequence on WTP by if control for climate importance
plot_wtp_consequence <-
    wtp_tbl %>%
    filter(
        grepl("consequence", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate,
            color = climate_is_important,
            group = climate_is_important
        )
    ) +
    geom_point(
        position = position_dodge(width = 0.5), # Dodge points to avoid overlap
        size = 3
    ) +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        position = position_dodge(width = 0.5), # Dodge error bars to match points
        width = 0.2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis labels
        axis.title.x = element_text(size = 14) # Increase x-axis label size
    ) +
    ylab("WTP") +
    xlab("B: Consequence") +
    scale_color_manual(
        values = c("red", "blue"),
        labels = c("No", "Yes"),
        name = "Control Climate Importance"
    )

### plot effect of endorsement on WTP by if control for attitudes
plot_wtp_endorsement <-
    wtp_tbl %>%
    filter(
        grepl(" MetOffice| UN", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate,
            color = climate_is_important,
            group = climate_is_important
        )
    ) +
    geom_point(
        position = position_dodge(width = 0.5), # Dodge points to avoid overlap
        size = 3
    ) +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        position = position_dodge(width = 0.5), # Dodge error bars to match points
        width = 0.2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis labels
        axis.title.x = element_text(size = 14) # Increase x-axis label size
    ) +
    ylab("WTP") +
    xlab("C: Endorsement") +
    scale_color_manual(
        values = c("red", "blue"),
        labels = c("No", "Yes"),
        name = "Control Climate Importance"
    )

### combine WTP plots
ggarrange(
    plot_wtp_co2,
    plot_wtp_consequence,
    plot_wtp_endorsement,
    ncol = 1,
    nrow = 3
)

ggsave(
    "./plots/ce_wtp_with_ci.png",
    width = 8,
    height = 16
)

################################################################################
### Figure 5 with attitude
################################################################################

mlogit_ctrl_p1_12$coefficients["price"] <- mlogit_ctrl_p1_12$coefficients["price"] * -1
wtp_with_att <- wtp.gmnl(mlogit_ctrl_p1_12, wrt = "price")

### plot WTP for framing effect and co2 consumption
wtp_tbl <- wtp_no_att[, c(1, 2)] %>%
    as.data.frame() %>%
    mutate(
        lb = Estimate - 1.96 * `Std. Error`,
        ub = Estimate + 1.96 * `Std. Error`,
        with_att = FALSE,
        var = rownames(.)
    ) %>%
    rbind(
        wtp_with_att[, c(1, 2)] %>%
            as.data.frame() %>%
            mutate(
                lb = Estimate - 1.96 * `Std. Error`,
                ub = Estimate + 1.96 * `Std. Error`,
                with_att = TRUE,
                var = rownames(.)
            )
    )

### clean up variable names
wtp_tbl <-
    wtp_tbl %>%
    mutate(
        var = str_replace_all(var, "framing_effect", ""),
        var = str_replace_all(var, "co2", "CO2"),
        var = str_replace_all(var, "I", "Intercept"),
        var = str_replace_all(var, "project_", ""),
        var = str_replace_all(var, "_value", ""),
        var = str_replace_all(var, "_", " "),
        var = str_replace_all(var, "\\.", " * ")
    )

wtp_tbl$var <- factor(wtp_tbl$var, levels = unique(wtp_tbl$var))

### plot effect of CO2 on WTP by if control for attitudes
plot_wtp_co2 <-
    wtp_tbl %>%
    filter(
        grepl(" CO2", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate,
            color = with_att,
            group = with_att
        )
    ) +
    geom_point(
        position = position_dodge(width = 0.5), # Dodge points to avoid overlap
        size = 3
    ) +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        position = position_dodge(width = 0.5), # Dodge error bars to match points
        width = 0.2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis labels
        axis.title.x = element_text(size = 14) # Increase x-axis label size
    ) +
    ylab("WTP") +
    xlab(expression("A: " * CO[2] * "")) +
    scale_color_manual(
        values = c("red", "blue"),
        labels = c("No", "Yes"),
        name = "Control Attitude"
    )

### plot effect of consequence on WTP by if control for attitudes
plot_wtp_consequence <-
    wtp_tbl %>%
    filter(
        grepl(" consequence", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate,
            color = with_att,
            group = with_att
        )
    ) +
    geom_point(
        position = position_dodge(width = 0.5), # Dodge points to avoid overlap
        size = 3
    ) +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        position = position_dodge(width = 0.5), # Dodge error bars to match points
        width = 0.2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis labels
        axis.title.x = element_text(size = 14) # Increase x-axis label size
    ) +
    ylab("WTP") +
    xlab("B: Consequence") +
    scale_color_manual(
        values = c("red", "blue"),
        labels = c("No", "Yes"),
        name = "Control Attitude"
    )

### plot effect of endorsement on WTP by if control for attitudes
plot_wtp_endorsement <-
    wtp_tbl %>%
    filter(
        grepl(" MetOffice| UN", var)
    ) %>%
    ggplot(
        aes(
            x = var,
            y = Estimate,
            color = with_att,
            group = with_att
        )
    ) +
    geom_point(
        position = position_dodge(width = 0.5), # Dodge points to avoid overlap
        size = 3
    ) +
    geom_errorbar(
        aes(
            ymin = lb,
            ymax = ub
        ),
        position = position_dodge(width = 0.5), # Dodge error bars to match points
        width = 0.2,
        linetype = "solid",
        linewidth = 0.5
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1), # Rotate x-axis labels
        axis.title.x = element_text(size = 14) # Increase x-axis label size
    ) +
    ylab("WTP") +
    xlab("C: Endorsement") +
    scale_color_manual(
        values = c("red", "blue"),
        labels = c("No", "Yes"),
        name = "Control Attitude"
    )

### combine WTP plots
ggarrange(
    plot_wtp_co2,
    plot_wtp_consequence,
    plot_wtp_endorsement,
    ncol = 1,
    nrow = 3
)

ggsave(
    "./plots/ce_wtp_with_att.png",
    width = 8,
    height = 16
)

################################################################################
### Figure 6 attitude on co2 consumption
################################################################################
tbl <-
    summary_tbl %>%
    filter(
        !grepl("Intercept", term)
    ) %>%
    mutate(
        term = case_when(
            term == "Stronglydisagree" ~ "Strongly disagree",
            term == "Mildlydisagree" ~ "Mildly disagree",
            term == "Mildlyagree" ~ "Mildly agree",
            term == "Stronglyagree" ~ "Strongly agree"
        ),
        lb = estimate - 1.96 * std.error,
        ub = estimate + 1.96 * std.error,
        label = case_when(
            variable == "Q1" ~ "Humans have right to modify natural",
            variable == "Q2" ~ "Humans are abusing the planet",
            variable == "Q3" ~ "Plants and animals have the same rights as humans",
            variable == "Q4" ~ "Nature is strong enough",
            variable == "Q5" ~ "Humans should rule nature",
            variable == "Q6" ~ "Balance of nature is delicate",
        )
    ) %>%
    mutate(
        term = factor(
            term,
            levels = c(
                "Strongly disagree",
                "Mildly disagree",
                "Mildly agree",
                "Strongly agree"
            )
        )
    )

### plot the effect of each attitude
plots <-
    lapply(
        1:6,
        function(i) {
            q <- paste0("Q", i)
            label <- unique(
                tbl$label[tbl$variable == q]
            )
            label <- paste0(
                q,
                ": ",
                label
            )

            plot <-
                tbl %>%
                filter(variable == q) %>%
                ggplot(
                    aes(
                        x = term,
                        y = estimate
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
                # rotate x-axis labels 60 degrees and shift down not to overlap
                theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                ylab(expression(CO[2] * " Emission")) +
                xlab(label) +
                # increase x label size
                theme(axis.title.x = element_text(size = 14))

            return(plot)
        }
    )

### combine attitude plots
ggarrange(
    plots[[1]],
    plots[[2]],
    plots[[3]],
    plots[[4]],
    plots[[5]],
    plots[[6]],
    ncol = 2,
    nrow = 3
)

ggsave(
    "./plots/attitude_effects_on_co2.png",
    width = 8,
    height = 16
)