#----------------------------------Functions----------------------------------
perform_anova <- function(data, alpha = 0.05) {
    cat("--------------------------------\n")

    # Convert data to the correct format if it's a matrix or array
    if (is.matrix(data) || is.array(data)) {
        data <- data.frame(score = as.vector(data), group = rep(rownames(data), each = ncol(data)))
    }

    # Ensure the data is in the correct format
    if (!("score" %in% names(data)) || !("group" %in% names(data))) {
        stop("Data must have 'score' and 'group' columns")
    }

    # Calculate group means and grand mean
    group_means <- aggregate(score ~ group, data, mean)
    grand_mean <- mean(data$score)

    # Print the means
    print(group_means)
    cat("Grand Mean:", grand_mean, "\n")

    # Calculate Sum of Squares Total (SST) and Sum of Squares Within (SSW)
    SST <- sum((data$score - grand_mean)^2)
    SSW <- sum(sapply(split(data$score, data$group), function(x) sum((x - mean(x))^2)))

    # Print the tables of SST and SSW
    cat("Sum of Squares Total (SST):", SST, "\n")
    cat("Sum of Squares Within (SSW):", SSW, "\n")

    # Perform ANOVA and print the results
    anova_result <- aov(score ~ group, data = data)
    summary_anova <- summary(anova_result)
    print(summary_anova)

    # Calculate F-statistic manually
    SSB <- SST - SSW
    df_between <- length(unique(data$group)) - 1
    df_within <- nrow(data) - length(unique(data$group))
    MSB <- SSB / df_between
    MSW <- SSW / df_within
    F_statistic <- MSB / MSW

    # Calculate critical F-value
    F_critical <- qf(1 - alpha, df_between, df_within)

    # Print the F-statistic and critical F-value
    cat("Calculated F-statistic:", F_statistic, "\n")
    cat("Critical F-value at", alpha * 100, "% significance level:", F_critical, "\n")

    # Decision
    if (F_statistic > F_critical) {
        cat("Result: Reject the null hypothesis - there is a significant relationship.\n")
    } else {
        cat("Result: Fail to reject the null hypothesis - there is no significant relationship.\n")
    }

    cat("--------------------------------\n")
}



#----------------------------------Pr1------------------------------------------
# Is there (at the 0.05 level of significance) a relationship between the teaching method and the math grades students achieve?
# A(87 80 74 82 74 81 97 71)
# B(58 63 64 75 70 73 80 62)
# C(81 62 70 64 70 72 92 63)

# Data
scores_A <- c(87, 80, 74, 82, 74, 81, 97, 71)
scores_B <- c(58, 63, 64, 75, 70, 73, 80, 62)
scores_C <- c(81, 62, 70, 64, 70, 72, 92, 63)

# Combine the data
data <- data.frame(
    score = c(scores_A, scores_B, scores_C),
    group = factor(rep(c("A", "B", "C"), c(length(scores_A), length(scores_B), length(scores_C))))
)

# Hypothesis
# RQ: Is there (at the 0.05 level of significance) a relationship between the teaching method and the math grades students achieve?
# H0: m1 = m2 = m3
# H1: m1 <> m2 <> m3

# Perform ANOVA
perform_anova(data)
