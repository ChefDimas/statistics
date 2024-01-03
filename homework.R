perform_chi_square_test <- function(observed, expected = NULL, significance_level = 0.05, correct = FALSE) {
    cat("--------------------------------\n")

    # If expected values are not provided, calculate them
    if (is.null(expected)) {
        # Row totals
        row_totals <- apply(observed, 1, sum)
        # Column totals
        col_totals <- apply(observed, 2, sum)
        # Total number of observations
        total <- sum(row_totals)
        # Calculate expected frequencies
        expected <- outer(row_totals, col_totals) / total
        cat("================================", "\n")
        print("Expected Frequencies:")
        print(expected)
        cat("================================", "\n")
    }

    # Calculate the Chi-square statistic manually
    chi_squared_statistic <- sum((observed - expected)^2 / expected)

    # Degrees of freedom
    df <- (nrow(observed) - 1) * (ncol(observed) - 1)

    # Calculate the critical Chi-square value
    chi_squared_critical <- qchisq(1 - significance_level, df)

    # Print the manual calculation results
    cat("Degrees of Freedom:", df, "\n")
    cat("Manual Chi-squared Statistic:", chi_squared_statistic, "\n")
    cat("Critical Chi-squared at", significance_level * 100, "% significance:", chi_squared_critical, "\n")

    # Decision based on manual calculation
    if (chi_squared_statistic > chi_squared_critical) {
        cat("Manual Result: Reject the null hypothesis - there is a significant difference.\n")
    } else {
        cat("Manual Result: Fail to reject the null hypothesis - there is no significant difference.\n")
    }

    # Perform the built-in Chi-square test for confirmation
    chi_test_result <- chisq.test(observed, correct)

    # Print the built-in test results
    cat("\nBuilt-in Chi-squared Test Result:\n")
    print(chi_test_result)

    cat("--------------------------------\n")

    # Returning the manual and built-in test results for further use if needed
    return(list(
        manual_chi_squared_statistic = chi_squared_statistic,
        manual_chi_squared_critical = chi_squared_critical,
        built_in_chi_squared_test = chi_test_result
    ))
}
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


# -------------------------------Problem 1--------------------------------
library(readxl)
df <- read_excel("./data/BoxFills.xlsx")

# can simplify the df creation?
df2 <- data.frame(score = c(df$"Plant 1", df$"Plant 2", df$"Plant 3", df$"Plant 4"), group = rep(group, each = length(df$"Plant 1")))




significance_level <- 0.05
# RQ: Is there a difference amoung 4 plant box weights?
# H0: mu1 = mu2 = mu3 = mu4
# H1: mu1 <> mu2 <> mu3 <> mu4

perform_anova(df2, significance_level)

# -------------------------------Problem 2--------------------------------
observed <- matrix(
    c(
        21, 28, 35, 38,
        34, 22, 29, 37,
        36, 41, 28, 17
    ),
    nrow = 3, byrow = TRUE
)

# Set the column and row names for clarity
colnames(observed) <- c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")
rownames(observed) <- c("Low", "Medium", "High")

# Hypothesis
# RQ: Is there a relationship between the quarter and the draft number?

# if p value is less than significance level, we reject the null hypothesis

# Perform the Chi-square test
perform_chi_square_test(observed, significance_level = 0.5)
