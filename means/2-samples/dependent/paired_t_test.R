# You are comparing the means of two related groups.
# This could mean the same individuals measured at two different times or two groups with matched subjects.
# The data are paired, meaning each data point in one group is related to one and only one data point in the second group.
rm(list = ls())

paired_t_test <- function(sample_data1, sample_data2, confidence_level, test_type = "two-tailed", correct = FALSE) {
    cat("--------------------\n")
    # Calculate differences
    differences <- sample_data1 - sample_data2

    # Calculate mean and standard deviation of differences
    mean_diff <- mean(differences)
    sd_diff <- sd(differences)
    n <- length(differences)

    # Calculate the standard error and t-value
    standard_error <- sd_diff / sqrt(n)
    t_value <- mean_diff / standard_error
    df <- n - 1

    # Determine the critical t-value and p-value
    alpha <- 1 - confidence_level
    t_critical_lower <- NA
    t_critical_upper <- NA
    if (test_type == "two-tailed") {
        t_critical_lower <- qt(alpha / 2, df)
        t_critical_upper <- qt(1 - alpha / 2, df)
        p_value <- 2 * pt(-abs(t_value), df)
    } else if (test_type == "one-tailed-left") {
        t_critical_lower <- qt(alpha, df)
        p_value <- pt(t_value, df)
    } else if (test_type == "one-tailed-right") {
        t_critical_upper <- qt(1 - alpha, df)
        p_value <- 1 - pt(t_value, df)
    } else {
        stop("Invalid test type. Please specify 'two-tailed', 'one-tailed-left', or 'one-tailed-right'.")
    }

    # Print the results
    cat("T-value:", t_value, "\n")
    cat("Critical T-value Lower (if applicable):", t_critical_lower, "\n")
    cat("Critical T-value Upper (if applicable):", t_critical_upper, "\n")
    cat("P-value:", p_value, "\n")

    # Use t.test for built-in calculation
    t_test_result <- t.test(sample_data1, sample_data2, paired = TRUE, conf.level = confidence_level, alternative = ifelse(test_type == "two-tailed", "two.sided", "less"))

    # Decision making
    decision <- ifelse(p_value < alpha, "Reject the null hypothesis", "Do NOT reject the null hypothesis")

    cat("Decision:", decision, "\n")
    cat("--------------------\n")
    return(list(t_value = t_value, t_critical_lower = t_critical_lower, t_critical_upper = t_critical_upper, p_value = p_value, t_test_result = t_test_result, decision = decision))
}

#----------------------------------Pr1------------------------------------------
library(readxl)
data <- read_excel("./data/Intaglio.xlsx")
confidence_level <- 0.95
treated <- data$Treated
untrated <- data$Untreated

# paired_t_test(untrated, treated, confidence_level = confidence_level, "two-tailed")

#----------------------------------Pr2------------------------------------------

library(readxl)
data <- read_excel("./data/TargetWalmart.xlsx")
confidence_interval <- 0.95
sample_size <- 33
data1 <- data$"Target"
data2 <- data$"Walmart"

# paired_t_test(data1, data2, confidence_level = confidence_interval, test_type = "two-tailed")

#----------------------------------Pr3------------------------------------------
library(readxl)
data <- read_excel("./data/Myeloma.xlsx")
confidence_interval <- 0.95
sample_size <- 7
data1 <- data$"Before"
data2 <- data$"After"

# paired_t_test(data1, data2, confidence_level = confidence_interval, test_type = "two-tailed")

#----------------------------------Pr4------------------------------------------
library(readxl)
data <- read_excel("./data/Concrete.xlsx")
confidence_interval <- 0.95
sample_size <- 40
data1 <- data$"Two days"
data2 <- data$"Seven days"

# paired_t_test(data1, data2, confidence_level = confidence_interval, test_type = "two-tailed")
