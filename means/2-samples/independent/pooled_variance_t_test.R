# You are comparing the means of two independent groups.
# You believe or want to assume the two populations have the same variance (homoscedasticity).
# Your sample sizes are smaller or you don't know the population standard deviations, making the t-test more appropriate than a Z-test.
rm(list = ls())

pooled_variance_t_test <- function(sample_data1 = NULL, sample_data2 = NULL, confidence_level, test_type = "two-tailed", mean1 = NA, mean2 = NA, sd1 = NA, sd2 = NA, n1 = NA, n2 = NA) {
    cat("--------------------\n")

    # Use provided means and standard deviations, or calculate them
    if (is.na(mean1)) mean1 <- mean(sample_data1)
    if (is.na(mean2)) mean2 <- mean(sample_data2)
    if (is.na(sd1)) sd1 <- sd(sample_data1)
    if (is.na(sd2)) sd2 <- sd(sample_data2)

    # Use provided sample sizes, or calculate them
    if (is.na(n1) && !is.null(sample_data1)) n1 <- length(sample_data1)
    if (is.na(n2) && !is.null(sample_data2)) n2 <- length(sample_data2)

    # Check if necessary parameters are provided
    if (is.na(n1) || is.na(n2)) {
        stop("Sample sizes (n1 and n2) must be provided or calculable from sample data.")
    }

    # Calculate the pooled standard deviation and standard error
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    standard_error <- pooled_sd * sqrt(1 / n1 + 1 / n2)


    # Calculate the t-value
    t_value <- (mean1 - mean2) / standard_error
    df <- n1 + n2 - 2

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

    # Use t.test for built-in calculation if sample data is provided
    if (!is.null(sample_data1) && !is.null(sample_data2)) {
        t_test_result <- t.test(sample_data1, sample_data2, var.equal = TRUE, conf.level = confidence_level, alternative = ifelse(test_type == "two-tailed", "two.sided", ifelse(test_type == "one-tailed-left", "less", "greater")))
    } else {
        t_test_result <- NA
    }

    # Decision making
    decision <- ifelse(p_value < alpha, "Reject the null hypothesis", "Do NOT reject the null hypothesis")

    cat("Decision:", decision, "\n")
    cat("--------------------\n")
    return(list(t_value = t_value, t_critical_lower = t_critical_lower, t_critical_upper = t_critical_upper, p_value = p_value, t_test_result = t_test_result, decision = decision))
}


#----------------------------------Pr1------------------------------------------
mean_cheesecake <- 780
mean_salad <- 1041
sd_cheesecake <- 128
sd_salad <- 140
sample_size <- 20
# H0: mu1 = mu2
# H1: mu1 <> mu2

# pooled_variance_t_test(mean1 = mean_cheesecake, mean2 = mean_salad, sd1 = sd_cheesecake, sd2 = sd_salad, n1 = sample_size, n2 = sample_size, confidence_level = 0.95, test_type = "two-tailed")


#----------------------------------Pr2------------------------------------------
library(readxl)
data <- read_excel("./data/Phone.xlsx")
confidence_interval <- 0.95
sample_size <- 20
data1 <- data$"Location 1"
data2 <- data$"Location 2"

# H0: mu1 = mu2
# H1: mu1 <> mu2

# pooled_variance_t_test(sample_data1 = data1, sample_data2 = data2, confidence_level = confidence_interval, test_type = "two-tailed")

#----------------------------------Pr3------------------------------------------
