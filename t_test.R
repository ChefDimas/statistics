# t-tests are used when the population standard deviation is unknown and the sample size
# is < 30
#----------------------------------Functions----------------------------------
t_test <- function(sample_data, population_mean, confidence_level, test_type = "one-tailed-lower", min_sample_size = 30) {
    sample_size <- length(sample_data)
    sample_mean <- mean(sample_data)
    sample_sd <- sd(sample_data)

    # Calculate the standard error of the mean
    standard_error <- sample_sd / sqrt(sample_size)

    # Calculate the t-value manually
    t_value_manual <- (sample_mean - population_mean) / standard_error

    # Determine the degrees of freedom
    df <- sample_size - 1

    # Determine the alpha (significance level)
    alpha <- 1 - confidence_level

    # Determine the critical t-value and p-value based on the test type
    if (test_type == "two-tailed") {
        t_critical_lower <- qt(alpha / 2, df)
        t_critical_upper <- qt(1 - alpha / 2, df)
        p_value_manual <- 2 * pt(-abs(t_value_manual), df)
    } else if (test_type == "one-tailed-lower") {
        t_critical_lower <- qt(alpha, df)
        p_value_manual <- pt(t_value_manual, df)
    } else if (test_type == "one-tailed-upper") {
        t_critical_upper <- qt(1 - alpha, df)
        p_value_manual <- 1 - pt(t_value_manual, df)
    } else {
        stop("Invalid test type. Please specify 'two-tailed', 'one-tailed-lower', or 'one-tailed-upper'.")
    }

    # Print manual results
    cat("Manual t-value:", t_value_manual, "\n")
    cat("Manual critical t-value(s):", ifelse(exists("t_critical_lower"), t_critical_lower, ""), ifelse(exists("t_critical_upper"), t_critical_upper, ""), "\n")
    cat("Manual p-value:", p_value_manual, "\n")
    cat("Degrees of freedom:", df, "\n")
    cat("Significance level (alpha):", alpha, "\n")

    # Manual decision
    manual_decision <- "Not enough data to decide"
    if (sample_size >= 2) {
        if (test_type == "two-tailed") {
            if (abs(t_value_manual) > abs(t_critical_upper)) {
                manual_decision <- "Reject the null hypothesis"
            } else {
                manual_decision <- "Do NOT reject the null hypothesis"
            }
        } else if (test_type == "one-tailed-lower" && t_value_manual < t_critical_lower) {
            manual_decision <- "Reject the null hypothesis"
        } else if (test_type == "one-tailed-upper" && t_value_manual > t_critical_upper) {
            manual_decision <- "Reject the null hypothesis"
        } else {
            manual_decision <- "Do NOT reject the null hypothesis"
        }
    }
    cat("Manual decision:", manual_decision, "\n")

    # Comparison of alpha and p-value
    comparison <- ifelse(p_value_manual < alpha, "p-value < alpha: Reject the null hypothesis", "p-value >= alpha: Do NOT reject the null hypothesis")
    cat("Comparison (alpha vs p-value):", comparison, "\n")

    # Only perform the t.test() if there are enough observations
    if (sample_size >= min_sample_size) {
        test_result <- t.test(
            x = sample_data, mu = population_mean, conf.level = confidence_level,
            alternative = ifelse(test_type == "two-tailed", "two.sided", ifelse(test_type == "one-tailed-lower", "less", "greater"))
        )

        # Print t.test() results
        cat("t.test t-value:", test_result$statistic, "\n")
        cat("p-value from t.test:", test_result$p.value, "\n")

        # t.test() decision
        t_test_decision <- ifelse(test_result$p.value < alpha, "Reject the null hypothesis", "Do NOT reject the null hypothesis")
        cat("t.test decision:", t_test_decision, "\n")
    } else {
        cat("Not enough observations for t.test(). At least", min_sample_size, "required, but only", sample_size, "provided.\n")
    }

    return(list(
        manual_t_value = t_value_manual, manual_critical_t_value = c(t_critical_lower, t_critical_upper),
        manual_p_value = p_value_manual, manual_decision = manual_decision, comparison = comparison,
        t_test_result = if (sample_size >= min_sample_size) test_result else NULL
    ))
}


#----------------------------------Pr1------------------------------------------
# It is believed that College students get less than seven hours of sleep per night,
# on average. A survey of 22 College students generated a mean of 7.24 hours with a
# standard deviation of 1.93 hours. At a level of significance of 5%,
# do College students get less than seven hours of sleep per night, on average?

# Data
n <- 22
average <- 7.24
population_mean <- 7
sd <- 1.93
sample_data <- rnorm(n = 22, mean = 7.24, sd = 1.93)
alpha <- 0.05

# Hypothesis
# RQ: Is the mean less than 7?
# H0: mean = 7
# H1: mean < 7
# Test statistic: T
# If T < T_lower => reject H0

# Test
# t_test(sample_mean = 7.24, sample_size = 22, population_mean = 7, sample_sd = 1.93, confidence_level = 0.95, test_type = "one-tailed-lower")


#----------------------------------Pr2------------------------------------------
# The mean number of sick days an employee takes per year is believed to be about ten.
# Members of a personnel department do not believe this figure.
# They randomly survey eight employees.
# The number of sick days they took for the past year are as follows: 12; 4; 15; 3; 11; 8; 6; 8.
# Should the personnel team believe that the mean number is ten?

# Data
n <- 8
average <- mean(c(12, 4, 15, 3, 11, 8, 6, 8))
population_mean <- 10
sd <- sd(c(12, 4, 15, 3, 11, 8, 6, 8))
sample_data <- c(12, 4, 15, 3, 11, 8, 6, 8)
alpha <- 0.05

# Hypothesis
# RQ: Is the mean different from 10?
# H0: mean = 10
# H1: mean <> 10
# Test statistic: T
# If T < T_lower or T > T_upper => reject H0

# Test
t_test(sample_data = sample_data, population_mean = population_mean, confidence_level = 0.95, test_type = "two-tailed")
