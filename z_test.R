install.packages("BSDA")
library(BSDA)

#----------------------------------Functions----------------------------------
z_test <- function(sample_mean, sample_size, population_mean, population_sd, confidence_level, test_type = "two-tailed") {
    # Calculate the standard error of the mean
    standard_error <- population_sd / sqrt(sample_size)

    # Calculate the z-value
    z_value <- (sample_mean - population_mean) / standard_error

    # Determine the alpha and critical z-value based on the test type
    if (test_type == "two-tailed") {
        alpha <- 1 - confidence_level
        z_critical <- qnorm(1 - alpha / 2) # Critical z-value for two-tailed test
    } else { # one-tailed tests
        alpha <- (1 - confidence_level)
        if (test_type == "one-tailed-lower") {
            z_critical <- qnorm(1 - alpha) # Critical z-value for lower tail one-tailed test
        } else if (test_type == "one-tailed-upper") {
            z_critical <- qnorm(1 - alpha) # Critical z-value for upper tail one-tailed test
        } else {
            stop("Invalid test type. Please specify 'two-tailed', 'one-tailed-lower', or 'one-tailed-upper'.")
        }
    }

    # Print the z-value and critical z-value
    cat("Z-value:", z_value, "\n")
    cat("Critical Z-value:", z_critical, "\n")

    # Determine whether to reject the null hypothesis based on the test type and z-value
    if (test_type == "two-tailed") {
        if (abs(z_value) > z_critical) {
            cat("Reject the null hypothesis: the mean is significantly different from", population_mean, "\n")
        } else {
            cat("Do NOT reject the null hypothesis: no significant difference from the mean of", population_mean, "\n")
        }
    } else {
        if ((test_type == "one-tailed-lower" && z_value < -z_critical) || (test_type == "one-tailed-upper" && z_value > z_critical)) {
            cat("Reject the null hypothesis: the mean is significantly different as per the one-tailed test\n")
        } else {
            cat("Do NOT reject the null hypothesis: no significant difference as per the one-tailed test\n")
        }
    }

    return(list(z_value = z_value, z_critical = z_critical))
}





#----------------------------------Pr1------------------------------------------

# IQ is normally distributed with historical mean=100 and sd=12. A random sample of 36
# people take the IQ test and the average score of that sample was 94. With a 95% confidence,
# test the hypothesis that the mean now different from 100!

# Data
n <- 36
sample_mean <- 94
population_mean <- 100
sd <- 12
alpha <- 0.05

# Hypothesis
# RQ: Is the mean different from 100?
# H0: mean = 100
# H1: mean <> 100
# Test statistic: Z
# If Z < Z_lower or Z > Z_upper => reject H0

# Test
# z_test(sample_mean = sample_mean, sample_size = n, population_mean = population_mean, population_sd = sd, confidence_level = 0.95, test_type = "two-tailed")


#----------------------------------Pr2------------------------------------------
# IQ is normally distributed with historical mean=100 and sd=12
# A random sample of 36 people take the IQ test and the average score of that sample was 97.
# With a 95% confidence, test the hypothesis that the mean now is less than 100!

# Data
n <- 36
sample_mean <- 97
population_mean <- 100
sd <- 12
alpha <- 0.05

# Hypothesis
# RQ: Is the mean less than 100?
# H0: mean = 100
# H1: mean < 100
# Test statistic: Z
# If Z < Z_lower => reject H0

# Test
# z_test(sample_mean = sample_mean, sample_size = n, population_mean = population_mean, population_sd = sd, confidence_level = 0.95, test_type = "one-tailed-lower")


#----------------------------------Pr3------------------------------------------
# IQ is normally distributed with historical mean=100 and sd=12
# A random sample of 36 people take the IQ test and the average score of that sample was 97.
# With a 90% confidence, test the hypothesis that the mean now is less than 100!

# Data
n <- 36
sample_mean <- 97
population_mean <- 100
sd <- 12
alpha <- 0.1

# Hypothesis
# RQ: Is the mean less than 100?
# H0: mean = 100
# H1: mean < 100
# Test statistic: Z
# If Z < Z_lower => reject H0

# Test
# z_test(sample_mean = sample_mean, sample_size = n, population_mean = population_mean, population_sd = sd, confidence_level = 0.9, test_type = "one-tailed-lower")
