#----------------------------------Functions----------------------------------
two_proportion_z_test <- function(successes1, sample_size1, successes2, sample_size2, confidence_level, test_type = "two-tailed") {
    cat("--------------------\n")
    # Calculate sample proportions and their difference
    p1 <- successes1 / sample_size1
    p2 <- successes2 / sample_size2
    p_diff <- p1 - p2

    # Calculate the pooled proportion and standard error
    pooled_p <- (successes1 + successes2) / (sample_size1 + sample_size2)
    standard_error <- sqrt(pooled_p * (1 - pooled_p) * (1 / sample_size1 + 1 / sample_size2))

    # Calculate the z-value
    z_value <- p_diff / standard_error

    # Determine the critical z-value and p-value
    alpha <- 1 - confidence_level
    z_critical_lower <- NA
    z_critical_upper <- NA
    if (test_type == "two-tailed") {
        z_critical_lower <- qnorm(alpha / 2)
        z_critical_upper <- qnorm(1 - alpha / 2)
        p_value <- 2 * (1 - pnorm(abs(z_value)))
    } else if (test_type == "one-tailed-left") {
        z_critical_lower <- qnorm(alpha)
        p_value <- pnorm(z_value)
    } else if (test_type == "one-tailed-right") {
        z_critical_upper <- qnorm(1 - alpha)
        p_value <- 1 - pnorm(z_value)
    } else {
        stop("Invalid test type. Please specify 'two-tailed', 'one-tailed-left', or 'one-tailed-right'.")
    }

    # Print the results
    cat("Z-value:", z_value, "\n")
    cat("Critical Z-value Lower (if applicable):", z_critical_lower, "\n")
    cat("Critical Z-value Upper (if applicable):", z_critical_upper, "\n")
    cat("P-value:", p_value, "\n")

    # Use prop.test for built-in calculation
    prop_test_result <- prop.test(c(successes1, successes2), c(sample_size1, sample_size2), conf.level = confidence_level, correct = FALSE)

    # Decision making
    decision <- ifelse(p_value < alpha, "Reject the null hypothesis", "Do NOT reject the null hypothesis")

    cat("Decision:", decision, "\n")
    cat("--------------------\n")
    return(list(z_value = z_value, z_critical_lower = z_critical_lower, z_critical_upper = z_critical_upper, p_value = p_value, prop_test_result = prop_test_result, decision = decision))
}




pooled_variance_t_test <- function(sample_data1, sample_data2, confidence_level, test_type = "two-tailed") {
    cat("--------------------\n")
    # Calculate sample means and standard deviations
    mean1 <- mean(sample_data1)
    mean2 <- mean(sample_data2)
    sd1 <- sd(sample_data1)
    sd2 <- sd(sample_data2)

    # Calculate the pooled standard deviation and standard error
    n1 <- length(sample_data1)
    n2 <- length(sample_data2)
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

    # Use t.test for built-in calculation
    t_test_result <- t.test(sample_data1, sample_data2, var.equal = TRUE, conf.level = confidence_level, alternative = ifelse(test_type == "two-tailed", "two.sided", "less"))

    # Decision making
    decision <- ifelse(p_value < alpha, "Reject the null hypothesis", "Do NOT reject the null hypothesis")

    cat("Decision:", decision, "\n")
    cat("--------------------\n")
    return(list(t_value = t_value, t_critical_lower = t_critical_lower, t_critical_upper = t_critical_upper, p_value = p_value, t_test_result = t_test_result, decision = decision))
}




paired_t_test <- function(sample_data1, sample_data2, confidence_level, test_type = "two-tailed") {
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
# Old design clicked: 351, not clicked: 3291
# New design clicked: 451, not clicked: 3105
# Is there a difference between the two designs?

# Data
successes1 <- 351
sample_size1 <- 3291 + 351
successes2 <- 451
sample_size2 <- 3105 + 451
confidence_level <- 0.95

# Hypothesis
# RQ: Is there a difference between the two designs?
# H0: p1 - p2 = 0
# H1: p1 - p2 <> 0
# Test type: Z-test for two proportions

# Test
# two_proportion_z_test(successes1, sample_size1, successes2, sample_size2, confidence_level, test_type = "two-tailed")


#----------------------------------Pr2------------------------------------------
# Old design clicked: 351, not clicked: 3291
# New design clicked: 451, not clicked: 3105
# Is the old design worse than the new design?

# Data
successes1 <- 351
sample_size1 <- 3291 + 351
successes2 <- 451
sample_size2 <- 3105 + 451
confidence_level <- 0.95

# Hypothesis
# RQ: Is the old design worse than the new design?
# H0: p1 - p2 >= 0
# H1: p1 - p2 < 0

# Test
# two_proportion_z_test(successes1, sample_size1, successes2, sample_size2, confidence_level, test_type = "one-tailed-left")


#----------------------------------Pr3------------------------------------------
# Aspirin: hearth attack: 104, no hearth attack: 10933
# Placebo: hearth attack: 189, no hearth attack: 10845
# Is there a difference in the proportion of Heart Attacks (at a significance level of 1%) between both groups?

# Data
successes1 <- 104
sample_size1 <- 10933 + 104
successes2 <- 189
sample_size2 <- 10845 + 189
confidence_level <- 0.99

# Hypothesis
# RQ: Is there a difference in the proportion of Heart Attacks (at a significance level of 1%) between both groups?
# H0: p1 - p2 = 0
# H1: p1 - p2 <> 0

# Test
# two_proportion_z_test(successes1, sample_size1, successes2, sample_size2, confidence_level, test_type = "two-tailed")


# Is the proportion of Heart Attacks (at a significance level of one-in-a-million!)
# lower in the Aspirin group than in the placebo group?

# Data
successes1 <- 104
sample_size1 <- 10933 + 104
successes2 <- 189
sample_size2 <- 10845 + 189
confidence_level <- 0.999999

# Hypothesis
# RQ: Is the proportion of Heart Attacks (at a significance level of one-in-a-million!) lower in the Aspirin group than in the placebo group?
# H0: p1 - p2 >= 0
# H1: p1 - p2 < 0

# Test
# two_proportion_z_test(successes1, sample_size1, successes2, sample_size2, confidence_level, test_type = "one-tailed-left")
# Alternatively we can use R:
# prop.test(c(104, 189), c(11037, 11034), conf.level = 1 - 1e-6, alternative = "less", correct = F)
