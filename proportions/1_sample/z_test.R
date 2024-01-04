one_population_z_test <- function(successes, sample_size, claimed_proportion, confidence_level, test_type = "two-tailed") {
    sample_proportion <- successes / sample_size
    standard_error <- sqrt(claimed_proportion * (1 - claimed_proportion) / sample_size)
    z_value <- (sample_proportion - claimed_proportion) / standard_error
    alpha <- 1 - confidence_level

    # Initialize critical values
    z_critical_lower <- NA
    z_critical_upper <- NA

    # Adjust the critical value and p-value calculation based on the test type
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

    prop_test_result <- prop.test(x = successes, n = sample_size, p = claimed_proportion, conf.level = confidence_level, alternative = ifelse(test_type == "one-tailed-left", "less", ifelse(test_type == "one-tailed-right", "greater", "two.sided")), correct = FALSE)

    # Print results
    cat("Sample proportion:", sample_proportion, "\n")
    cat("Z-value:", z_value, "\n")
    cat("P-value:", p_value, "\n")
    cat("Critical Z-value(s):", ifelse(!is.na(z_critical_lower), paste("Lower:", z_critical_lower), ""), ifelse(!is.na(z_critical_upper), paste("Upper:", z_critical_upper), ""), "\n")
    cat("Prop.test X-squared:", prop_test_result$statistic, "\n")
    cat("Prop.test P-value:", prop_test_result$p.value, "\n")
    cat("Confidence interval:", paste(prop_test_result$conf.int[1], prop_test_result$conf.int[2]), "\n")

    # Decision based on the p-value and alpha
    decision <- ifelse(p_value < alpha, "Reject the null hypothesis", "Do NOT reject the null hypothesis")
    cat("Decision:", decision, "\n")
    cat("P-value comparison with alpha:", ifelse(p_value < alpha, "p-value < alpha", "p-value >= alpha"), "\n")

    return(list(z_value = z_value, z_critical = c(z_critical_lower, z_critical_upper), p_value = p_value, prop_test_result = prop_test_result, decision = decision))
}
