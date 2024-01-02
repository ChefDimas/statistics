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


#----------------------------------Pr4------------------------------------------
# Do the prices for meals differ between city and suburbs?
library(readxl)
df <- read_excel("data/Restaurants.xlsx")

# Data
location <- df$Location
price <- df$Cost

# Hypothesis
# RQ: Do the prices for meals differ between city and suburbs?
# H0: mu1 - mu2 = 0
# H1: mu1 - mu2 <> 0

# Test
# pooled_variance_t_test(sample_data1 = price[location == "City"], sample_data2 = price[location == "Suburban"], confidence_level = 0.95, test_type = "two-tailed")
# Alternatively we can use R:
# t.test (X1, X2, paired=F, var.equal=T)


#----------------------------------Pr5------------------------------------------
# Is there a difference at a significance level of 1% between the rating of TV service
# & Internet service for various providers?

# Data
library(readxl)
df <- read_excel("data/Telecom.xlsx")


# Hypothesis
# RQ: Is there a difference at a significance level of 1% between the rating of TV service & Internet service for various providers?
# H0: mu1 - mu2 = 0
# H1: mu1 - mu2 <> 0

# Test
# paired_t_test(sample_data1 = df$TV, sample_data2 = df$Internet, confidence_level = 0.99, test_type = "two-tailed")


#----------------------------------Pr6------------------------------------------
# The Concrete file contains data that represent the compressive strength, in thousands of
# pounds per square inch (psi), of 40 samples of concrete taken two and seven days after
# pouring.
# At the 0.01 level of significance, is there evidence that the mean strength is lower at
# two days than at seven days? Data file: Concrete.xls

# Data
library(readxl)
df <- read_excel("data/Concrete.xlsx")

# Hypothesis
# RQ: At the 0.01 level of significance, is there evidence that the mean strength is lower at two days than at seven days?
# H0: mu1 - mu2 = 0
# H1: mu1 < mu2

# Test
# paired_t_test(sample_data1 = df$"Two days", sample_data2 = df$"Seven days", confidence_level = 0.99, test_type = "one-tailed-left")
# print(t.test(df$"Two days", df$"Seven days", paired = TRUE, conf.level = 0.99, alternative = "less"))

#----------------------------------Pr7------------------------------------------
# A problem with a telephone line that prevents a customer from receiving or making calls is upsetting to both the customer and the telephone company. The following data represent samples of 20 problems reported to two different offices of a telephone company and the time to clear these problems (in minutes) from the customers’ lines:
# Central Office I Time to Clear Problems (minutes)
# 1.48 1.75 0.78 2.85 0.52 1.60 4.15 3.97 1.48 3.10 1.02 0.53 0.93 1.60 0.80 1.05 6.32 3.93 5.45 0.97
# Central Office II Time to Clear Problems (minutes)
# 7.55 3.75 0.10 1.10 0.60 0.52 3.30 2.10 0.58 4.02 3.75 0.65 1.92 0.60 1.53 4.23 0.08 1.48 1.65 0.72
# Is there evidence of a difference in the mean waiting time between the two offices? (Use α = 0.05)

# Data
central_office1 <- c(1.48, 1.75, 0.78, 2.85, 0.52, 1.60, 4.15, 3.97, 1.48, 3.10, 1.02, 0.53, 0.93, 1.60, 0.80, 1.05, 6.32, 3.93, 5.45, 0.97)
central_office2 <- c(7.55, 3.75, 0.10, 1.10, 0.60, 0.52, 3.30, 2.10, 0.58, 4.02, 3.75, 0.65, 1.92, 0.60, 1.53, 4.23, 0.08, 1.48, 1.65, 0.72)

# Hypothesis
# RQ: Is there evidence of a difference in the mean waiting time between the two offices?
# H0: mu1 - mu2 = 0
# H1: mu1 <> mu2

# Test
# pooled_variance_t_test(sample_data1 = central_office1, sample_data2 = central_office2, confidence_level = 0.95, test_type = "two-tailed")


#----------------------------------Pr8------------------------------------------
# Technology has led to the rise of extreme workers who are on the job 60 hours or more per week. One of the reasons cited by employees as to why they worked long hours was that they loved their job because it is stimulating/challenging/provides an adrenaline rush.
# Suppose that the survey of 1,564 workaholics included 786 men and 778 women, and the
# results showed that 707 men and 638 women loved their job because it is stimulating.
# At the 0.05 level of significance, is the proportion of workaholic men who love their
# job because it is stimulating different from the proportion of women?

# Data
successes1 <- 707
sample_size1 <- 786
successes2 <- 638
sample_size2 <- 778
confidence_level <- 0.95

# Hypothesis
# H0: p1 - p2 = 0
# H1: p1 - p2 <> 0

# Test
# two_proportion_z_test(successes1, sample_size1, successes2, sample_size2, confidence_level, test_type = "two-tailed")


#----------------------------------Pr9------------------------------------------
# You would like to determine at a level of significance of α = 0.05, whether the mean
# surface hardness of steel intaglio printing plates prepared using a new treatment
# differs from the mean hardness of plates that are untreated. The following results
# are from an experiment in which 40 steel plates, 20 treated and 20 untreated,
# were tested for surface hardness. Data file: Intaglio.xls

# Data
library(readxl)
df <- read_excel("data/Intaglio.xlsx")

# Hypothesis
# H0: mu1 - mu2 = 0
# H1: mu1 <> mu2

# Test
pooled_variance_t_test(sample_data1 = df$Treated, sample_data2 = df$Untreated, confidence_level = 0.95, test_type = "two-tailed")
