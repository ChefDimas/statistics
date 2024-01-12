rm(list = ls())
t_test <- function(sample_data, population_mean, confidence_level, test_type = "one-tailed-lower", min_sample_size = 30) {
    cat("--------------------", "\n")
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

    # Initialize critical values
    t_critical_lower <- NA
    t_critical_upper <- NA

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
    cat("--------------------", "\n")
    return(list(
        manual_t_value = t_value_manual, manual_critical_t_value = c(t_critical_lower, t_critical_upper),
        manual_p_value = p_value_manual, manual_decision = manual_decision, comparison = comparison,
        t_test_result = if (sample_size >= min_sample_size) test_result else NULL
    ))
}

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

perform_chi_square_test <- function(observed, expected = NULL, significance_level = 0.05) {
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
    cat("Critical Chi-squared at", significance_level, "% significance:", chi_squared_critical, "\n")

    # Decision based on manual calculation
    if (chi_squared_statistic > chi_squared_critical) {
        cat("Manual Result: Reject the null hypothesis - there is a significant difference.\n")
    } else {
        cat("Manual Result: Fail to reject the null hypothesis - there is no significant difference.\n")
    }

    # Perform the built-in Chi-square test for confirmation
    chi_test_result <- chisq.test(observed, correct = FALSE)

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

    cat("Built-in t.test Result:\n")
    print(t_test_result)

    # Decision making
    decision <- ifelse(p_value < alpha, "Reject the null hypothesis", "Do NOT reject the null hypothesis")

    cat("Decision:", decision, "\n")
    cat("--------------------\n")
    return(list(t_value = t_value, t_critical_lower = t_critical_lower, t_critical_upper = t_critical_upper, p_value = p_value, t_test_result = t_test_result, decision = decision))
}


perform_anova <- function(data, significance_level = 0.05) {
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
    F_critical <- qf(1 - significance_level, df_between, df_within)

    # Print the F-statistic and critical F-value
    cat("Calculated F-statistic:", F_statistic, "\n")
    cat("Critical F-value at", significance_level, "% significance level:", F_critical, "\n")

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
df2 <- stack(df)
colnames(df2) <- c("score", "group")

significance_level <- 0.05
# RQ: Is there a difference amoung 4 plant box weights?
# H0: mu1 = mu2 = mu3 = mu4
# H1: mu1 <> mu2 <> mu3 <> mu4

# perform_anova(df2, significance_level)

# -------------------------------Problem 2--------------------------------
observed <- matrix(c(21, 28, 35, 38, 34, 22, 29, 37, 36, 41, 28, 17), nrow = 3, byrow = TRUE)

# Set the column and row names for clarity
colnames(observed) <- c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")
rownames(observed) <- c("Low", "Medium", "High")

# Hypothesis
# RQ: Is there a relationship between the quarter and the draft number?

# if p value is less than significance level, we reject the null hypothesis

# Perform the Chi-square test
# perform_chi_square_test(observed, significance_level = 0.5)


# -------------------------------Problem 3--------------------------------
library(readxl)
df <- read_excel("./data/CatFood.xlsx")
df2 <- stack(df)
colnames(df2) <- c("score", "group")
significance_level <- 0.05


# Hypothesis
# RQ: Is there a difference in the mean weight which cat consumed in the first 10 minutes gain among the 5 cat food ingredients?
# H0: mu1 = mu2 = mu3 = mu4 = mu5
# H1: mu1 <> mu2 <> mu3 <> mu4 <> mu5

# perform_anova(df2, significance_level = significance_level)


# -------------------------------Problem 4--------------------------------
library(readxl)
df <- read_excel("./data/LungCapData.xls")

# A
# Is there a dependency/association/relation between (Gender) and (Smoking) habits?

gender <- df$Gender
smoking <- df$Smoke
contingency_table <- table(gender, smoking)

# Hypothesis
# RQ:Is there a dependency/association/relation between (Gender) and (Smoking) habits?
# H0: m1 = m2
# H1: m1 <> m2

# perform_chi_square_test(contingency_table, significance_level = 0.05)

# B
# Check relations between all other factors
gender <- df$Gender
smoking <- df$Smoke
caesarean <- df$Caesarean

contingency_table <- table(gender, caesarean)
# perform_chi_square_test(contingency_table, significance_level = 0.05)

contingency_table <- table(caesarean, gender)
# perform_chi_square_test(contingency_table, significance_level = 0.05)

contingency_table <- table(smoking, caesarean)
# perform_chi_square_test(contingency_table, significance_level = 0.05)

lung_cap <- df$LungCap
age <- df$Age
height <- df$Height


# C
# Is there a difference in the mean lung capacity between smokers and non-smokers?
# H0: mu1 = mu2
# H1: mu1 <> mu2

smokers <- df$LungCap[df$Smoke == "yes"]
non_smokers <- df$LungCap[df$Smoke == "no"]
# pooled_variance_t_test(smokers, non_smokers, confidence_level = 0.95, test_type = "two-tailed")
# D
# Check the difference of LungCapbetween all other groups/factors

# Age
smokers <- df$Age[df$Smoke == "yes"]
non_smokers <- df$Age[df$Smoke == "no"]
# pooled_variance_t_test(smokers, age, confidence_level = 0.95, test_type = "two-tailed")

# Height
smokers <- df$Height[df$Smoke == "yes"]
non_smokers <- df$Height[df$Smoke == "no"]

# E
# Is there a linear relation between (LungCap) and (Age)? What is the formula? How good is the model?
model <- lm(LungCap ~ Age, data = df)
# print(summary(model))
# plot(df$Age, df$LungCap)
# abline(model, col = "blue")
# plot(model)

# F
# Can you enhance the model above by including (Age) as well? Explain?


# -------------------------------Problem 5--------------------------------
# The apraised value in thousands of dollars and land area was collected for a sample of 30
# single-family homes.Develop a simple linear regression model to predict the appraised value
# based on the land area.

library(readxl)
df <- read_excel("./data/GlenCove.xlsx")

# A
# Use the least-squares method to compute the regression coefficients b0 and b1.
# State the simple linear regression equation for predicting the appraised value based on the land area.
y <- df$"Fair Market Value($000)"
x <- df$"Property Size (acres)"
model <- lm(y ~ x, data = df)
summary <- summary(model)
print(summary)
b1 <- summary$coefficients[2, 1]
b0 <- summary$coefficients[1, 1]
# predict_value_equation <- b0 + b1 * land_area_value

# B
# Interpret the meaning of Y intercept, b0, and the slope, b1, in this problem.

# b0 - (354.99): The baseline appraised value when the land area is 0. It's the point where the regression line intersects the Y-axis.
# b1 - (434.54): The expected increase in the appraised value for each additional unit of land area. It's the slope of the regression line.

# C
# Explain why the regression coefficient b0 has no practical meaning in this problem.
#
# While b0 has a mathematical role in locating the regression line and is necessary
# for making predictions within the relevant range of your data, its value as the
# predicted appraised value for a land area of 0 doesn't make practical sense in the
# real world. In the context of property appraisal, there's no scenario where a house
# would have zero land area, making interpretations based on this value unrealistic.
# Instead, the focus is generally on the slope (b1) which provides a meaningful and
# practical interpretation of how changes in land area relate to changes in
# appraised value.

# D
# Predict the appraised value for a house with a land area of 0.25 acre.
predict_value_equation <- b0 + b1 * 0.25

# E
# Compute the coefficient of determination, r^2, and interpret its meaning.
r_squared <- summary(model)$r.squared
# print(r_squared)
# A higher r^2 value indicates a better fit between the model and the data.
# For instance, an r^2 of 0.7 suggests that 70% of the variability in the appraised
# value can be explained by the land area. However, a high r^2 doesn't necessarily
# mean the model is good; it must be interpreted in the context of the data and
# alongside other diagnostic measures.

# F
# Perform a residual analysis on your results and determine adequacy of the model.
residuals <- residuals(model)
# plot(fitted(model), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")
# abline(h = 0, col = "red")
# print(shapiro.test(residuals))
# if p value is less than significance level, residuals are not normally distributed

# G
# Determine whether a significant relationship exists between the appraised value and the land area at the 0.05 level of significance.
# H0: b1 = 0
# H1: b1 <> 0
# if p value is less than significance level, we reject the null hypothesis
# print(summary(model)$coefficients[, "Pr(>|t|)"][2])

# H
# Construct a 95% confidence interval estimate of the population slope, between the appraised value and the land area.
n <- length(x)
significance_level <- 0.05
mean <- mean(y)
sd <- sd(y)
# FOR REGRESSION
# wrong
# rez1 <- mean - qt(1 - significance_level / 2, n - 2) * sd / sqrt(n)
# rez2 <- mean + qt(1 - significance_level / 2, n - 2) * sd / sqrt(n)
