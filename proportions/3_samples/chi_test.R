#----------------------------------Functions----------------------------------
# use this function when you want to test the independence between two categorical variables.
# For example, if you want to test if there's a significant association between a
# treatment (like Aspirin vs. Placebo) and an outcome (like having a heart attack or not).
# This function is particularly useful when dealing with 2x2 contingency tables,
# but can be extended to larger tables as well.
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



#----------------------------------Pr1------------------------------------------
# at a significance level of 5%, is there a difference in the proportion of heart attacks between the two groups (Aspirin & Placebo)?
# Observed values:
# Aspirin: 104(Yes) 10933(No)
# Placebo: 189(Yes) 10845 (No)
# Expected values:
# Aspirin: 146.5(Yes) 10890.5(No)
# Placebo: 146.5(Yes) 10887.5 (No)

# Data
observed <- as.table(rbind(c(104, 10933), c(189, 10845)))
expected <- as.table(rbind(c(146.5, 10890.5), c(146.5, 10887.5)))
dimnames(observed) <- list(c("Aspirin", "Placebo"), c("HeartAttack_Yes", "HeartAttack_No"))
dimnames(expected) <- list(c("Aspirin", "Placebo"), c("HeartAttack_Yes", "HeartAttack_No"))
# If p value is less than significance level, we reject the null hypothesis
significance_level <- 0.05

# Hypothesis test
# H0: p1 = p2
# H1: p1 <> p2

# Chi-square test
# perform_chi_square_test(observed, expected, significance_level = significance_level)


#----------------------------------Pr2------------------------------------------
# At a significance level of 5%, is there a difference in order accuracy
# (proportion of filled correctly) between the different fast food chains?
# Burger King: 203(Yes) 43(No)
# Wendy's: 245(Yes) 37(No)
# McDonald's: 247(Yes) 33(No)

# Data
observed <- as.table(rbind(c(203, 43), c(245, 37), c(247, 33)))
dimnames(observed) <- list(c("BurgerKing", "Wendy's", "McDonald's"), c("OrderAccuracy_Yes", "OrderAccuracy_No"))
significance_level <- 0.05

# Hypothesis test
# H0: p1 = p2 = p3
# H1: At least one of the proportions is different

# Chi-square test
# perform_chi_square_test(observed, significance_level = significance_level)


#----------------------------------Pr3------------------------------------------
# Is there (at the 0.05 level of significance) a relationship between the type of dessert ordered and the type of entree ordered by the customers of this restaurant?
# Observed values:
# Entree: Beef Poultry Fish Pasta
# Dessert:Ice Cream  13 8 12 14
# Dessert:Cake 98 12 29 6
# Dessert:Fruit 8 10 6 2
# Dessert:None 124 98 149 41

# Data
observed <- as.table(rbind(c(13, 8, 12, 14), c(98, 12, 29, 6), c(8, 10, 6, 2), c(124, 98, 149, 41)))
dimnames(observed) <- list(c("IceCream", "Cake", "Fruit", "None"), c("Beef", "Poultry", "Fish", "Pasta"))

# Hypothesis test
# H0: p1 = p2 = p3 = p4
# H1: p1 <> p2 <> p3 <> p4

# Chi-square test
# perform_chi_square_test(observed, significance_level = significance_level, correct = TRUE)


#----------------------------------Pr4------------------------------------------
insurance <- c(40, 160)
phramacy <- c(80, 120)
research <- c(90, 110)

data <- as.table(rbind(insurance, phramacy, research))
dimnames(data) <- list(c("Insurance", "Phramacy", "Research"), c("Yes", "No"))
# print(data)

significance_level <- 0.05

# Hypothesis test
# H0: p1 = p2 = p3
# H1: At least one of the proportions is different


# Chi-square test
# perform_chi_square_test(data, significance_level = significance_level)


#----------------------------------Pr5------------------------------------------
low <- c(21, 28, 35, 38)
medium <- c(34, 22, 29, 37)
high <- c(36, 41, 28, 17)

data <- as.table(rbind(low, medium, high))
dimnames(data) <- list(c("Low", "Medium", "High"), c("IQ", "IIQ", "IIIQ", "IVQ"))
significance_level <- 0.05

# Hypothesis test
# H0: p1 = p2 = p3
# H1: At least one of the proportions is different

# Chi-square test
# perform_chi_square_test(data, significance_level = significance_level)



#----------------------------------Pr4------------------------------------------
# Data
library(readxl)
data <- read_excel("./data/LungCapData.xls")


new_data <- table(data$Smoke, data$Gender)
print(new_data)

significance_level <- 0.05

# Hypothesis
# RQ: Is a dependency between gender and smoking status?
# H0: m1 = m2
# H1: m1 <> m2

# Perform Chi-square test
# perform_chi_square_test(new_data, significance_level = significance_level)
