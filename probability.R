#----------------------------------Functions----------------------------------

create_contingency_table <- function(data, row_labels, col_labels) {
  if (length(row_labels) != length(data) || length(col_labels) != length(data[[1]])) {
    stop("The number of row and column labels must match the dimensions of the data.")
  }

  # Combine data into a matrix
  table <- matrix(unlist(data), nrow = length(data), byrow = TRUE)
  rownames(table) <- row_labels
  colnames(table) <- col_labels

  return(table)
}

calculate_probabilities <- function(table) {
  total_respondents <- sum(table)

  # Calculate probabilities while maintaining the matrix structure
  probability_matrix <- table / total_respondents

  return(probability_matrix)
}




get_probability <- function(prob_matrix, condition1, condition2 = NULL) {
  if (!is.null(condition2)) {
    # If two conditions are provided, proceed as before
    if (condition1 %in% rownames(prob_matrix) && condition2 %in% colnames(prob_matrix)) {
      return(prob_matrix[condition1, condition2])
    } else if (condition1 %in% colnames(prob_matrix) && condition2 %in% rownames(prob_matrix)) {
      return(prob_matrix[condition2, condition1])
    } else {
      stop("Invalid conditions. They must match the row or column names of the probability matrix.")
    }
  } else {
    # If only one condition is provided
    if (condition1 %in% rownames(prob_matrix)) {
      # Sum probabilities across all columns for the given row
      return(sum(prob_matrix[condition1, ]))
    } else if (condition1 %in% colnames(prob_matrix)) {
      # Sum probabilities across all rows for the given column
      return(sum(prob_matrix[, condition1]))
    } else {
      stop("Invalid condition. It must match a row or column name of the probability matrix.")
    }
  }
}



conditional_probability <- function(table, given, target) {
  # Check if given and target are valid
  if (!(given %in% rownames(table) || given %in% colnames(table)) ||
    !(target %in% rownames(table) || target %in% colnames(table))) {
    stop("Invalid given or target. They must match the row or column names of the table.")
  }

  # Determine the positions of given and target
  if (given %in% rownames(table)) {
    # Given is a row
    if (target %in% colnames(table)) {
      # Target is a column
      return(table[given, target] / sum(table[given, ]))
    } else {
      stop("Mismatch: 'given' is a row name but 'target' is not a column name.")
    }
  } else {
    # Given is a column
    if (target %in% rownames(table)) {
      # Target is a row
      return(table[target, given] / sum(table[, given]))
    } else {
      stop("Mismatch: 'given' is a column name but 'target' is not a row name.")
    }
  }
}


bayes_rule <- function(table, evidence, hypothesis) {
  # Validate inputs
  if (!(evidence %in% rownames(table) || evidence %in% colnames(table)) ||
    !(hypothesis %in% rownames(table) || hypothesis %in% colnames(table))) {
    stop("Invalid evidence or hypothesis. Please ensure they match the row and column names of the table.")
  }

  # Calculate P(Hypothesis)
  prob_hypothesis <- ifelse(hypothesis %in% rownames(table),
    sum(table[hypothesis, ]) / sum(table),
    sum(table[, hypothesis]) / sum(table)
  )

  # Calculate P(Evidence|Hypothesis)
  prob_evidence_given_hypothesis <- conditional_probability(table, hypothesis, evidence)

  # Calculate P(Evidence)
  prob_evidence <- ifelse(evidence %in% rownames(table),
    sum(table[evidence, ]) / sum(table),
    sum(table[, evidence]) / sum(table)
  )

  # Apply Bayes' rule
  prob_hypothesis_given_evidence <- (prob_evidence_given_hypothesis * prob_hypothesis) / prob_evidence
  return(prob_hypothesis_given_evidence)
}






#----------------------------------Pr1------------------------------------------
# Data
males <- c(136, 104) # 136 yes, 104 no
females <- c(224, 36) # 224 yes, 36 no
data <- list(males, females)
col_labels <- c("Yes", "No")
row_labels <- c("Male", "Female")
total <- sum(males) + sum(females)


contingency_table <- create_contingency_table(data, row_labels = row_labels, col_labels = col_labels)
all_probabilities <- calculate_probabilities(contingency_table)


# Probability calculations
prob_male <- get_probability(all_probabilities, "Male")
prob_enjoy <- get_probability(all_probabilities, "Yes")
prob_female_enjoy <- get_probability(all_probabilities, "Female", "Yes")
prob_male_not_enjoy <- get_probability(all_probabilities, "Male", "No")
prob_female_or_enjoy <- get_probability(all_probabilities, "Female") + get_probability(all_probabilities, "Yes") - get_probability(all_probabilities, "Female", "Yes")
prob_male_or_not_enjoy <- get_probability(all_probabilities, "Male") + get_probability(all_probabilities, "No") - get_probability(all_probabilities, "Male", "No")
prob_female_knowing_enjoy <- bayes_rule(contingency_table, evidence = "Yes", "Female")

#----------------------------------Pr2------------------------------------------

# Two manufacturers supply blankets to emergency relief organizations. Manufacturer A
# supplies 3000 blankets and 4% are irregular in workmanship. Manufacturer B supplies
# 2400 blankets and 7% are found to be irregular. Given that a blanket is irregular,
# find the probability that it came from manufacturer B.

# Data
manufacturer_A <- c(3000, 3000 * 0.04) # 3000 yes, 120 no
manufacturer_B <- c(2400, 2400 * 0.07) # 2400 yes, 168 no
data <- list(manufacturer_A, manufacturer_B)
col_labels <- c("Regular", "Irregular")
row_labels <- c("Manufacturer A", "Manufacturer B")


contingency_table <- create_contingency_table(data, row_labels = row_labels, col_labels = col_labels)
prob_manufacturer_B_given_irregular <- bayes_rule(contingency_table, evidence = "Irregular", "Manufacturer B")

#----------------------------------Pr3------------------------------------------

# Company A supplies 40% of the computers sold and is late 5% of the time.
# Company B supplies 30% of the computers sold and is late 3% of the time.
# Company C supplies another 30% and is late 2.5% of the time.
# A computer arrives late - what is the probability that it came from Company A?

# Data
company_A <- c(38, 2) # 40 yes, 2 no
company_B <- c(29.1, 0.9) # 30 yes, 0.9 no
company_C <- c(29.25, 0.75) # 30 yes, 0.75 no
data <- list(company_A, company_B, company_C)
col_labels <- c("Not Late", "Late")
row_labels <- c("Company A", "Company B", "Company C")

contingency_table <- create_contingency_table(data, row_labels = row_labels, col_labels = col_labels)
prob_company_A_given_late <- bayes_rule(contingency_table, evidence = "Late", "Company A")
