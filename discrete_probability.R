#----------------------------------Functions----------------------------------


#----------------------------------Pr1------------------------------------------
# Suppose there are twelve (12) multiple choice questions in an English class quiz.
# Each question has five (5) possible answers, and only one of them is correct.
# Find the probability of having six correct answers if a student attempts to answer every question at random.

# Data
size <- 12
p <- 1 / 5
dbinom(6, size, prob = p)
