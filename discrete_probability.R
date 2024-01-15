#----------------------------------Functions----------------------------------


#----------------------------------Pr1------------------------------------------
# Suppose there are twelve (12) multiple choice questions in an English class quiz.
# Each question has five (5) possible answers, and only one of them is correct.
# Find the probability of having six correct answers if a student attempts to answer every question at random.

# Data
size <- 12
p <- 1 / 5
rez <- dbinom(6, size, prob = p)

#----------------------------------Pr2------------------------------------------
# Suppose there are twelve multiple choice questions in an English class quiz.
# Each question has five possible answers, and only one of them is correct.
# Find the probability of having six or less correct answers if a student attempts to answer every question at random.

# Data
size <- 12
p <- 1 / 5
rez <- pbinom(6, size, prob = p, lower.tail = TRUE)

#----------------------------------Pr3------------------------------------------
# If there are twelve cars crossing a bridge per minute on average, find the probability
# of having seventeen or more cars crossing the bridge in a particular minute

# Data
lambda <- 12
rez <- ppois(16, lambda = lambda, lower.tail = FALSE)

#----------------------------------Pr4------------------------------------------
# The filling process of an energy drink manufacturer is normally distributed with mean of 0.25L and a standard deviation of 0.05L
# What is the probability that a can will have a filling volume between 0.2L and 0.3L?

# Data
mean <- 0.25
sd <- 0.05
rez <- pnorm(0.3, mean = mean, sd = sd) - pnorm(0.2, mean = mean, sd = sd)

#----------------------------------Pr5------------------------------------------
# Assuming that the test scores of a college entrance exam fit a normal distribution.
# Furthermore, the mean test score is 72.0, and the standard deviation is 8.0
# What is the percentage of students scoring 80 or more in the exam?
# Between which 2 grades (around the mean) will 50% of the grades lie?
# Between which 2 grades (around the mean) will 95% of the grades lie?

# Data

mean <- 72
sd <- 8
rez1 <- pnorm(80, mean = mean, sd = sd, lower.tail = FALSE)
rez2 <- qnorm(c(0.25, 0.75), mean = mean, sd = sd)
rez3 <- qnorm(c(0.025, 0.975), mean = mean, sd = sd)

# The IQ’s of 600 applicants to a certain college are approximately normally distributed
# with a mean of 100 and a standard deviation of 15
# a) If the college requires an IQ of at least 95, how many students will be rejected?
# b) What is the 80% IQ range? And the 95% IQ range?

# Data
mean <- 100
sd <- 15
rez1 <- pnorm(95, mean = mean, sd = sd, lower.tail = TRUE) * 600
rez2 <- qnorm(c(0.1, 0.9), mean = mean, sd = sd)
rez3 <- qnorm(c(0.025, 0.975), mean = mean, sd = sd)

#----------------------------------Pr7------------------------------------------
# The annual salaries of employees in a large company are approximately normally
# distributed with a mean of $50,000 and a standard deviation of $15,000.
# What percent of people earn less than $35,000 a year?
# What percent of people earn between $35,000 and $65,000?
# What percent of people earn more than $80,000?

# Data
mean <- 50000
sd <- 15000
rez1 <- pnorm(35000, mean = mean, sd = sd, lower.tail = TRUE)
rez2 <- pnorm(65000, mean = mean, sd = sd, lower.tail = TRUE) - pnorm(35000, mean = mean, sd = sd, lower.tail = TRUE)
rez3 <- pnorm(80000, mean = mean, sd = sd, lower.tail = FALSE)


#----------------------------------Pr8------------------------------------------
# In the last 100 years, there have been 93 earthquakes measuring 6.0 or more on the
# Richter scale. What is the probability of having 3 earthquakes in the same year that
# all measure 6.0 or more? 0.053

# Data
lambda <- 93 / 100
rez <- dpois(3, lambda = lambda)

#----------------------------------Pr9------------------------------------------
# Telephone calls enter a college switchboard on the average of two every three minutes.
# What is the probability of 5 or more calls arriving in a 9-minute period? 0.715

# Data
lambda <- 2 / 3 * 9
rez <- ppois(4, lambda = lambda, lower.tail = FALSE)

#----------------------------------Pr10------------------------------------------
# Find P(−1 < z < 1) and P(−2 < z < 2). Does your answer agree with the Thumb Rule (magic numbers)?

# Data
rez1 <- pnorm(1, mean = 0, sd = 1) - pnorm(-1, mean = 0, sd = 1)
rez2 <- pnorm(2, mean = 0, sd = 1) - pnorm(-2, mean = 0, sd = 1)
rez3 <- pnorm(3, mean = 0, sd = 1) - pnorm(-3, mean = 0, sd = 1)

#----------------------------------Pr11------------------------------------------
# Find the interquartile range of a normal distribution with mean 100 and standard deviation 10?

# Data
rez <- qnorm(0.75, 100, 10) - qnorm(0.25, 100, 10)


#----------------------------------Pr12------------------------------------------
# he average salary for first-year teachers is $27,989. Assume the distribution is
# approximately normal with standard deviation $3250.
# What is the probability that a randomly selected first-year teacher makes between $20,000 and $30,000 each year? 0.725
# What is the probability that a randomly selected first-year teacher's salary less than $20,000? 0.007
# What is the 90th percentile salary?32,154$

# Data
mean <- 27989
sd <- 3250
rez1 <- pnorm(30000, mean = mean, sd = sd, lower.tail = TRUE) - pnorm(20000, mean = mean, sd = sd, lower.tail = TRUE)
rez2 <- pnorm(20000, mean = mean, sd = sd, lower.tail = TRUE)
rez3 <- qnorm(0.9, mean = mean, sd = sd)


#----------------------------------PrSALES------------------------------------------
prob_close_deal <- 0.2
deal_value <- 25000
target_deals <- 25
total_deals <- 100
rez <- pbinom(target_deals, size = total_deals, prob = prob_close_deal, lower.tail = FALSE)
print(rez)
