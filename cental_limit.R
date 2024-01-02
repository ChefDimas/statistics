#----------------------------------Functions----------------------------------

#----------------------------------Pr1------------------------------------------
# An electrical firm manufactures light bulbs that have a length of life that is approx.
# normally distributed, with a standard deviation of 40 hours.
# If a sample of 30 bulbs has an average of 780 hours, find the 96% confidence interval
# for the population mean of all bulbs produces by the firm.

# Data
n <- 30
average <- 780
sd <- 40
alpha <- 0.04

rez1 <- average - qnorm(1 - alpha / 2) * sd / sqrt(n)
rez2 <- average + qnorm(1 - alpha / 2) * sd / sqrt(n)

#----------------------------------Pr2------------------------------------------
# An electrical firm manufactures light bulbs that have a length of life that is approx.
# normally distributed. If a sample of 5 bulbs have the following lives: 740, 760, 780, 800, 820 h,
# find the 96% confidence interval for the population mean of all bulbs produces by the firm.

# Data
n <- 5
average <- mean(c(740, 760, 780, 800, 820))
sd <- sd(c(740, 760, 780, 800, 820))
alpha <- 0.04

rez1 <- average - qt(1 - alpha / 2, n - 1) * sd / sqrt(n)
rez2 <- average + qt(1 - alpha / 2, n - 1) * sd / sqrt(n)
