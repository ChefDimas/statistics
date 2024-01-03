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

#----------------------------------Pr3------------------------------------------

# A basket of various products was purchased online at 7 different online stores and the following prices were charged in $
# OS1 72.95
# OS2 75.13
# OS3 75.85
# OS4 62.13
# OS5 52.70
# OS6 72.19
# Construct a 95% confidence interval es�mate of the mean price of the basket at online stores?

# Data
n <- 7
average <- mean(c(72.95, 75.13, 75.85, 62.13, 52.70, 72.19))
sd <- sd(c(72.95, 75.13, 75.85, 62.13, 52.70, 72.19))
alpha <- 0.05

rez1 <- average - qt(1 - alpha / 2, n - 1) * sd / sqrt(n)
rez2 <- average + qt(1 - alpha / 2, n - 1) * sd / sqrt(n)
