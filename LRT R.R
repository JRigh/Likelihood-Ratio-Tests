#-----------------------
# Likelihood Ratio Tests
#-----------------------

#--------------------------------
# 1. Normal model, variance known

# Set seed for reproducibility
set.seed(2023)

# Parameters
mu = 12 # true mean (unknown in practice)
n = 40; sigma = 3; mu0 = seq(5, 15, by = 1); alpha = 0.05

# Generate artificial sample from a normal distribution
data <- round(rnorm(n, mean = mu, sd = sigma), 2)

# save a copy of entire dataset, training and testing datasets in .csv
write.csv(data, 
          "C:/Users/julia/OneDrive/Desktop/github/36. LRT/data.csv",
          row.names = FALSE)

#  LRT statistic
lrt_statistic <- n * ((mean(data) - mu0)^2 / sigma^2)

# Calculate the critical value from the chi-squared distribution
critical_value <- qchisq(1 - alpha, df = 1)

# Perform the Likelihood Ratio Test
reject_null <- lrt_statistic > critical_value

# Print results
results = data.frame(mu0 = mu0, 
                     lrt_statistic = lrt_statistic, 
                     decision = ifelse(lrt_statistic > critical_value, 'Yes', 'No'))
#    mu0 lrt_statistic decision
# 1    5   232.3952474      Yes
# 2    6   172.5631715      Yes
# 3    7   121.6199845      Yes
# 4    8    79.5656864      Yes
# 5    9    46.4002772      Yes
# 6   10    22.1237569      Yes
# 7   11     6.7361255      Yes
# 8   12     0.2373829       No
# 9   13     2.6275293       No
# 10  14    13.9065645      Yes
# 11  15    34.0744886      Yes

#----------------------------------
# 1. Normal model, variance unknown