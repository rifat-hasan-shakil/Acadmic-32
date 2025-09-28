###Solution 1

# Install package if needed
# install.packages("sampling")

library(sampling)

# Population sales (in thousand Taka)
sales <- c(120, 80, 140, 200, 100, 60)
N <- length(sales)

# PPS with replacement: sample of size 3
set.seed(123)
pps_sample <- ppswr(pik = sales / sum(sales), n = 3)
selected_units <- which(pps_sample > 0)

# Sampled values
sampled_sales <- sales[selected_units]
sampled_sales

# Horvitz-Thompson estimator
pik <- sales / sum(sales)  # selection probs
HT_estimate <- sum(sampled_sales / pik[selected_units])
HT_estimate


###Solution 2

set.seed(123)

# Population: 4 schools, 3 classes each, 10 students per class
schools <- 1:4
classes <- 1:3
students <- 1:10

# Generate population scores
population <- expand.grid(School = schools, Class = classes, Student = students)
population$Score <- round(rnorm(nrow(population), mean = 50, sd = 10), 1)

# Stage 1: Select 2 schools
selected_schools <- sample(schools, 2)
# Stage 2: Select 1 class per selected school
selected_classes <- unlist(lapply(selected_schools, function(s) sample(classes, 1)))
# Stage 3: Select 5 students per selected class
sampled_data <- do.call(rbind, lapply(1:2, function(i) {
  subset(population, School == selected_schools[i] & Class == selected_classes[i]) |>
    sample(5)
}))

# Sample mean estimate
mean(sampled_data$Score)


set.seed(123)

# Generate population
N <- 1000
household_size <- rpois(N, 5) + 1
income <- 2000 * household_size + rnorm(N, mean = 0, sd = 5000)

# First phase sample: collect household size
phase1 <- sample(1:N, 100)
aux_data <- data.frame(ID = phase1, Size = household_size[phase1])

# Second phase sample: collect income
phase2 <- sample(phase1, 40)
sub_data <- data.frame(ID = phase2, Size = household_size[phase2], Income = income[phase2])

# Regression estimator
reg_model <- lm(Income ~ Size, data = sub_data)

# Predict population income using auxiliary info
y_hat <- predict(reg_model, newdata = data.frame(Size = household_size))
reg_estimate <- mean(y_hat)

# Compare with true mean
true_mean <- mean(income)
c(True = true_mean, Regression_Estimate = reg_estimate)



##### 4
set.seed(123)

# Population
N <- 1000
Smoker <- rbinom(N, 1, 0.3)

# SRS simulation
simulate_srs <- function() {
  samp <- sample(Smoker, 100)
  mean(samp)
}

# Repeat 50 times
sample_means <- replicate(50, simulate_srs())

# Bias and MSE
bias <- mean(sample_means) - mean(Smoker)
mse <- mean((sample_means - mean(Smoker))^2)

hist(sample_means, col = "skyblue", main = "Distribution of Sample Proportions")
abline(v = mean(Smoker), col = "red", lwd = 2)

c(True_Proportion = mean(Smoker), Bias = bias, MSE = mse)



#####5

set.seed(123)

# Population of 500 households
population <- rnorm(500, mean = 15000, sd = 3000)

# Census true mean
true_mean <- mean(population)

# Function for survey
survey_mean <- function() {
  samp <- sample(population, 50)
  mean(samp)
}

# Repeat 1000 times
survey_means <- replicate(1000, survey_mean())

# Plot
hist(survey_means, col = "lightgreen", main = "Distribution of Sample Means")
abline(v = true_mean, col = "red", lwd = 2)

c(True_Census_Mean = true_mean, Survey_Mean = mean(survey_means))

