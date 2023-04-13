# load data
diabetes <- read.csv("diabetes.csv")

# set seed
set.seed(123)

# take random sample of 25 observations
sample_data <- diabetes[sample(nrow(diabetes), 25, replace=FALSE), ]

# calculate mean and highest glucose values of sample
mean_glucose <- mean(sample_data$Glucose)
max_glucose <- max(sample_data$Glucose)

# calculate mean and highest glucose values of population
mean_glucose_pop <- mean(diabetes$Glucose)
max_glucose_pop <- max(diabetes$Glucose)

# create bar chart to compare mean glucose values
mean_values <- c(mean_glucose_pop, mean_glucose)
names(mean_values) <- c("Population", "Sample")
barplot(mean_values, main="Comparison of Mean Glucose Values", ylab="Mean Glucose")

# create bar chart to compare highest glucose values
max_values <- c(max_glucose_pop, max_glucose)
names(max_values) <- c("Population", "Sample")
barplot(max_values, main="Comparison of Highest Glucose Values", ylab="Highest Glucose")
