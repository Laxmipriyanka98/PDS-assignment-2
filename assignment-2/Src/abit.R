# data analysis
diabetes <- read.csv('C:/Users/priya/OneDrive/Documents/pds assignment-2/assignment-2/diabetes.csv')

# set data
set.seed(123)

# choose sample of 35 observations
sample_data <- diabetes[sample(nrow(diabetes), 26, replace=FALSE), ]

# calculate sample of mean and highest glucose values 
mean_glucose <- mean(sample_data$Glucose)
max_glucose <- max(sample_data$Glucose)

# calculate mean and highest glucose values
mean_glucose_pop <- mean(diabetes$Glucose)
max_glucose_pop <- max(diabetes$Glucose)

# bar chart to compare mean values
mean_values <- c(mean_glucose_pop, mean_glucose)
names(mean_values) <- c("Population", "Sample")
barplot(mean_values, main="Comparing Mean Glucose Values", ylab="Mean Glucose",col="pink")


# bar chart to compare highest glucose values
max_values <- c(max_glucose_pop, max_glucose)
names(max_values) <- c("Population", "Sample")
barplot(max_values, main="Comparing Highest Glucose Values", ylab="Highest Glucose",col="violet")

