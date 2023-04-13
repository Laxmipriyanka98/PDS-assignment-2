# PDS-assignment-2
# A Bit
# data analysis
diabetes <- read.csv('C:/Users/priya/OneDrive/Documents/pds assignment-2/assignment-2/diabetes.csv')

#data clean
library(dplyr)
Read the CSV file into a dataframe
df <- read.csv('C:/Users/priya/OneDrive/Documents/pds assignment-2/assignment-2/diabetes.csv')

Check for missing values in each column
missing_values <- colSums(is.na(df))

Print the missing values count for each columnn
cat("Missing Values Count:\n")
print(missing_values)
#Since there are no missing values the data provided is already clean

set data
set.seed(123)

 choose sample of 35 observations
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

# B bit
# to determine the 98th percentile of the population and the sample's BMI
BMI_95_sample <- quantile(sample_data$BMI, probs=0.95)
BMI_95_pop <- quantile(diabetes$BMI, probs=0.95)

# To compare BMI distributions, build a boxplot.
boxplot(diabetes$BMI, sample_data$BMI, names=c("Population", "Sample"), 
        main="Comparing BMI Distributions", ylab="BMI",col="yellow")

#C bit
# sampling using bootsrap
library(boot)
set.seed(122)
n_samples <- 500
sample_size <- 151
boot_samples <- boot(diabetes$BloodPressure, function(data, idx) {
  mean_bp <- mean(data[idx])
  sd_bp <- sd(data[idx])
  perc_bp <- quantile(data[idx], probs=0.92)
  return(c(mean_bp, sd_bp, perc_bp))
}, R=n_samples, strata=diabetes$Outcome, sim="ordinary")

# bootstrap statistics extraction
boot_means <- boot_samples$t[,1]
boot_sds <- boot_samples$t[,2]
boot_percs <- boot_samples$t[,3]

# calculate demographic data
mean_bp_pop <- mean(diabetes$BloodPressure)
sd_bp_pop <- sd(diabetes$BloodPressure)
perc_bp_pop <- quantile(diabetes$BloodPressure, probs=0.92)

# To compare the distributions of bootstrap statistics and population statistics, build histograms.
par(mfrow=c(1,1))
hist(boot_means, main="Distributing Bootstrap Mean", xlab="Mean BloodPressure",col="green")
abline(v=mean_bp_pop, col="red")
hist(boot_sds, main="Distributing Bootstrap Standard Deviation", xlab="Standard",col="blue")

