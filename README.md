# PDS-assignment-2
# A Bit
# Data analysis 
diabetes <- read.csv('C:/Users/priya/OneDrive/Documents/pds assignment-2/assignment-2/diabetes.csv')

# Data clean
 
 library(dplyr)

* Read the CSV file

df <- read.csv('C:/Users/priya/OneDrive/Documents/pds assignment-2/assignment-2/diabetes.csv')

* Check missing values columns

missing_values <- colSums(is.na(df))

* Print the missing values 

cat("Missing Values Count:\n")

print(missing_values)

# Because there are no missing values, the information is already accurate.

* set data

set.seed(121)

* choose sample of 25 observations

sample_data <- diabetes[sample(nrow(diabetes), 25, replace=FALSE), ]

* calculate sample of mean and highest glucose values 

mean_glucose <- mean(sample_data$Glucose)

max_glucose <- max(sample_data$Glucose)

* calculate mean and highest glucose values

mean_glucose_pop <- mean(diabetes$Glucose)

max_glucose_pop <- max(diabetes$Glucose)

* bar chart to compare mean values

mean_values <- c(mean_glucose_pop, mean_glucose)

names(mean_values) <- c("Population", "Sample")

barplot(mean_values, main="Comparing Mean Glucose Values", ylab="Mean Glucose",col="pink")


* bar chart to compare highest glucose values

max_values <- c(max_glucose_pop, max_glucose)

names(max_values) <- c("Population", "Sample")

barplot(max_values, main="Comparing Highest Glucose Values", ylab="Highest Glucose",col="violet")

# B bit
* to determine the 98th percentile of the population and the sample's BMI

BMI_98_sample <- quantile(sample_data$BMI, probs=0.98)

BMI_98_pop <- quantile(diabetes$BMI, probs=0.98)

* To compare BMI distributions, build a boxplot.

boxplot(diabetes$BMI, sample_data$BMI, names=c("Population", "Sample"), 
        main="Comparing BMI Distributions", ylab="BMI",col="yellow")

# C bit
* libraries for bootstrapping

library(boot)

* setting seed  121

set.seed(121)

*  no.of samples to be created

num_samples <- 500

* no.of observations considered 

sample_size <- 150

* calculate mean, standard deviation, and percentile of sample

boot_samples <- boot(diabetes$BloodPressure, function(data, idx) {
 
 boot_means <- mean(data[idx])
 
 boot_sds <- sd(data[idx])
 
 boot_percs <- quantile(data[idx], probs=0.92)
 
 return(c(boot_means, boot_sds, boot_percs))

}, R=num_samples, strata=diabetes$Outcome, sim="ordinary")

* extract sample bootstrap statistic values 

bootmean <- boot_samples$t[,1]

mean_bootmean <- mean(bootmean)

bootstdev <- boot_samples$t[,2]

mean_bootstdev <- mean(bootstdev)

bootquantile <- boot_samples$t[,3]

mean_bootquantile <- mean(bootquantile)

* calculate bootstrap statistic values of population

mean_BPpop <- mean(diabetes$BloodPressure)

sd_BPpop <- sd(diabetes$BloodPressure)

quan_BPpop <- quantile(diabetes$BloodPressure, probs=0.92)

* plotting histograms to compare 

quan_compare <- c(Population_quantile=quan_BPpop,Sample_quantile=mean_bootquantile)

mean_compare <- c(Population_mean=mean_BPpop,Sample_mean=mean_bootmean)

stdev_compare <- c(Population_stdev=sd_BPpop,Sample_stdev=mean_bootstdev)



barplot(quan_compare, col = c("orange","yellow"), space=1,
        main="Comparing quantile at 95% of population & bootstrap sample of Blood Pressure",xlab="Group",ylab="quantile of value")

barplot(mean_compare, col = c("pink","red"), space=1,
        main="Comparing mean of population & bootstrap sample of Blood Pressure",xlab="Group",ylab="mean of value")

barplot(stdev_compare, col = c("blue","skyblue"), space=1,
        main="Comparing of standard deviation of population & bootstrap sample of Blood Pressure",xlab="Group",ylab="standard deviation of value")

