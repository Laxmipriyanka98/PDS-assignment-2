# calculate 98th percentile of BMI for sample and population
BMI_98_sample <- quantile(sample_data$BMI, probs=0.98)
BMI_98_pop <- quantile(diabetes$BMI, probs=0.98)

# create boxplot to compare BMI distributions
boxplot(diabetes$BMI, sample_data$BMI, names=c("Population", "Sample"), 
        main="Comparison of BMI Distributions", ylab="BMI")
