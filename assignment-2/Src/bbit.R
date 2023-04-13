# to determine the 98th percentile of the population and the sample's BMI
BMI_95_sample <- quantile(sample_data$BMI, probs=0.95)
BMI_95_pop <- quantile(diabetes$BMI, probs=0.95)

# To compare BMI distributions, build a boxplot.
boxplot(diabetes$BMI, sample_data$BMI, names=c("Population", "Sample"), 
        main="Comparing BMI Distributions", ylab="BMI",col="yellow")
