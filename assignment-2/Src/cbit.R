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
