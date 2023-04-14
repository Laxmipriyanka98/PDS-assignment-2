# libraries for bootstrapping
library(boot)

#setting seed  121
set.seed(121)

# no.of samples to be created
num_samples <- 500

#no.of observations considered 
sample_size <- 150

#calculate mean, standard deviation, and percentile of sample
boot_samples <- boot(diabetes$BloodPressure, function(data, idx) {
  boot_means <- mean(data[idx])
  boot_sds <- sd(data[idx])
  boot_percs <- quantile(data[idx], probs=0.92)
  return(c(boot_means, boot_sds, boot_percs))
}, R=num_samples, strata=diabetes$Outcome, sim="ordinary")

#extract sample bootstrap statistic values 
bootmean <- boot_samples$t[,1]
mean_bootmean <- mean(bootmean)

bootstdev <- boot_samples$t[,2]
mean_bootstdev <- mean(bootstdev)

bootquantile <- boot_samples$t[,3]
mean_bootquantile <- mean(bootquantile)

#calculate bootstrap statistic values of population
mean_BPpop <- mean(diabetes$BloodPressure)
sd_BPpop <- sd(diabetes$BloodPressure)
quan_BPpop <- quantile(diabetes$BloodPressure, probs=0.92)

#plotting histograms to compare 
quan_compare <- c(Population_quantile=quan_BPpop,Sample_quantile=mean_bootquantile)
mean_compare <- c(Population_mean=mean_BPpop,Sample_mean=mean_bootmean)
stdev_compare <- c(Population_stdev=sd_BPpop,Sample_stdev=mean_bootstdev)



barplot(quan_compare, col = c("orange","yellow"), space=1,
        main="Comparing quantile at 95% of population & bootstrap sample of Blood Pressure",xlab="Group",ylab="quantile of value")

barplot(mean_compare, col = c("pink","red"), space=1,
        main="Comparing mean of population & bootstrap sample of Blood Pressure",xlab="Group",ylab="mean of value")

barplot(stdev_compare, col = c("blue","skyblue"), space=1,
        main="Comparing of standard deviation of population & bootstrap sample of Blood Pressure",xlab="Group",ylab="standard deviation of value")
