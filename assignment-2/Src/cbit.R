num_samples = 500
sample_size = 150

# Create an empty array to store the bootstrap statistics
bootstrap_stats = np.empty(num_samples,3)

# Generate the bootstrap samples and calculate their statistics
for i in range(num_samples):
  sample = df['BloodPressure'].sample(n=sample_size, replace=True)
bootstrap_stats[i, 0] = sample.mean()
bootstrap_stats[i, 1] = sample.std()
bootstrap_stats[i, 2] = np.percentile(sample, 90)

# Calculate the population statistics
population_mean = df['BloodPressure'].mean()
population_std = df['BloodPressure'].std()
population_percentile = np.percentile(df['BloodPressure'], 90)

# Create a bar chart to compare the means of the bootstrap samples and the population
plt.bar(['Bootstrap Mean', 'Population Mean'], [bootstrap_stats[:, 0].mean(), population_mean])
plt.title('Comparison of BloodPressure Means')
plt.ylabel('BloodPressure')
plt.show()

# Create a bar chart to compare the standard deviations of the bootstrap samples and the population
plt.bar(['Bootstrap Standard Deviation', 'Population Standard Deviation'], 
        [bootstrap_stats[:, 1].mean(), population_std])
plt.title('Comparison of BloodPressure Standard Deviations')
plt.ylabel('BloodPressure')
plt.show()

# Create a bar chart to compare the 90th percentile of the bootstrap samples and the population
plt.bar(['Bootstrap 90th Percentile', 'Population 90th Percentile'], 
        [bootstrap_stats[:, 2].mean(), population_percentile])
plt.title('Comparison of BloodPressure 90th Percentiles')
plt.ylabel('BloodPressure')
plt.show()