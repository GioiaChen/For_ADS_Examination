# Season preferences data
Poll_seasons <- data.frame(Spring = 40, Summer = 30, Autumn = 18, Winter = 28)
expected_freq <- sum(Poll_seasons) * 0.25

# Number of simulations
num_simulations <- 10000

# Initialize vector to store chi-square values
chi_sq_values <- numeric(num_simulations)

# Perform simulations
for (i in 1:num_simulations) {
  # Generate population with expected proportions
  population <- rep(c("Spring", "Summer", "Autumn", "Winter"), expected_freq)
  
  # Sample 116 values
  sample_data <- sample(population, 116, replace = TRUE)
  
  # Calculate observed frequencies
  observed_freq <- table(sample_data)
  
  # Calculate chi-square value
  chi_sq_values[i] <- sum((observed_freq - expected_freq)^2 / expected_freq)
}

# Visualize the distribution of simulated chi-square values
plot(density(chi_sq_values))

# Calculate p-value using simulated distribution
p_value_simulated <- mean(chi_sq_values >= chisq.test(Poll_seasons)$statistic)





# Generate random chi-square values with degrees of freedom 1, 5, and 10
chi_sq_df1 <- rchisq(1000, df = 1)
chi_sq_df5 <- rchisq(1000, df = 5)
chi_sq_df10 <- rchisq(1000, df = 10)

# Plot the chi-square distributions
hist(chi_sq_df1, breaks = 30, col = "skyblue", xlab = "Chi-square Value", main = "Chi-square Distribution (df = 1)")
hist(chi_sq_df5, breaks = 30, col = "skyblue", xlab = "Chi-square Value", main = "Chi-square Distribution (df = 5)")
hist(chi_sq_df10, breaks = 30, col = "skyblue", xlab = "Chi-square Value", main = "Chi-square Distribution (df = 10)")





# Example data for chi-square test of homogeneity
season_pref <- c("Spring", "Summer", "Autumn", "Winter")
allergy_report <- c("Yes", "No")

# Generate example data frame
data <- expand.grid(Season = season_pref, Allergy = allergy_report)
data$count <- c(10, 5, 3, 2, 15, 20, 10, 5, 8, 12, 5, 3)

# Visualize the data
mosaicplot(data$count ~ data$Season + data$Allergy, main = "Mosaic Plot")

# Perform chi-square test of homogeneity
chisq_result <- chisq.test(data$count, data$Season, data$Allergy)





# Create a matrix with survival data
geneX_survival_data <- matrix(c(10, 20, 5, 15), nrow = 2, byrow = TRUE,
                              dimnames = list(c("Survived", "Not Survived"), c("GeneX Present", "GeneX KO")))

# Perform a chi-square test without Yates's continuity correction
chi_square_result_without_correction <- chisq.test(geneX_survival_data, correct = FALSE)

# Print the result and any warning messages
print(chi_square_result_without_correction)

# Perform a chi-square test with Yates's continuity correction
chi_square_result_with_correction <- chisq.test(geneX_survival_data, correct = TRUE)

# Print the result and any warning messages
print(chi_square_result_with_correction)

# Perform a Fisher’s exact test
fisher_test_result <- fisher.test(geneX_survival_data)

# Print the result
print(fisher_test_result)


require(stats)
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)
## Formula interface for tabulated data:
mosaicplot(~ Sex + Age + Survived, data = Titanic, color = TRUE)

mosaicplot(HairEyeColor, shade = TRUE)
## Independence model of hair and eye color and sex.  Indicates that
## there are more blue eyed blonde females than expected in the case
## of independence and too few brown eyed blonde females.
## The corresponding model is:
fm <- loglin(HairEyeColor, list(1, 2, 3))
pchisq(fm$pearson, fm$df, lower.tail = FALSE)

mosaicplot(HairEyeColor, shade = TRUE, margin = list(1:2, 3))
## Model of joint independence of sex from hair and eye color.  Males
## are underrepresented among people with brown hair and eyes, and are
## overrepresented among people with brown hair and blue eyes.
## The corresponding model is:
fm <- loglin(HairEyeColor, list(1:2, 3))
pchisq(fm$pearson, fm$df, lower.tail = FALSE)

## Formula interface for raw data: visualize cross-tabulation of numbers
## of gears and carburettors in Motor Trend car data.
mosaicplot(~ gear + carb, data = mtcars, color = TRUE, las = 1)
# color recycling
mosaicplot(~ gear + carb, data = mtcars, color = 2:3, las = 1)


library(ggplot2)
degrees_of_freedom<-c(1,2,3,4,5,10,20)
data <- apply(degrees_of_freedom, function(df){
  data.frame(chi_square =rchisq(10000, df, ncp=0),df = as.factor(df))})
data_combined <- do.call(rbind, data)
ggplot(data_combined,aes(x=chi_square,fill=df))+
  geom_density(alpha=0.5)+
  labs(title ="chi-square Distribution for Different Degrees of Freedom", x="Chi-square value",
y= "Density",fill = "Degrees of Freedom") +
  theme_minimal()


data<-data.frame(Spring=c(5,8,9,18),Summer=c(1,5,8,16),Fall=c(1,2,3,12),Winter=c(9,5,9,5),
                 row.names=c("Severe allergies","Mild allergies","Sporadic allergies","Never allergic"))
barplot(as.matrix(data),col=c("pink1","pink2","pink3","pink4"))
mosaicplot(data,col=c("pink1","pink2","pink3","pink4"))
library(ggplot2)
library(ggpubr)
ggballoonplot(data)













