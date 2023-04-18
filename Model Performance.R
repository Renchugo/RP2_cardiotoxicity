#Greene, 1983 Dose:75 ADT:15min Cancer
observed_time <- c(0, 0.05, 0.167, 0.5, 1, 3, 6, 24, 48, 72, 98)
observed_concentration <- c(6.25, 2.778, 1.3, 0.355, 0.169, 0.094, 0.078, 0.035, 0.021, 0.0156, 0.0101)

#Camaggi 1988 Dose:60 bolus Age:42-72 Cancer
observed_time <- c(1,2,4,8,12,24,36,48,72,96,120,144,168)
observed_concentration <- c(0.0983,0.0647,0.0473,0.04,0.0291,0.0246,0.0178,0.015,0.0113,0.0079,0.0049,0.0039,0.0031)

# Solve the PBPK model to obtain the predicted concentrations at the time points
predicted <- results %>% filter(time %in% observed_time) %>% select(PL)
if (is.data.frame(predicted)) {
  predicted <- as.numeric(predicted$PL)
} else {
  cat("The 'predicted' object is not a data frame. Please check the object.\n")
}
# Goodness-of-fit metrics

# Calculate Mean Absolute Error (MAE)
mae <- function(predicted, observed) {
  return(mean(abs(predicted - observed)))
}

# Calculate Root Mean Square Error (RMSE)
rmse <- function(predicted, observed) {
  return(sqrt(mean((predicted - observed)^2)))
}


# Calculate the goodness-of-fit metrics
mae_value <- mae(predicted, observed_concentration)
rmse_value <- rmse(predicted, observed_concentration)
r_squared_value <- r_squared(predicted, observed_concentration)

cat("MAE:", mae_value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("R²:", r_squared_value, "\n")

# Pearson correlation coefficient (r): Measures the linear relationship between observed and predicted concentrations. A value close to 1 indicates a strong positive correlation.
correlation <- cor(observed_concentration, predicted)

# Coefficient of Determination (R-squared, R²): Represents the proportion of the variance in the observed data that is predictable from the model. R² ranges from 0 to 1, where 1 indicates a perfect fit, and 0 means the model does not explain any variability in the observed data.
r_squared <- cor(observed_concentration, predicted)^2

# Median Absolute Performance Error (MAPE%):
mape_percent <- function(predicted, observed_concentration) {
  percentage_errors <- abs((observed_concentration - predicted) / observed_concentration) * 100
  return(median(percentage_errors))
}

# Median Performance Error (MPE%):
mpe_percent <- function(predicted, observed_concentration) {
  percentage_errors <- ((observed_concentration - predicted) / observed_concentration) * 100
  return(median(percentage_errors))
}
# Root Mean Squared Performance Error (RMSPE%):
rmspe_percent <- function(predicted, observed_concentration) {
  percentage_errors <- ((observed_concentration - predicted) / observed_concentration)^2 * 100
  return(sqrt(mean(percentage_errors)))
}

mape_value <- mape_percent(predicted_values, observed_concentration)
mpe_value <- mpe_percent(predicted_values, observed_concentration)
rmspe_value <- rmspe_percent(predicted_values, observed_concentration)

cat("MAPE%:", mape_value, "\n")
cat("MPE%:", mpe_value, "\n")
cat("RMSPE%:", rmspe_value, "\n")

# Prediction error (PE) {https://doi.org/10.1371/journal.pcbi.1008786}
PE <- function(predicted, observed_concentration) {
  PE <- ((observed_concentration - predicted) / (observed_concentration + predicted)/2)
  return(mean(PE))
}
PE_value <- PE(predicted_values, observed_concentration)

# Plasma area under curve AUC
library(pracma)
auc_plasma <- trapz(results$time, results$PL)
auc_plasma

#  Akaike Information Criterion (AIC)
residuals <- observed_concentration - predicted
sigma_sq <- var(residuals) #Calculate the variance of the residuals, which represents the spread of the differences between observed and predicted concentrations.
log_likelihood <- -0.5 * sum(((residuals)^2) / sigma_sq + log(2 * pi * sigma_sq))
AIC = -2 * log(log_likelihood) + 2 * k







