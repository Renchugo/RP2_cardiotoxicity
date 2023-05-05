# 0-3 hour
#Benjamin, 1973 Dose:60 ADT: 1-5min Cancer
points(c(0.083,0.167,0.25,0.33,0.5,0.75,1), c(5.66,2.6,1.31,0.753,0.353,0.236,0.197), pch=16, col="black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "60mg/m^2 (Benjamin 1973)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(0.083,0.167,0.25,0.33,0.5,0.75,1)
observed_concentration <- c(5.66,2.6,1.31,0.753,0.353,0.236,0.197)

#Robert, 1983 Dose:35 Age:16-67 ADT:3min Cancer
points(c(0.05,0.1,0.167,0.25,0.33), c(5.207,2.73,1.48,0.54,0.332), pch=16, col= "black")
points(c(0.05,0.1,0.167,0.25,0.33), c(3.72,1.862,0.901,0.387,0.215), pch=16, col= "#CC79A7")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "#CC79A7", cex = 1.0)
points(x = max(results$time) * 0.30, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "35mg/m^2 (Robert 1983)", pos = 4, cex = 1.0, col = "black", font = 2)

observed_time <- c(0.05,0.1,0.167,0.25,0.33)
observed_concentration <- c(5.207,2.73,1.48,0.54,0.332)

observed_concentration <- c(3.72,1.862,0.901,0.387,0.215)

#Kerr 1986 Dose: 40 mg/m2 IV bolus Age:median: 52 small cell lung cancer
points(c(0.27,0.44,0.64,1.038,1.43,2.01,3.99,5.95,7.98,9.98,12.01,15.97,23.93,47.93),c(2.058,0.911,0.314,0.295,0.214,0.127,0.057,0.051,0.043,0.038,0.033,0.028,0.025,0.011),pch=16, col= "black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "40mg/m^2 (Kerr 1986)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(0.27,0.44,0.64,1.04,1.43,2.01,3.99,5.95,7.98,9.98,12.01,15.97,23.93,47.93)
observed_concentration <- c(2.058,0.911,0.314,0.295,0.214,0.127,0.057,0.051,0.043,0.038,0.033,0.028,0.025,0.011)

#Eksborg, 1986 Dose: 40mg IV bolus Time: 3 min Age:42-70 Ovarian carcinoma (Time after adm (hr)) 
points(c(0.000+0.05,0.14+0.05,0.45+0.05,0.65+0.05,0.91+0.05,1.91+0.05,2.95+0.05,5.96+0.05,11.95+0.05,17.91+0.05,23.98+0.05),c(2.078,0.431,0.101,0.054,0.046,0.025,0.023,0.014,0.009,0.007,0.009),pch=16, col= "black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "40mg/m^2 (Eksborg 1986)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(0.000+0.05,0.14+0.05,0.45+0.05,0.65+0.05,0.91+0.05,1.91+0.05,2.95+0.05,5.96+0.05,11.95+0.05,17.91+0.05,23.98+0.05)
observed_concentration <- c(2.078,0.431,0.101,0.054,0.046,0.025,0.023,0.014,0.009,0.007,0.009)

#Yoshida 1994 Dose:0.9 mg/kg Age:47-72 ADT: 30min Cancer
points(c(0.5742,0.8134,1,1.292,1.531,2.536,4.498,8.518,24.45,43.5),c(2.4638,0.2566,0.0896,0.0645,0.0568,0.0435,0.0323,0.0248,0.0152,0.0078), pch=16, col= "black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "36mg/m^2 (Yoshida 1994)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(0.57,0.81,1,1.29,1.53,2.54,4.50,8.52,24.45,43.5)
observed_concentration <-c(2.4638,0.2566,0.0896,0.0645,0.0568,0.0435,0.0323,0.0248,0.0152,0.0078)

# Eksborg, 1985 Dose: 50 mg or 33.6 mg/m2  IV bolus Time: 3 min Age:63 Colon cancer, normal liver function
points(c(0.02,0.15,0.43,0.67,0.91,1.93,2.89,5.86,11.83,17.94,23.93),c(3.865,0.387,0.163,0.111,0.081,0.055,0.045,0.038,0.024,0.020,0.013),pch=16, col= "black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "33.6mg/m^2 (Eksborg, 1985)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(0.02,0.15,0.43,0.67,0.91,1.93,2.89,5.86,11.83,17.94,23.93)
observed_concentration <- c(3.865,0.387,0.163,0.111,0.081,0.055,0.045,0.038,0.024,0.020,0.013)

#Piscitelli, 1993 Dose:45 - 72 mg/m2 Age:55 ADT:1hour Cancer
points(c(1.27,1.58,2.22,3.16,4.16,6.15,8.10,12.10,24.08,36.06,47.00),c(1.726,0.2,0.125,0.092,0.074,0.056,0.048,0.041,0.029,0.020,0.014), pch=16, col= "black")
points(x = max(results$time) * 0.30, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.30, y = max(results$PL* 0.8) , labels = "45-72mg/m^2 (Piscitelli 1993)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(1.27,1.58,2.22,3.16,4.16,6.15,8.10,12.10,24.08,36.06,47.00)
observed_concentration <- c(1.726,0.2,0.125,0.092,0.074,0.056,0.048,0.041,0.029,0.020,0.014)

#Erttmann 1988 Dose: 15 mg/m2 IV bolus Time:5min  Age: 5-24
points(c(0.11,0.11,0.22,0.27,0.45,0.10,2.00,2.99,3.94,5.00,6.02,7.55,8.00,8.25,8.50,9.18,10.07,10.67),c(1.979,0.772,0.275,0.165,0.0772,0.0494,0.0375,0.0296,0.026,0.022,0.0173,0.0162,0.01998,0.0184,0.0169,0.0173,0.0163,0.0144),pch=16, col= "black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "15mg/m^2 (Erttmann 1988)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(0.11,0.11,0.22,0.27,0.45,0.10,2.00,2.99,3.94,5.00,6.02,7.55,8.00,8.25,8.50,9.18,10.07,10.67)
  observed_concentration <- c(1.979,0.772,0.275,0.165,0.0772,0.0494,0.0375,0.0296,0.026,0.022,0.0173,0.0162,0.01998,0.0184,0.0169,0.0173,0.0163,0.0144)

#Greene, 1983 Dose:75 ADT:15min Cancer
points(c(0,0.05,0.17,0.5,1,3,6,24,48,72,98), c(6.25,2.778,1.3,0.355,0.169,0.094,0.078,0.035,0.021,0.0156,0.0101), pch=16, col= "black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "75mg/m^2 (Greene 1983)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(0,0.05,0.17,0.5,1,3,6,24,48,72,98)
observed_concentration <- c(6.25,2.778,1.3,0.355,0.169,0.094,0.078,0.035,0.021,0.0156,0.0101)

#Chan, 1978 IV bolus Time: 1-2 minSolid tumors
# Patient 1: hepatoma, normal renal/hep function, 30 mg/m2
points(c(0.17,0.15,0.36,0.61,0.79,1.03,1.76,2.76,3.82,4.79),c(1.098,0.740,0.359,0.239,0.185,0.149,0.101,0.112,0.076,0.059),pch=16, col= "black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "30mg/m^2 (Chan 1978)", pos = 4, cex = 1.0, col = "black", font = 2)


#Camaggi 1988 Dose:60 bolus Age:42-72 Cancer
points(c(0.25,0.5,1,2,4,8,12,24,36,48,72,96,120,144,168), c(1.1339,0.1991,0.0983,0.0647,0.0473,0.04,0.0291,0.0246,0.0178,0.015,0.0113,0.0079,0.0049,0.0039,0.0031), pch=16, col="black")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.8) , pch = 16, col = "black", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.8) , labels = "60mg/m^2 (Camaggi 1988)", pos = 4, cex = 1.0, col = "black", font = 2)
observed_time <- c(0.25,0.5,1,2,4,8,12,24,36,48,72,96,120,144,168)
observed_concentration <- c(1.1339,0.1991,0.0983,0.0647,0.0473,0.04,0.0291,0.0246,0.0178,0.015,0.0113,0.0079,0.0049,0.0039,0.0031)

  
#Johnson, 1992 Dose:60 bolus Age:20-76 Hepatocellular carcinoma
points(c(0.176,0.204,0.377,0.559,1.058,2.064,3.06,4.297,12.574,20.444,24.264,30.606,48.564,72.481),c(6.757,1.946,0.574,0.366,0.166,0.142,0.115,0.0897,0.0622,0.0476,0.0413,0.0348,0.0242,0.0139), pch=16, col= "#CC79A7")
points(x = max(results$time) * 0.35, y = max(results$PL* 0.4) , pch = 16, col = "#CC79A7", cex = 1.0)
text(x = max(results$time) * 0.36, y = max(results$PL* 0.4) , labels = "60mg/m^2 (Johnson, 1992)", pos = 4, cex = 1.0, col = "#CC79A7", font = 2)
observed_time <- c(0.176,0.204,0.377,0.559,1.058,2.064,3.06,4.297,12.574,20.444,24.264,30.606,48.564,72.481)
  observed_concentration <- c(6.757,1.946,0.574,0.366,0.166,0.142,0.115,0.0897,0.0622,0.0476,0.0413,0.0348,0.0242,0.0139)

#Multiple doses
  #Speth, 1987 Dose:9 mg/m2 per day (36 mg/m2 in total) Advanced multiple myeloma
  points(c(1,2.8,5.7,19.5,23.8,27.7,47.8,56.6,71.7,81.9,93.5,95.6,99.6,105.6,118.3,142.5,167.6,191.5),c(0.00378,0.00392,0.0149,0.0172,0.0196,0.0255,0.0237,0.0248,0.0303,0.0279,0.0295,0.0314,0.0126,0.0136,0.0093,0.00577,0.00253,0.00329), pch=16, col= "black")
  points(x = max(results$time) * 0.05, y = max(results$PL* 0.05) , pch = 16, col = "black", cex = 1.0)
  text(x = max(results$time) * 0.06, y = max(results$PL* 0.05) , labels = "9 mg/m2 per day (Speth 1987)", pos = 4, cex = 1.0, col = "black", font = 2)
  
  #Muller, 1993 9 mg/m2 per day (36 mg/m2 in total) Age:70 Cancer
  points(c(24.0,48.3,71.9,96.4,96.1,97.9,100.2,104.6,120.1,144.4,168.6,216.1),c(0.0213,0.0222,0.0293,0.0261,0.0228,0.0196,0.0178,0.0175,0.0169,0.014,0.0104,0.008), pch=16, col= "#CC79A7")
  points(x = max(results$time) * 0.05, y = max(results$PL* 0.07) , pch = 16, col = "#CC79A7", cex = 1.0)
  text(x = max(results$time) * 0.06, y = max(results$PL* 0.07) , labels = "9 mg/m2 per day (Muller 1993)", pos = 4, cex = 1.0, col = "#CC79A7", font = 2)
  
  #Bugat 1989 Dose:15mg/m2/day (60 mg/m2 in total) infusion iv Age: 43-70 Cancer
  points(c(4.5,12.3,24.6,36.5,48.5,60.3,72.2,84.0,96.3,98.3,108.1,111.9,119.9,132.3,144.2),c(0.0194,0.0291,0.0358,0.0364,0.0321,0.0357,0.036,0.0437,0.0368,0.0277,0.0219,0.0212,0.0184,0.0162,0.015),pch=16, col= "black")
  points(x = max(results$time) * 0.15, y = max(results$PL* 0.35) , pch = 16, col = "black", cex = 1.0)
  text(x = max(results$time) * 0.16, y = max(results$PL* 0.35) , labels = "15 mg/m2 per day (Bugat 1989)", pos = 4, cex = 1.0, col = "black", font = 2)
  

  
# Solve the PBPK model to obtain the predicted concentrations at the time points
predicted <- results %>% filter(time %in% observed_time) %>% select(PL)
predicted <- as.numeric(predicted[,1])
{
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

cat("MAE:", mae_value, "\n")
cat("RMSE:", rmse_value, "\n")


#Calculate the Total Sum of Squares (TSS):
tss <- sum((observed_concentration - mean(observed_concentration))^2)
#Calculate the Residual Sum of Squares (RSS):
residuals <- observed_concentration - predicted
rss <- sum(residuals^2)
#Calculate R-squared:
r_squared <- 1 - (rss / tss)
cat("R²:", r_squared, "\n")

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

mape_value <- mape_percent(predicted, observed_concentration)
mpe_value <- mpe_percent(predicted, observed_concentration)
rmspe_value <- rmspe_percent(predicted, observed_concentration)

cat("MAPE%:", mape_value, "\n")
cat("MPE%:", mpe_value, "\n")
cat("RMSPE%:", rmspe_value, "\n")

# Prediction error (PE) {https://doi.org/10.1371/journal.pcbi.1008786}
PE <- function(predicted, observed_concentration) {
  PE <- ((observed_concentration - predicted) / (observed_concentration + predicted)/2)
  return(mean(PE))
}
PE_value <- PE(predicted, observed_concentration)

# Plasma area under curve AUC
library(pracma)
auc_plasma <- trapz(results$time, results$PL)
auc_plasma

#  Akaike Information Criterion (AIC)
residuals <- observed_concentration - predicted
sigma_sq <- var(residuals) #Calculate the variance of the residuals, which represents the spread of the differences between observed and predicted concentrations.
log_likelihood <- -0.5 * sum(((residuals)^2) / sigma_sq + log(2 * pi * sigma_sq))
AIC = -2 * log(log_likelihood) + 2 * k
}






