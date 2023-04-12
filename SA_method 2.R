library(deSolve)
library(sensitivity)
library(Bolstad)
library(pracma)
library(ggplot2)
library(reshape2)
options(max.print = 1e5)

compartments <- list( "Cre"="Cre",
                      "Cve"="Cve",
                      "Car"="Car",
                      
                      "Che_ec"="Che_ec",
                      "Ca_ec"="Ca_ec",
                      "Cbo_ec"="Cbo_ec",
                      "Cbr_ec"="Cbr_ec",
                      "Cgu_ec"="Cgu_ec",
                      "Cki_ec"="Cki_ec",
                      "Cli_ec"="Cli_ec",
                      "Clu_ec"="Clu_ec",
                      "Cmu_ec"="Cmu_ec",
                      "Csk_ec"="Csk_ec",
                      "Csp_ec"="Csp_ec",
                      
                      "Cmyo_ict"="Cmyo_ict",
                      "Cother_ict"="Cother_ict",
                      "Ca_ict"="Ca_ict",
                      "Cbo_ict"="Cbo_ict",
                      "Cbr_ict"="Cbr_ict",
                      "Cgu_ict"="Cgu_ict",
                      "Cki_ict"="Cki_ict",
                      "Cli_ict"="Cli_ict",
                      "Clu_ict"="Clu_ict",
                      "Cmu_ict"="Cmu_ict",
                      "Csk_ict"="Csk_ict",
                      "Csp_ict"="Csp_ict",
                      
                      "Cbloocell"="Cbloocell",
                      "Cbloocell_E1R"="Cbloocell_E1R",
                      "Cbloocell_E1"="Cbloocell_E1",
                      
                      "Cmyo_E1R"="Cmyo_E1R",
                      "Cmyo_E1"="Cmyo_E1",
                      "Cmyo_E2R"="Cmyo_E2R",
                      "Cmyo_E2"="Cmyo_E2",
                      "Cmyo_E3R"="Cmyo_E3R",
                      "Cmyo_E3"="Cmyo_E3",
                      
                      "Cother_E1R"="Cother_E1R",
                      "Cother_E1"="Cother_E1",
                      "Cother_E2R"="Cother_E2R",
                      "Cother_E2"="Cother_E2",
                      "Cother_E3R"="Cother_E3R",
                      "Cother_E3"="Cother_E3",
                      
                      "Cad_E1R"="Cad_E1R",
                      "Cad_E1"="Cad_E1",
                      "Cad_E2R"="Cad_E2R",
                      "Cad_E2"="Cad_E2",
                      "Cad_E3R"="Cad_E3R",
                      "Cad_E3"="Cad_E3",
                      
                      "Cbo_E1R"="Cbo_E1R",
                      "Cbo_E1"="Cbo_E1",
                      "Cbo_E2R"="Cbo_E2R",
                      "Cbo_E2"="Cbo_E2",
                      "Cbo_E3R"="Cbo_E3R",
                      "Cbo_E3"="Cbo_E3",
                      
                      "Cbr_E1R"="Cbr_E1R",
                      "Cbr_E1"="Cbr_E1",
                      "Cbr_E2R"="Cbr_E2R",
                      "Cbr_E2"="Cbr_E2",
                      "Cbr_E3R"="Cbr_E3R",
                      "Cbr_E3"="Cbr_E3",
                      
                      "Cgu_E1R"="Cgu_E1R",
                      "Cgu_E1"="Cgu_E1",
                      "Cgu_E2R"="Cgu_E2R",
                      "Cgu_E2"="Cgu_E2",
                      "Cgu_E3R"="Cgu_E3R",
                      "Cgu_E3"="Cgu_E3",
                      
                      "Cki_E1R"="Cki_E1R",
                      "Cki_E1"="Cki_E1",
                      "Cki_E2R"="Cki_E2R",
                      "Cki_E2"="Cki_E2",
                      "Cki_E3R"="Cki_E3R",
                      "Cki_E3"="Cki_E3",
                      
                      "Cli_E1R"="Cli_E1R",
                      "Cli_E1"="Cli_E1",
                      "Cli_E2R"="Cli_E2R",
                      "Cli_E2"="Cli_E2",
                      "Cli_E3R"="Cli_E3R",
                      "Cli_E3"="Cli_E3",
                      
                      "Clu_E1R"="Clu_E1R",
                      "Clu_E1"="Clu_E1",
                      "Clu_E2R"="Clu_E2R",
                      "Clu_E2"="Clu_E2",
                      "Clu_E3R"="Clu_E3R",
                      "Clu_E3"="Clu_E3",
                      
                      "Cmu_E1R"="Cmu_E1R",
                      "Cmu_E1"="Cmu_E1",
                      "Cmu_E2R"="Cmu_E2R",
                      "Cmu_E2"="Cmu_E2",
                      "Cmu_E3R"="Cmu_E3R",
                      "Cmu_E3"="Cmu_E3",
                      
                      "Csk_E1R"="Csk_E1R",
                      "Csk_E1"="Csk_E1",
                      "Csk_E2R"="Csk_E2R",
                      "Csk_E2"="Csk_E2",
                      "Csk_E3R"="Csk_E3R",
                      "Csk_E3"="Csk_E3",
                      
                      "Csp_E1R"="Csp_E1R",
                      "Csp_E1"="Csp_E1",
                      "Csp_E2R"="Csp_E2R",
                      "Csp_E2"="Csp_E2",
                      "Csp_E3R"="Csp_E3R",
                      "Csp_E3"="Csp_E3" ) #used as input in function, compartments that are used in pbpk

# Declare the mean value of each Substance-Specific parameter 
groupped.prior <- c(
  BP = BP,
  fup = fup,
  DNA_li = DNA_li,
  DNA_he = DNA_he,
  DNA_ki = DNA_ki,
  DNA_bo = DNA_bo,
  DNA_gu = DNA_gu,
  DNA_mu = DNA_mu,
  DNA_ad = DNA_ad,
  DNA_sk = DNA_sk,
  DNA_sp = DNA_sp,
  DNA_br = DNA_br,
  DNA_lu = DNA_lu, 
  DNA_bloodcell = DNA_bloodcell,
  
  mtDNA_li = mtDNA_li,
  mtDNA_he = mtDNA_he,
  mtDNA_ki = mtDNA_ki,
  mtDNA_bo = mtDNA_bo,
  mtDNA_gu = mtDNA_gu,
  mtDNA_mu = mtDNA_mu,
  mtDNA_ad = mtDNA_ad,
  mtDNA_sk = mtDNA_sk,
  mtDNA_sp = mtDNA_sp,
  mtDNA_br = mtDNA_br,
  mtDNA_lu = mtDNA_lu, 
  
  Cardiolipin_li = Cardiolipin_li,
  Cardiolipin_he = Cardiolipin_he,
  Cardiolipin_ki = Cardiolipin_ki,
  Cardiolipin_bo = Cardiolipin_bo,
  Cardiolipin_gu = Cardiolipin_gu,
  Cardiolipin_mu = Cardiolipin_mu,
  Cardiolipin_ad = Cardiolipin_ad,
  Cardiolipin_sk = Cardiolipin_sk,
  Cardiolipin_sp = Cardiolipin_sp,
  Cardiolipin_br = Cardiolipin_br,
  Cardiolipin_lu = Cardiolipin_lu, 
  
  Kon_cardiolipin = Kon_cardiolipin,
  Koff_cardiolipin = Koff_cardiolipin,
  Kon_DNA = Kon_DNA,
  Koff_DNA = Koff_DNA,
  
  Kon_mtDNA = Kon_mtDNA,
  Koff_mtDNA = Koff_mtDNA,
  
  CL_renal = CL_renal,
  CL_hepatic = CL_hepatic)

# "init_prior" is the vector of all Substance-Specific parameters, 
# considering that each compartment has its own independent parameters
init_prior <- c(
  BP = BP,
  fup = fup,
  DNA_li = DNA_li,
  DNA_he = DNA_he,
  DNA_ki = DNA_ki,
  DNA_bo = DNA_bo,
  DNA_gu = DNA_gu,
  DNA_mu = DNA_mu,
  DNA_ad = DNA_ad,
  DNA_sk = DNA_sk,
  DNA_sp = DNA_sp,
  DNA_br = DNA_br,
  DNA_lu = DNA_lu, 
  DNA_bloodcell = DNA_bloodcell,
  
  mtDNA_li = mtDNA_li,
  mtDNA_he = mtDNA_he,
  mtDNA_ki = mtDNA_ki,
  mtDNA_bo = mtDNA_bo,
  mtDNA_gu = mtDNA_gu,
  mtDNA_mu = mtDNA_mu,
  mtDNA_ad = mtDNA_ad,
  mtDNA_sk = mtDNA_sk,
  mtDNA_sp = mtDNA_sp,
  mtDNA_br = mtDNA_br,
  mtDNA_lu = mtDNA_lu, 
  
  Cardiolipin_li = Cardiolipin_li,
  Cardiolipin_he = Cardiolipin_he,
  Cardiolipin_ki = Cardiolipin_ki,
  Cardiolipin_bo = Cardiolipin_bo,
  Cardiolipin_gu = Cardiolipin_gu,
  Cardiolipin_mu = Cardiolipin_mu,
  Cardiolipin_ad = Cardiolipin_ad,
  Cardiolipin_sk = Cardiolipin_sk,
  Cardiolipin_sp = Cardiolipin_sp,
  Cardiolipin_br = Cardiolipin_br,
  Cardiolipin_lu = Cardiolipin_lu, 
  
  Kon_cardiolipin = Kon_cardiolipin,
  Koff_cardiolipin = Koff_cardiolipin,
  Kon_DNA = Kon_DNA,
  Koff_DNA = Koff_DNA,
  
  Kon_mtDNA = Kon_mtDNA,
  Koff_mtDNA = Koff_mtDNA,
  
  CL_renal = CL_renal,
  CL_hepatic = CL_hepatic
)

# Create the "PBPK_function". The input of PBPK_function is a data frame,
# that each row is a sample for the Substance_specific parameters. So the input "X" 
# is a data frame [N_samples x N_params] created by the GSA method
PBPK <- function(X){
  # Define the initial conditions 
  state <- c(
    INFUSION = r,
    Cre = 0,
    Cve = 0,
    Car = 0,
    
    Che_ec = 0,
    Cad_ec = 0,
    Cbo_ec = 0,
    Cbr_ec = 0,
    Cgu_ec = 0,
    Cki_ec = 0,
    Cli_ec = 0,
    Clu_ec = 0,
    Cmu_ec = 0,
    Csk_ec = 0,
    Csp_ec = 0,
    
    Cmyo_ict = 0,
    Cother_ict = 0,
    Cad_ict = 0,
    Cbo_ict = 0,
    Cbr_ict = 0,
    Cgu_ict = 0,
    Cki_ict = 0,
    Cli_ict = 0,
    Clu_ict = 0,
    Cmu_ict = 0,
    Csk_ict = 0,
    Csp_ict = 0,
    Cbloodcell = 0,
    
    Cbloodcell_E1R = 0,
    Cbloodcell_E1 = DNA_bloodcell,
    
    Cmyo_E1R = 0,
    Cmyo_E1 = DNA_myo,
    Cmyo_E2R = 0,
    Cmyo_E2 = Cardiolipin_myo,
    Cmyo_E3R = 0,
    Cmyo_E3 = mtDNA_myo,
    
    Cother_E1R = 0,
    Cother_E1 = DNA_other,
    Cother_E2R = 0,
    Cother_E2 = Cardiolipin_other,
    Cother_E3R = 0,
    Cother_E3 = mtDNA_other,
    
    Cad_E1R = 0,
    Cad_E1 = DNA_ad,
    Cad_E2R = 0,
    Cad_E2 = Cardiolipin_ad,
    Cad_E3R = 0,
    Cad_E3 = mtDNA_ad,
    
    Cbo_E1R = 0,
    Cbo_E1 = DNA_bo,
    Cbo_E2R = 0,
    Cbo_E2 = Cardiolipin_bo,
    Cbo_E3R = 0,
    Cbo_E3 = mtDNA_bo,
    
    Cbr_E1R = 0,
    Cbr_E1 = DNA_br,
    Cbr_E2R = 0,
    Cbr_E2 = Cardiolipin_br,
    Cbr_E3R = 0,
    Cbr_E3 = mtDNA_br,
    
    Cgu_E1R = 0,
    Cgu_E1 = DNA_gu,
    Cgu_E2R = 0,
    Cgu_E2 = Cardiolipin_gu,
    Cgu_E3R = 0,
    Cgu_E3 = mtDNA_gu,
    
    Cki_E1R = 0,
    Cki_E1 = DNA_ki,
    Cki_E2R = 0,
    Cki_E2 = Cardiolipin_ki,
    Cki_E3R = 0,
    Cki_E3 = mtDNA_ki,
    
    Cli_E1R = 0,
    Cli_E1 = DNA_li,
    Cli_E2R = 0,
    Cli_E2 = Cardiolipin_li,
    Cli_E3R = 0,
    Cli_E3 = mtDNA_li,
    
    Clu_E1R = 0,
    Clu_E1 = DNA_lu,
    Clu_E2R = 0,
    Clu_E2 = Cardiolipin_lu,
    Clu_E3R = 0,
    Clu_E3 = mtDNA_lu,
    
    Cmu_E1R = 0,
    Cmu_E1 = DNA_mu,
    Cmu_E2R = 0,
    Cmu_E2 = Cardiolipin_mu,
    Cmu_E3R = 0,
    Cmu_E3 = mtDNA_mu,
    
    Csk_E1R = 0,
    Csk_E1 = DNA_sk,
    Csk_E2R = 0,
    Csk_E2 = Cardiolipin_sk,
    Csk_E3R = 0,
    Csk_E3 = mtDNA_sk,
    
    Csp_E1R = 0,
    Csp_E1 = DNA_sp,
    Csp_E2R = 0,
    Csp_E2 = Cardiolipin_sp,
    Csp_E3R = 0,
    Csp_E3 = mtDNA_sp
  )  
  
# Create a matrix to store the results of the ODEs system for each parametric sample
  output <- matrix(0, nrow = nrow(X), ncol = 12)
  
# Define the CV of each parameter
  cv <- rep(NA, length(init_prior))
  for (p in 1:length(init_prior)) {
    cv[p] <- 50/100
  }
  
  eta <- init_prior
  std <- cv*eta
#Tranform from Normal to lognormal
  eta_tr <- log((eta^2)/sqrt((eta^2)+std^2))
  std_tr <- sqrt(log(1 + (std^2)/(eta^2)))
  
# Calculate the solution of the ODEs sytem, considering the initial mean values of the parameters.
# This solution will replace the "NA" values which will occur from solution of the ODEs system 
# for some parametric samples
  average_run <-  solution <- ode(times = times, func = PBPKModel, y = state,
                                  parms = c(eta,parameters), method = "bdf")
  error_counter<<-0 # a counter to count how many times the integration failed
  success <- rep(NA, nrow(X))
  for (i in 1:nrow((X))) {
    print(paste("We are at iteration", i, sep = "  "))
    
# Inverse transform the sample from uniform[0,1] to the lognormal distributions of the Substance-Specific parameters
    u <- as.numeric(X[i,])
    inv_cdf <- exp(eta_tr + sqrt(2)*std_tr*erfinv(2*u - 1))
    names(inv_cdf) <-names(parameters)
    params <- c(inv_cdf,parameters)
    solution <- ode(times = times, func = PBPKModel, y = state, parms = params, method = "bdf")
    
# Check if the current parametric sample gave a "normal" ODEs solution (not "NA" values).
# If not, then replace this solution with the average solution (calculated above) in the output matrix
    if (dim(solution)[1] != dim(average_run)[1]){
      solution <- average_run
      error_counter = error_counter + 1
      success[i] = FALSE  
    }else{
      success[i] = TRUE
    }
    Total_amounts <- solution[,35:46]
    
    for (k in 1:ncol(Total_amounts)) {
#Integrate the "Time" dimension by calculating the AUC of the "Total" curve of each compartment. 
      AUC <- sintegral(times, Total_amounts[,k], n.pts = 256)
      output[i,k] <- as.numeric(AUC["value"])
    }
    success <<- success
  }
#Return a matrix that each row contains the AUC results for all the compartments, of the corresponding parametric sample
  return(output)
}

# Define the GSA method
# It is considered that the sampling distribution is uniform(0,1) and the sample is transformed to 
# lognormal inside the PBPK function. In addition, it is strongly recommended to set "scale=TRUE", in order to scale
# the output matrix

n<-10 # Number of samples for each parameter
X1 <- data.frame(matrix(runif(length(init_prior) * n, min = 0, max = 1), nrow = n))
X2 <- data.frame(matrix(runif(length(init_prior) * n, min = 0, max = 1), nrow = n))
sobol_indices <- sobol2002(model=NULL, X1 = X1, X2 = X2, nboot = 0, conf = 0.95)

# Calculate the results of the model for each parametric sample
y <- PBPK(sobol_indices$X)

#  Normalize the columns of matrix y
for (i in 1:ncol(y)) {
  mu <- mean(y[,i])
  sigma <- sd(y[,i])
  for (k in 1:nrow(y)) {
    y[k,i] <- (y[k,i]-mu)/sigma
  }
}

### Prepare the matrices to store the first and total sobol indeces of each parameter
S_1 <- matrix(NA, nrow = length(init_prior), ncol = 101)
S_total <- S_1
rownames(S_1) <- param_names <- c( "BP",
                                   "fup",
                                   "DNA_li",
                                   "DNA_he",
                                   "DNA_ki",
                                   "DNA_bo",
                                   "DNA_gu",
                                   "DNA_mu",
                                   "DNA_ad",
                                   "DNA_sk",
                                   "DNA_sp",
                                   "DNA_br",
                                   "DNA_lu", 
                                   "DNA_bloodcell",
                                   
                                   "mtDNA_li",
                                   "mtDNA_he",
                                   "mtDNA_ki",
                                   "mtDNA_bo",
                                   "mtDNA_gu",
                                   "mtDNA_mu",
                                   "mtDNA_ad",
                                   "mtDNA_sk",
                                   "mtDNA_sp",
                                   "mtDNA_br",
                                   "mtDNA_lu", 
                                   
                                   "Cardiolipin_li",
                                   "Cardiolipin_he",
                                   "Cardiolipin_ki",
                                   "Cardiolipin_bo",
                                   "Cardiolipin_gu",
                                   "Cardiolipin_mu",
                                   "Cardiolipin_ad",
                                   "Cardiolipin_sk",
                                   "Cardiolipin_sp",
                                   "Cardiolipin_br",
                                   "Cardiolipin_lu", 
                                   
                                   "Kon_cardiolipin",
                                   "Koff_cardiolipin",
                                   "Kon_DNA",
                                   "Koff_DNA",
                                   
                                   "Kon_mtDNA",
                                   "Koff_mtDNA",
                                   
                                   "CL_renal",
                                   "CL_hepatic")
colnames(S_1) <- c(          "Cre",
                             "Cve",
                             "Car",
                             
                             "Che_ec",
                             "Ca_ec",
                             "Cbo_ec",
                             "Cbr_ec",
                             "Cgu_ec",
                             "Cki_ec",
                             "Cli_ec",
                             "Clu_ec",
                             "Cmu_ec",
                             "Csk_ec",
                             "Csp_ec",
                             
                             "Cmyo_ict",
                             "Cother_ict",
                             "Ca_ict",
                             "Cbo_ict",
                             "Cbr_ict",
                             "Cgu_ict",
                             "Cki_ict",
                             "Cli_ict",
                             "Clu_ict",
                             "Cmu_ict",
                             "Csk_ict",
                             "Csp_ict",
                             
                             "Cbloocell",
                             "Cbloocell_E1R",
                             "Cbloocell_E1",
                             
                             "Cmyo_E1R",
                             "Cmyo_E1",
                             "Cmyo_E2R",
                             "Cmyo_E2",
                             "Cmyo_E3R",
                             "Cmyo_E3",
                             
                             "Cother_E1R",
                             "Cother_E1",
                             "Cother_E2R",
                             "Cother_E2",
                             "Cother_E3R",
                             "Cother_E3",
                             
                             "Cad_E1R",
                             "Cad_E1",
                             "Cad_E2R",
                             "Cad_E2",
                             "Cad_E3R",
                             "Cad_E3",
                             
                             "Cbo_E1R",
                             "Cbo_E1",
                             "Cbo_E2R",
                             "Cbo_E2",
                             "Cbo_E3R",
                             "Cbo_E3",
                             
                             "Cbr_E1R",
                             "Cbr_E1",
                             "Cbr_E2R",
                             "Cbr_E2",
                             "Cbr_E3R",
                             "Cbr_E3",
                             
                             "Cgu_E1R",
                             "Cgu_E1",
                             "Cgu_E2R",
                             "Cgu_E2",
                             "Cgu_E3R",
                             "Cgu_E3",
                             
                             "Cki_E1R",
                             "Cki_E1",
                             "Cki_E2R",
                             "Cki_E2",
                             "Cki_E3R",
                             "Cki_E3",
                             
                             "Cli_E1R",
                             "Cli_E1",
                             "Cli_E2R",
                             "Cli_E2",
                             "Cli_E3R",
                             "Cli_E3",
                             
                             "Clu_E1R",
                             "Clu_E1",
                             "Clu_E2R",
                             "Clu_E2",
                             "Clu_E3R",
                             "Clu_E3",
                             
                             "Cmu_E1R",
                             "Cmu_E1",
                             "Cmu_E2R",
                             "Cmu_E2",
                             "Cmu_E3R",
                             "Cmu_E3",
                             
                             "Csk_E1R",
                             "Csk_E1",
                             "Csk_E2R",
                             "Csk_E2",
                             "Csk_E3R",
                             "Csk_E3",
                             
                             "Csp_E1R",
                             "Csp_E1",
                             "Csp_E2R",
                             "Csp_E2",
                             "Csp_E3R",
                             "Csp_E3")
rownames(S_total) <- rownames(S_1)
colnames(S_total) <- colnames(S_1)

# Calculate the Sobol indexes based on the X1 and X2 sample data frames, which produced the "y" output matrix
for (i in 1:12) {
  tell(sobol_indices,y[,i])
  S_1[,i] <- unlist(sobol_indices[["S"]])
  S_total[,i] <- unlist(sobol_indices[["T"]])
}

# Integration to eliminate the "Compartments" parameters.
# The calculated weights are based on the AUC of mass collected in each compartment,

sobol_data <- melt(sobol_data) #transform to long format the data

Sobol_plot <- ggplot(sobol_indices, aes(param_names, sobol_indices$T, fill=variable))+
  geom_bar(stat = "identity", position = "dodge" )+
  
  labs(title = "Sobol Global Sensitivity", x = "Parameters" )+
  theme(axis.text.x = element_text(size=15 ,angle=45),
        axis.text.y = element_text(size=15),
        plot.title = element_text(hjust = 0.5,size=26),
        axis.title.y =element_text(hjust = 0.5,size=18,face="bold"),
        axis.title.x =element_text(hjust = 0.5,size=18,face="bold"),
        #axis.text.x=element_text(size=18),
        legend.title=element_text(hjust = 0.5,size=18))
dev.off()

print(Sobol_plot)

################################################################################################
# Plots Sobol indexes for each compartment
# Define the path where the plots should be saved automatically
setwd("__")
comp_names <- c(  "BP",
                  "fup",
                  "DNA_li",
                  "DNA_he",
                  "DNA_ki",
                  "DNA_bo",
                  "DNA_gu",
                  "DNA_mu",
                  "DNA_ad",
                  "DNA_sk",
                  "DNA_sp",
                  "DNA_br",
                  "DNA_lu", 
                  "DNA_bloodcell",
                  
                  "mtDNA_li",
                  "mtDNA_he",
                  "mtDNA_ki",
                  "mtDNA_bo",
                  "mtDNA_gu",
                  "mtDNA_mu",
                  "mtDNA_ad",
                  "mtDNA_sk",
                  "mtDNA_sp",
                  "mtDNA_br",
                  "mtDNA_lu", 
                  
                  "Cardiolipin_li",
                  "Cardiolipin_he",
                  "Cardiolipin_ki",
                  "Cardiolipin_bo",
                  "Cardiolipin_gu",
                  "Cardiolipin_mu",
                  "Cardiolipin_ad",
                  "Cardiolipin_sk",
                  "Cardiolipin_sp",
                  "Cardiolipin_br",
                  "Cardiolipin_lu", 
                  
                  "Kon_cardiolipin",
                  "Koff_cardiolipin",
                  "Kon_DNA",
                  "Koff_DNA",
                  
                  "Kon_mtDNA",
                  "Koff_mtDNA",
                  
                  "CL_renal",
                  "CL_hepatic")
for (i in 1:length(comp_names)) {
  sobol.df <- data.frame(param_names, S_1[,i], S_total[,i])
  sobol.df <- melt(sobol.df)
  comp_name <- comp_names[i]
  save_name<-paste0(comp_name, ".png", sep="")
  
  Sobol_plot <- ggplot(sobol.df, aes(param_names, value, fill=variable))+
    geom_bar(stat = "identity", position = "dodge" )+
    
    labs(title = paste0("Sobol Sensitivity Indices for ", rlang::expr(!!comp_name)), x = "Parameters", y= "Sensitivity Index" )+
    theme(axis.text.x = element_text(size=15 ,angle=45),
          axis.text.y = element_text(size=15),
          plot.title = element_text(hjust = 0.5,size=26),
          axis.title.y =element_text(hjust = 0.5,size=18,face="bold"),
          axis.title.x =element_text(hjust = 0.5,size=18,face="bold"),
          #axis.text.x=element_text(size=18),
          legend.title=element_text(hjust = 0.5,size=18))
  png(rlang::expr(!!save_name), width = 1920, height = 1080, units = 'px')
  print(Sobol_plot)
  
  dev.off()
}




