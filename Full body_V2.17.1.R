# Based on V 2.14
# INTERINDIVIDUAL VARIABILITY for males
rm(list=ls(all=TRUE))

#REQUIRED PACKAGES:
{
  require(distr)
  require(data.table)
  require(dplyr)
  require(ggplot2)
  require(deSolve)
  require(plotly)
  require(openxlsx)
  require(ggquickeda)
}

{
  N <- 10
  female_count <- 0
  #age (y)
  age <- runif(N,18,50)     #age (range) multiple virtual male individuals (uniform distribution)
  
  #Height(cm)/Weight(kg)/BSA(m2) MALE
  Height_male <- runif(N,160,190) 
  Weight_male <- runif(N,50,100) 
  
  assign('Population',
         data.frame(
           sex = c(rep("M", N)),
           age = c(age),
           Height_male = c(Height_male),
           Weight_male = c(Weight_male)
         ))
  
  #CARDIAC OUTPUT IS AGE, SEX AND SURFACE AREA DEPENDANT. IT IS CALCULATED ALGEBRAICALLY ACCORDING TO THE FORMULAS IN [Tanner 1949]
  # CARDIAC OUTPUT ----------------------------------------------------------
  Population["BSA_male"] <-
    (Population$Weight_male ^ 0.425 * Population$Height_male ^ 0.725) * 0.007184 #Body surface area according to [DuBois-DuBois 1916]
  
  Population["CO"] <- 
    ifelse(
      Population$sex == "M",
      (1.1 * Population$BSA_male - 0.05 * Population$age + 5.5),
      (1.7 * Population$BSA_male - 0.04 * Population$age + 3.5))
  
  Population["CO"] <- Population["BSA_male"] * (110 + (184.974 * (exp(-0.0378 * Population$age) -exp(-0.2477 * Population$age)))) ##[journal.pcbi.1008786]

  Sample_time <- 0.01
  t_end <- 24 #[h] time of the end of simulation
  times <- seq(0, t_end, by = Sample_time) #time of simulation
  
  pKa <- 8.46 # [amine]
  MW <- 543.52 # g/mol
  
  oral_dose <- 0 #[mg] oral bolus dose
  inf_time <- 3/60 #[h] infusion time
  
  #SEX DEPENDENT BLOOD FLOWS for healthy population according to [journal.pcbi.1008786]
  # BLOOD FLOWS [L/h] -------------------------------------------------------
  Population["Qad"] <-
    ifelse(
      Population$sex == "M",
      Population$CO * 0.05,
      Population$CO * 0.085
    )
  Population["Qbo"] <- Population$CO * 0.05
  Population["Qbr"] <- Population$CO * ((10 + 2290 *(exp(-0.608*Population$age) - exp(-0.639*Population$age)))/100)
  
  Population["Qhe"] <-
    ifelse(
      Population$sex == "M",
      Population$CO * 0.04,
      Population$CO * 0.05
    )
  
  Population["Qki"] <-
    ifelse(
      Population$sex == "M",
      Population$CO * (( 4.53 + (14.63*Population$age^1/(0.188^1 + Population$age^1)))/100),
      Population$CO * 0.17
    )
  
  Population["Qmu"] <-
    ifelse(
      Population$sex == "M",
      Population$CO * ((6.03 + (12*Population$age^2.5/(11^2.5 + Population$age^2.5)))/100),
      Population$CO * 0.12
    )
  
  Population["Qsk"] <- Population$CO * ((1.0335 + (4*Population$age^5/(5.16^5 + Population$age^5)))/100)
  
  Population["Qgu"] <-
    ifelse(
      Population$sex == "M",
      Population$CO * 0.16,
      Population$CO * 0.17
    )
  
  Population["Qha"] <-
    ifelse(
      Population$sex == "M",
      Population$CO * 0.065,
      Population$CO * 0.065
    )
  
  Population["Qsp"] <-
    ifelse(
      Population$sex == "M",
      Population$CO * 0.02,
      Population$CO * 0.03
    )
  
  Population["Qli"] <- Population$Qgu + Population$Qha + Population$Qsp
  
  Population["Qre"] <- Population$CO - Population$Qbr - Population$Qad - Population$Qbo - Population$Qhe - Population$Qki - Population$Qmu - Population$Qsk - Population$Qsp - Population$Qgu - Population$Qha
  Population["Qre"] <- ifelse (Population$Qre>0,Population$Qre,0 )
  
  Population["Qlu"] <- Population$CO
  
  #control whether the sum of tissue blood flowa is equal to total cardiac output
  Population["CO - Qsum"] <-
    Population$CO - (apply(Population[, c(7:16,18)], 1, sum))
  
  # ORGAN VOLUMES [L] (according to [journal.pcbi.1008786])-----------------------------------------------------------
  #BW fraction (according to Simcyp Simulator) * random BW / tissue density
  #Population["Vad"] <- (1.36 * Population$Weight_male) / (Population$Height_male/ 100) - 42
  Population["Vad"] <- (0.259 * Population$Weight_male) / 0.923
  Population["Vbo"] <- (0.090 * Population$Weight_male) / 1.850
  Population["Vbr"] <- (10 * (Population$age + 0.315) / (9 + 6.92 * Population$age)) / 1.04
  Population["Vgu"] <- (0.021* (Population$Weight_male - Population$Vad * 0.92))/1.05
  Population["Vhe"] <- (( 22.81 * (Population$Height_male/100) * Population$Weight_male^0.5 -4.15)/1000)/1.05
  Population["Vki"] <- (4.214 * Population$Weight_male^0.823 + 4.456*Population$Weight_male^0.795)/1000
  Population["Vli"] <- ((576.9*(Population$Height_male/100) + 8.9*Population$Weight_male - 159.7)/1000)/1.05
  Population["Vlu"] <- ((29.08 * (Population$Height_male/ 100) * Population$Weight_male ^ 0.5 + 11.06 + 35.47 * (Population$Height_male/100) * Population$Weight_male^0.5 +5.53)/1000)/1.05
  Population["Vmu"] <- (0.3 + ((0.54-0.3)/18)* Population$age) * (Population$Weight_male - Population$Vad * 0.92)/1.04
  Population["Vsk"] <- (Population$BSA_male/1000) * 45.655+ (Population$BSA_male/1000) * 1240
  Population["Vsp"] <- ((8.74 * (Population$Height_male /100) * Population$Weight_male^0.5 + 11.06)/1000)*1.06
  Population["Vre"] <- (0.057 * Population$Weight_male) / 1.05
  Population["Vpl"] <- (0.044 * Population$Weight_male) / 1.025
  Population["Vrb"] <- (0.031 * Population$Weight_male) / 1.125
  Population["Vbl"] <- Population["Vpl"] + Population["Vrb"]
  
  #control the sum of body volumes:
  Population["V sum"] <- apply(Population[, c(21:34)], 1, sum)
  
  Population['Vbo_ic'] <- 22.5 / 100 * Population$Vbo
  Population['Vbo_ec'] <- 77.5 / 100 * Population$Vbo
  
  Population['Vbr_ic'] <- 87.5 / 100 * Population$Vbr
  Population['Vbr_ec'] <- 12.5 / 100 * Population$Vbr
  
  Population['Vsk_ic'] <- 87.5 / 100 * Population$Vsk
  Population['Vsk_ec'] <- 12.5 / 100 * Population$Vsk
  
  Population['Vli_ic'] <- 80.75 / 100 * Population$Vli
  Population['Vli_ec'] <- 19.25 / 100 * Population$Vli
  
  Population['Vki_ic'] <- 83.34 / 100 * Population$Vki
  Population['Vki_ec'] <- 16.66 / 100 * Population$Vki
  
  Population['Vlu_ic'] <- 72.89 / 100 * Population$Vlu
  Population['Vlu_ec'] <- 27.11 / 100 * Population$Vlu
  
  Population['Vmu_ic'] <- 72.5 / 100 * Population$Vmu
  Population['Vmu_ec'] <- 27.5 / 100 * Population$Vmu
  
  Population['Vre_ic'] <- 87.5 / 100 * Population$Vre
  Population['Vre_ec'] <- 12.5 / 100 * Population$Vre
  
  Population['Vsk_ic'] <- 72.5 / 100 * Population$Vsk
  Population['Vsk_ec'] <- 27.5 / 100 * Population$Vsk
  
  Population['Vsp_ic'] <- 74.50 / 100 * Population$Vsp
  Population['Vsp_ec'] <- 25.50 / 100 * Population$Vsp
  
  Population['Vgu_ic'] <- 82.09 / 100 * Population$Vgu
  Population['Vgu_ec'] <- 17.91 / 100 * Population$Vgu
  
  Population['Vad_ic'] <- 87.5 / 100 * Population$Vad
  Population['Vad_ec'] <- 12.5 / 100 * Population$Vad
  
  #Heart compartment volumes [L] -------------------------------------------------------
  Population['Vmyo'] <- 0.8 * Population$Vhe #myocardial
  Population['Vother'] <- 0.2 * Population$Vhe #other
  Population['Vother_ic'] <- 87.5 / 100 * Population$Vother #intracellular 
  Population['Vmyo_ic'] <- 87.5 / 100 * Population$Vmyo #intracellular of myocardial
  Population['Vhe_ec'] <- 12.5 / 100 * Population$Vhe #extracellular space of heart tissue
  Population['Vve'] <- (2 / 3) * Population$Vbl		#venous blood; assumed 2/3 of total blood according to volmues published in CPT. Regarding the distribution of blood volume within the circulation, the greatest volume resides in the venous vasculature, where 70-80% of the blood volume is found. -> http://www.cvphysiology.com/Blood%20Pressure/BP019
  Population['Var'] <- Population$Vbl - Population$Vve		#arterial blood
  Population['Vplas_ven'] <- Population$Vpl * (Population$Vve / (Population$Vve + Population$Var))  #venous plasma
  Population['Vplas_art'] <- Population$Vpl * (Population$Var / (Population$Vve + Population$Var)) 	#arterial plasma
  
  # CARDIOMYOCYTE VOLUME AND SURFACE AREA according to [Polak 2012] -------------------------------------------------------
  MV <- function(x) {
    sqrt(0.29602 ^ 2 + (c(1, x) %*% matrix(
      c(38.12228, -0.66728, -0.66728, 0.01639),
      nrow = 2,
      ncol = 2
    ) %*% t(t(c(
      1, x
    ))) / 1000))
  }
  Population["MV_SE"] <- as.vector(cbind(mapply(MV, Population$age))) #standard deviation of myocyte volume
  
  Population["MV"] <- as.vector(cbind(exp(rnorm(N, (Population$age * 0.04551 + 7.36346), Population$MV_SE)))) #[mcm^3]
  
  Population["MSA"] <- as.vector(cbind(exp(rnorm(N, 0.860 * log(Population$MV),
                                                 ((sqrt(
                                                   0.102 ^ 2 + (log(Population$MV)) ^ 2 * 0.002939 ^ 2
                                                 )))
  )))) #[mcm^2]
  
  Population['MVol'] <- as.vector(Population$MV * (10 ^ -15)) #[L] random age dependent myocate volume in um3 -> changing to liters
  
  #cells amounts -------------------------------------------------------
  Population['cell_amount_other'] <- Population$Vother_ic / Population$MVol #other heart tissue
  Population['cell_amount_myo'] <- Population$Vmyo_ic / Population$MVol #myocardial
  
  Population['SA_other'] <- (Population$cell_amount_other * Population$MSA) / (10 ^ 8) #[cm^2]
  Population['SA_myo'] <- (Population$cell_amount_myo * Population$MSA) / (10 ^ 8) #[cm^2]
  
}

{
  #mtDNA concentration calcalation (umol/L)
  br_cell <- 3 * 10^11 
  lu_cell <- 2.9 * 10^11 
  li_cell <- 3 * 10^11 
  ki_cell <- 10^10
  bo_cell <- 10^9
  he_cell <- 6 * 10^9
  mu_cell <- 1.5 * 10^10
  pa_cell <- 1.5 * 10^9
  
  # Assumption
  gu_cell <- 10^10
  ad_cell <- 10^10
  sk_cell <- 10^10
  sp_cell <- 10^10
  
  cn_li <- 2112
  cn_he <- 6216
  cn_ki <- 1162
  cn_mu <- 3230
  cn_br <- 1892
  cn_lu <- 250
  
  # Assumption
  cn_bo <- 1892
  cn_gu <- 1892
  cn_ad <- 1892
  cn_sk <- 1892
  cn_sp <- 1892
  
  #  1 ng of mtDNA corresponds to approximately 1.52 x 10^-9 μmol, assuming a molecular weight of 6.6 x 10^5 g/mol
  
  Population['mtDNA_li'] <- (li_cell * (cn_li * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vli
  Population['mtDNA_he'] <- (he_cell * (cn_he * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vhe
  Population['mtDNA_ki'] <- (ki_cell * (cn_ki * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vki
  Population['mtDNA_bo'] <- (bo_cell * (cn_bo * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vbo
  Population['mtDNA_gu'] <- (gu_cell * (cn_gu * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vgu
  Population['mtDNA_mu'] <- (mu_cell * (cn_mu * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vmu
  Population['mtDNA_ad'] <- (ad_cell * (cn_ad * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vad
  Population['mtDNA_sk'] <- (sk_cell * (cn_sk * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vsk
  Population['mtDNA_br'] <- (br_cell * (cn_br * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vbr
  Population['mtDNA_lu'] <- (lu_cell * (cn_lu * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vlu
  Population['mtDNA_sp'] <- (sp_cell * (cn_sp * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Population$Vsp
  
  Population['mtDNA_other'] <- 0.05 * Population$mtDNA_he
  Population['mtDNA_myo'] <- 0.95 * Population$mtDNA_he
  
}

Population["fup"] <- rlnorm(N, meanlog = log(0.26^2 / sqrt(0.18^2 + 0.26^2)), sdlog = sqrt(log(1 + (0.18^2 / 0.26^2))))

# TISSUE TO PLASMA PARTITION COEFFICIENT -------------------------------------------------------
{
  pH_iw <- 7 #pH of intracellular water
  pH_ew <- 7.4 #pH of extracellular water
  pH_rbc <- 7.15 #pH of red blood cells [swietach, 2010]
  logPow <- 1.27 
  P <- 10^logPow #the n octanol: buffer partition coefficient for non-adipose tissue and the olive oil buffer partition coefficient for adipose tissue
  X_rbc <- 1 + 10^(pKa-pH_rbc)
  Y_rbc <- 10^(pKa-pH_rbc)
  X_iw <- 1 + 10^(pKa-pH_iw)
  X_ew <- 1 + 10^(pKa-pH_ew)
  Y_iw <- 10^(pKa-pH_iw)
  logPvow <- (1.115 * logPow - 1.35) - log(X_ew) #For the partitioning into the adipose tissue, it is more accurate to use the vegetable oil:water partition coefficient
  Pad <- 10^ logPvow
  
  {#MALE tissue composition
    Population['EW_ad'] <- (32.154 - 2.7863 * log10(Population$age))*(14.1/18/100)       #EW: extracellular water
    Population['IW_ad'] <- (32.154 - 2.7863 * log10(Population$age))*(3.9/18/100)                     #IW: intracellular water
    Population['NL_ad'] <- (35.5 + 43.46 * Population$age /(1.5 + Population$age))*(79/79.2/100)                 #NL: neutral lipid
    Population['NP_ad'] <- (35.5 + 43.46 * Population$age /(1.5 + Population$age))*(0.2/79.2/100)                #NP: neutral phospholipid
    AP_ad <- 0.4 #mg/g the concentration of acidic phospholipids AP in adipose
    
    # Bone
    EW_bo <- 0.098
    IW_bo <- 0.341
    NL_bo <- 0.074
    NP_bo <- 0.0011
    AP_bo <- 0.67 #mg/g the concentration of acidic phospholipids AP in Bone
    
    Population['EW_gu'] <- (75.378 - 0.3932 * log10(Population$age))*(26.7/71.8/100)
    Population['IW_gu'] <- (75.378 - 0.3932 * log10(Population$age))*(45.1/71.8/100)
    Population['NL_gu'] <- (2.5 + 0.185 * Population$age)*(4.87/6.5/100)
    Population['NP_gu'] <- (2.5 + 0.185 * Population$age)*(1.63/6.5/100)
    AP_br <- 0.4 #mg/g the concentration of acidic phospholipids AP in Brain
    
    Population['EW_he'] <- (84.523 - 0.4249 * Population$age)*(31.3/75.8/100)
    Population['IW_he'] <- (84.523 - 0.4249 * Population$age)*(44.5/75.8/100)
    Population['NL_he'] <- (2.3159 + 0.0797 * Population$age)*(1.15/2.81/100)
    Population['NP_he'] <- (2.3159 + 0.0797 * Population$age)*(1.66/2.81/100)
    AP_gu <- 2.84 #mg/g the concentration of acidic phospholipids AP in Gut 
    
    Population['EW_ki'] <- (83.278 - 0.2162 * Population$age)*(28.3/78.3/100)
    Population['IW_ki'] <- (83.278 - 0.2162 * Population$age)*(50/78.3/100)
    Population['NL_ki'] <- (2.73 + 1.995 * Population$age /(2.59 + Population$age))*(2.07/3.69/100)
    Population['NP_ki'] <- (2.73 + 1.995 * Population$age /(2.59 + Population$age))*(1.62/3.69/100)
    AP_he <- 3.07 #mg/g the concentration of acidic phospholipids AP in Heart 
    
    Population['EW_li'] <- (75.69 - 0.573 * log10(Population$age))*(16.5/75.1/100)
    Population['IW_li'] <- (75.69 - 0.573 * log10(Population$age))*(58.6/75.1/100)
    Population['NL_li'] <- (3 + 3.089 * Population$age/(1.8 + Population$age))*(3.48/6/100)
    Population['NP_li'] <- (3 + 3.089 * Population$age/(1.8 + Population$age))*(2.52/6/100)
    AP_ki <- 2.48 #mg/g the concentration of acidic phospholipids AP in kidney 
    
    Population['EW_lu'] <- (80.973 - 0.4916 * log10(Population$age))*(34.8/81.1/100)
    Population['IW_lu'] <- (80.973 - 0.4916 * log10(Population$age))*(46.3/81.1/100)
    Population['NL_lu'] <- (1.857 - 0.211 * log10(Population$age))*(0.3/1.2/100)
    Population['NP_lu'] <- (1.857 - 0.211 * log10(Population$age))*(0.9/1.2/100)
    AP_li <- 5.09 #mg/g the concentration of acidic phospholipids AP in liver 
    
    Population['EW_mu'] <- (77.211 - 0.4321 * log10(Population$age))*(9.1/76/100)
    Population['IW_mu'] <- (77.211 - 0.4321 * log10(Population$age))*(66.9/76/100)
    Population['NL_mu'] <- (1.9852 + 0.0649 * Population$age)*(2.38/3.1/100)
    Population['NP_mu'] <- (1.9852 + 0.0649 * Population$age)*(0.72/3.1/100)
    AP_lu <- 0.5 #mg/g the concentration of acidic phospholipids AP in Lung 
    
    Population['EW_sk'] <- (72.395 - 1.1462 * log10(Population$age))*(62.3/71.77/100)
    Population['IW_sk'] <- (72.395 - 1.1462 * log10(Population$age))*(9.47/71.77/100)
    Population['NL_sk'] <-3.95*(2.84/3.95/100)
    Population['NP_sk'] <-3.95*(1.11/3.95/100)
    AP_mu <- 2.49 #mg/g the concentration of acidic phospholipids AP in muscle 
    
    Population['EW_sp'] <- (79.952 - 0.4178 * log10(Population$age))*(20.8/78.7/100)
    Population['IW_sp'] <- (79.952 - 0.4178 * log10(Population$age))*(57.9/78.7/100)
    Population['NL_sp'] <-(1.5 + 0.015 * Population$age)*(2.01/3.99/100)
    Population['NP_sp'] <-(1.5 + 0.015 * Population$age)*(1.98/3.99/100)
    
    Population['NL_pl'] <- (0.5578 + 0.036 * log10(Population$age))*(0.35/0.57/100)
    Population['NP_pl'] <- (0.5578 + 0.036 * log10(Population$age))*(0.22/0.57/100)
    AP_sk <- 1.32 #mg/g the concentration of acidic phospholipids AP in Skin 
    AP_sp <- 2.81 #mg/g the concentration of acidic phospholipids AP in Spleen 
    
    # Pancreas
    EW_pa <- 0.12
    IW_pa <- 0.664
    NL_pa <- 0.041
    NP_pa <- 0.0093
    AP_pa <- 1.67 #mg/g the concentration of acidic phospholipids AP in Pancreas 
    
    # Plasma
    EW_pl <- 0.945
    IW_pl <- 0
    NL_pl <- 0.0035
    NP_pl <- 0.0023
    AP_pl <- 0.04 #mg/g the concentration of acidic phospholipids AP in Plasma 
    
    # Brain
    EW_br <- 0.092
    IW_br <- 0.678
    NL_br <- 0.051
    NP_br <- 0.0565
    AP_br <- 0.4 #mg/g the concentration of acidic phospholipids AP in Brain
    
    IW_rbc <-66/100
    NL_rbc <-0.3*(0.17/0.46/100)
    NP_rbc <-0.3*(0.29/0.46/100)
    AP_rbc <- 0.44 #mg/g the concentration of acidic phospholipids AP in Plasma  
  }
  
  #Hematocrit
  #MALE
  Population['HCT_male'] <- (53 - ((43.0 * Population$age^1.12 / (0.05^1.12 + Population$age^1.12)) * (1 + (-0.93 * Population$age^0.25 / (0.10^0.25 + Population$age^0.25)))))/100
  Population['HCT_male_CV'] <- rnorm(N,0,0.065)
  Population['HCT_male'] <- Population$HCT_male*exp(Population$HCT_male_CV)
  
  #Blood-plasma ratio
  # BP=1-Ht + EP*Ht 
  Population['BP'] <- 1- Population$HCT_male + 1.34 * Population$HCT_male  #blood plasma ratio
  
  Population['Kpu_rbc'] <- (Population$BP + Population$HCT_male - 1) / (Population$HCT_male * Population$fup)
  Population['KaAP'] <- (Population$Kpu_rbc - (X_rbc / X_ew * IW_rbc) - ((logPow * NL_rbc + (0.3 * logPow + 0.7) * NP_rbc) /X_ew)) * (X_ew /  (AP_rbc * Y_rbc))#affinity constant for acidic phospholipids (AP) 
  
  Population['Kpu_ad'] <- Population$EW_ad + (X_iw / X_ew) * Population$IW_ad + ( Pad * Population$NL_ad + (0.3 * P + 0.7) * Population$NP_ad) / Population$EW_ad + Population$KaAP * AP_ad * Y_iw / X_ew
  Population['Kpu_bo'] <- EW_bo + (X_iw / X_ew) * IW_bo + ( P * NL_bo + (0.3 * P + 0.7) * NP_bo) / EW_bo + Population$KaAP * AP_bo * Y_iw / X_ew
  Population['Kpu_br'] <- EW_br + (X_iw / X_ew) * IW_br + ( P * NL_br + (0.3 * P + 0.7) * NP_br) / EW_br + Population$KaAP * AP_br * Y_iw / X_ew
  Population['Kpu_gu'] <- Population$EW_gu + (X_iw / X_ew) * Population$IW_gu + ( P * Population$NL_gu + (0.3 * P + 0.7) * Population$NP_gu) / Population$EW_gu + Population$KaAP * AP_gu * Y_iw / X_ew
  Population['Kpu_he'] <- Population$EW_he + (X_iw / X_ew) * Population$IW_he + ( P * Population$NL_he + (0.3 * P + 0.7) * Population$NP_he) / Population$EW_he + Population$KaAP * AP_he * Y_iw / X_ew
  Population['Kpu_ki'] <- Population$EW_ki + (X_iw / X_ew) * Population$IW_ki + ( P * Population$NL_ki + (0.3 * P + 0.7) * Population$NP_ki) / Population$EW_ki + Population$KaAP * AP_ki * Y_iw / X_ew
  Population['Kpu_li'] <- Population$EW_li + (X_iw / X_ew) * Population$IW_li + ( P * Population$NL_li + (0.3 * P + 0.7) * Population$NP_li) / Population$EW_li + Population$KaAP * AP_li * Y_iw / X_ew
  Population['Kpu_lu'] <- Population$EW_lu + (X_iw / X_ew) * Population$IW_lu + ( P * Population$NL_lu + (0.3 * P + 0.7) * Population$NP_lu) / Population$EW_lu + Population$KaAP * AP_lu * Y_iw / X_ew
  Population['Kpu_mu'] <- Population$EW_mu + (X_iw / X_ew) * Population$IW_mu + ( P * Population$NL_mu + (0.3 * P + 0.7) * Population$NP_mu) / Population$EW_mu + Population$KaAP * AP_mu * Y_iw / X_ew
  Population['Kpu_pa'] <- EW_pa + (X_iw / X_ew) * IW_pa + ( P * NL_pa + (0.3 * P + 0.7) * NP_pa) / EW_pa + Population$KaAP * AP_pa * Y_iw / X_ew
  Population['Kpu_sk'] <- Population$EW_sk + (X_iw / X_ew) * Population$IW_sk + ( P * Population$NL_sk + (0.3 * P + 0.7) * Population$NP_sk) / Population$EW_sk + Population$KaAP * AP_sk * Y_iw / X_ew
  Population['Kpu_sp'] <- Population$EW_sp + (X_iw / X_ew) * Population$IW_sp + ( P * Population$NL_sp + (0.3 * P + 0.7) * Population$NP_sp) / Population$EW_sp + Population$KaAP * AP_sp * Y_iw / X_ew
  Population['Kpu_pl'] <- EW_pl + (X_iw / X_ew) * IW_pl + ( P * NL_pl + (0.3 * P + 0.7) * NP_pl) / EW_pl + Population$KaAP * AP_pl * Y_iw / X_ew
  
  Population['Kpad'] <- Population$Kpu_ad * Population$fup
  Population['Kpbo'] <- Population$Kpu_bo * Population$fup
  Population['Kpbr'] <- Population$Kpu_br * Population$fup
  Population['Kpgu'] <- Population$Kpu_gu * Population$fup
  Population['Kphe'] <- Population$Kpu_he * Population$fup
  Population['Kpki'] <- Population$Kpu_ki * Population$fup
  Population['Kpli'] <- Population$Kpu_li * Population$fup
  Population['Kplu'] <- Population$Kpu_lu * Population$fup
  Population['Kpmu'] <- Population$Kpu_mu * Population$fup
  Population['Kpre'] <- Population$Kpu_pa * Population$fup
  Population['Kpsk'] <- Population$Kpu_sk * Population$fup
  Population['Kpsp'] <- Population$Kpu_sp * Population$fup
  Population['Kppl'] <- Population$Kpu_pl * Population$fup
  
  # tissue to plasma albumin ratio 
  ad_ratio <- 0.037
  bo_ratio <- 0.1
  br_ratio <- 0.048
  gu_ratio <- 0.158
  he_ratio <- 0.157
  ki_ratio <- 0.13  
  li_ratio <- 0.086
  lu_ratio <- 0.212
  mu_ratio <- 0.034
  pa_ratio <- 0.06
  sk_ratio <- 0.277
  sp_ratio <- 0.097
  
  # nonspecific protein binding (Kp) describe extracellular // intracellular
  Population['Kpec_he'] <-  (1- he_ratio) * Population$fup + he_ratio
  Population['Kpec_ad'] <-  (1- ad_ratio) * Population$fup + ad_ratio
  Population['Kpec_bo'] <-  (1- bo_ratio) * Population$fup + bo_ratio
  Population['Kpec_br'] <-  (1- br_ratio) * Population$fup + br_ratio
  Population['Kpec_gu'] <-  (1- gu_ratio) * Population$fup + gu_ratio
  Population['Kpec_ki'] <-  (1- ki_ratio) * Population$fup + ki_ratio
  Population['Kpec_li'] <-  (1- li_ratio) * Population$fup + li_ratio
  Population['Kpec_lu'] <-  (1- lu_ratio) * Population$fup + lu_ratio
  Population['Kpec_mu'] <-  (1- mu_ratio) * Population$fup + mu_ratio
  Population['Kpec_sk'] <-  (1- sk_ratio) * Population$fup + sk_ratio
  Population['Kpec_sp'] <-  (1- sp_ratio) * Population$fup + sp_ratio
  Population['Kpec_pa'] <-  (1- pa_ratio) * Population$fup + pa_ratio
}

Population['inf_dose_mg'] <- 33.6 * Population$BSA_male #[mg] infusion dose according to Pfizer guidance : https://www.pfizermedicalinformation.com/en-us/doxorubicin/dosage-admin
Population['inf_dose'] <- (Population$inf_dose_mg * 0.001 / MW ) * 1000000 # [umol]

pop2 <-
  Population[order(Population$Weight_male),] #order the population data according to BW
pop3 <-
  pop2[order(as.numeric(row.names(pop2))),] #rearranging the table with virtual population data according to individual numbers
setDT(pop3, keep.rownames = TRUE)[]
pop3$rn <- as.numeric(as.character(pop3$rn))

# MODEL -------------------------------------------------------------------
#Arguments of the function are the model parameters that vary
ModelVar <- function(CO,
                     Qad,
                     Qbo,
                     Qbr,
                     Qgu,
                     Qha,
                     Qhe,
                     Qki,
                     Qli,
                     Qlu,
                     Qmu,
                     Qre,
                     Qsk,
                     Qsp,
                     Vad,
                     Vad_ec,
                     Vad_ic,
                     Var,
                     Vbl,
                     Vbo,
                     Vbo_ec,
                     Vbo_ic,
                     Vbr,
                     Vbr_ec,
                     Vbr_ic,
                     Vgu,
                     Vgu_ec,
                     Vgu_ic,
                     Vhe,
                     Vhe_ec,
                     Vki,
                     Vki_ec,
                     Vki_ic,
                     Vli,
                     Vli_ec,
                     Vli_ic,
                     Vlu,
                     Vlu_ec,
                     Vlu_ic,
                     Vmu,
                     Vmu_ec,
                     Vmu_ic,
                     Vmyo,
                     Vmyo_ic,
                     Vother,
                     Vother_ic,
                     Vpl,
                     Vplas_art,
                     Vplas_ven,
                     Vrb,
                     Vre,
                     Vre_ec,
                     Vre_ic,
                     Vsk,
                     Vsk_ec,
                     Vsk_ic,
                     Vsp,
                     Vsp_ec,
                     Vsp_ic,
                     Vve,
                     SA_other,
                     SA_myo,
                     mtDNA_ad,
                     mtDNA_bo,
                     mtDNA_br,
                     mtDNA_gu,
                     mtDNA_he,
                     mtDNA_ki,
                     mtDNA_li,
                     mtDNA_lu,
                     mtDNA_mu,
                     mtDNA_myo,
                     mtDNA_other,
                     mtDNA_sk,
                     mtDNA_sp,
                     BP,
                     fup,
                     Kpgu,
                     Kphe,
                     Kpki,
                     Kpli,
                     Kplu,
                     Kpmu,
                     Kpp,
                     Kppl,
                     Kpre,
                     Kpsk,
                     Kpsp,
                     Kpad,
                     Kpbo,
                     Kpbr,
                     Kpec_he,
                     Kpec_ad,
                     Kpec_bo,
                     Kpec_br,
                     Kpec_gu,
                     Kpec_ki,
                     Kpec_li,
                     Kpec_lu,
                     Kpec_mu,
                     Kpec_sk,
                     Kpec_sp,
                     Kpec_pa,
                     t_end,
                     inf_dose)
{
  times <- seq(0, t_end, by = Sample_time)
  
  # PHYSIOLOGICAL PARAMETERS -------------------------------------------------------
  liver_density <- 1080 #[g/L]
  heart_density <- 1055 #[g/L] [Alexandra 2019]
  
  #the concentration of cardiolipin derive from [Daniel 2002]
  DNA_li <- 23.7 #umol/L 
  DNA_he <- 8.3 #umol/L 
  DNA_ki <- 16.2 #umol/L 
  DNA_bo <- 19.1 #umol/L 
  DNA_gu <- 25.2 #umol/L 
  
  DNA_mu <- 4.5 #umol/L Slowly perfused organs
  DNA_ad <- 4.5 #umol/L 
  DNA_sk <- 4.5 #umol/L 
  DNA_sp <- 4.5 #umol/L 
  
  DNA_br <- 15 #umol/L Rapidly perfused organ
  DNA_lu <- 15 #umol/L 
  
  DNA_bloodcell <- 0.01 #µmol/L assumption
  
  Cardiolipin_li <- 44.6 #umol/L 
  Cardiolipin_he <- 43.8 #umol/L 
  Cardiolipin_ki <- 52.3 #umol/L 
  Cardiolipin_bo <- 25 
  Cardiolipin_gu <- 25 
  
  Cardiolipin_ad <- 15 #umol/L  Slowly perfused
  Cardiolipin_mu <- 15 
  Cardiolipin_sk <- 15 
  Cardiolipin_sp <- 15 
  
  Cardiolipin_br <- 30 
  Cardiolipin_lu <- 30 
  
  DNA_other <- 0.05 * DNA_he
  DNA_myo <- 0.95 * DNA_he
  
  Cardiolipin_other <- 0.05 * Cardiolipin_he
  Cardiolipin_myo <- 0.95 * Cardiolipin_he
  
  Kd_DNA <- 3.23 # 3.23 umol/L = 0.00000323 mol/L = 0.00323 mM = 3.23 x 10^-6 M = 3230 nmol/L
  Koff_DNA <- 30564 #509.4 min-1 = 30564 h-1 
  Kon_DNA <- Koff_DNA / Kd_DNA 
  
  Kd_mtDNA <- 1 # 100nM to Molar (assume)
  Koff_mtDNA <- 30564 #509.4 min-1 = 30564 h-1
  Kon_mtDNA <- Koff_mtDNA / Kd_mtDNA
  
  Kd_cardiolipin <- 4 # 400nM to Molar (assume)
  Koff_cardiolipin <- 30564 #509.4 min-1 = 30564 h-1
  Kon_cardiolipin <- Koff_cardiolipin / Kd_cardiolipin
  
  kon1 <- Kon_DNA
  koff1 <- Koff_DNA
  
  kon2 <- Kon_cardiolipin
  koff2 <- Koff_cardiolipin
  
  kon3 <- Kon_mtDNA
  koff3 <- Koff_mtDNA
  
  #PER：cytoplasmic membrane permeability coefficient
  PER <- 0.0756  # cm/h （huahe Unpublished）
  
  SA_bloodcell <- 2580000 #[cm2] [Huahe paper]
  
  SA_bo = 381752.22
  SA_br = 55916.8
  SA_ki = 95801.19
  SA_li = 383745.62
  SA_lu = 668945.11
  SA_mu = 775236.33
  SA_pa = 36192.33
  SA_sk = 164447.1
  SA_sp = 64874.62
  SA_gu = 7495.21 + 5122.72
  SA_ad = 250634.81
  
  # PARAMETERS FOR ICF and ECF in heart tissue -------------------------------------------------------
  pH_ic <- 7.2 #[Vaugha-Jones 2009, Zheng 2005]
  pH_ec <- 7.4 #[Vaugha-Jones 2009, Zheng 2005]
  
  #Henderson_Hasselbalch equation for base compound -> fraction of un-ionized base in heart compartments: -------------------------------------------------------
  funionized_ic <- 1 / (1 + 10 ^ (pKa - pH_ic))
  funionized_ec <- 1 / (1 + 10 ^ (pKa - pH_ec))
  
  Kpp <- (1 + 10 ^ (pKa - pH_ic)) / (1 + 10 ^ (pKa - pH_ec)) 
  
  #IV INFUSION RATE
  r = inf_dose #[umol]
  t = inf_time #time of infusion [h]
  inf = r / t #infusion rate [umol/h]
  
  #DISTRIBUTION  -------------------------------------------------------
  #Drug binding
  fup <- 0.26 #fraction unbound in plasma (Huahe)
  fu_ec <- 1 #fraction unbound in extracellular fluid is assumed
  fu_heart <- 1 #fraction unbound in heart is assumed
  
  fuec_he <- 1 #fraction unbound in heart extracellular fluid is assumed
  fuec_bo <- 1 
  fuec_br <- 1 
  fuec_ki <- 1 
  fuec_li <- 1 
  fuec_lu <- 1 
  fuec_mu <- 1 
  fuec_sk <- 1 
  fuec_sp <- 1 
  fuec_gu <- 1 
  fuec_ad <- 1 
  
  # CL clearance [Leandro 2019] -------------------------------------------------------
  CL_renal <- 0.66 #[L/ h] renal clearance
  CL_hepatic <- 29.97 #[L/ h] hepatic clearance
  CLint_heart <- 0 #[L/ h] heart clearance CYP3A4 was not detected in heart tissue
  CL_total <- 30.6 #[L/ h] total clearance
  
  # MODEL -------------------------------------------------------------------
  parameters <- c(
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
  
  # State variables -------------------------------------------------------
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
  ###Differential equations - umol/h/L -------------------------------------------------------
  PBPKModel = function(times, state, parameters) {
    with(as.list(c(state, parameters)), {
      inf <- ifelse(times <= t, inf, 0)
      
      #DOX concentrations:
      Cliverfree <-  Cli_ec * (fup / BP)  #liver free concentration
      Ckidneyfree <- Cki_ec * (fup / BP) #kidney free concentration
      Cplasmavenous <- Cve / BP	#venous plasma concentration
      
      ## rates of changes
      dINFUSION <- -inf
      dCre <- (Qre * (Car - Cre / Kpre * BP))/ Vre  		#rest of body
      dCve <- (inf + Qad * (Cad_ec / Kpad * BP) + Qbo * (Cbo_ec / Kpbo * BP) + Qbr * (Cbr_ec / Kpbr * BP) + Qki* (Cki_ec / Kpki * BP) + Qli * (Cli_ec / Kpli * BP) + Qmu* (Cmu_ec / Kpmu * BP) + Qsk* (Csk_ec / Kpsk * BP)+ Qhe * (Che_ec / Kphe * BP) + Qre*(Cre / Kpre * BP) - Qlu * Cve - PER * SA_bloodcell * 0.001 * (Cve* funionized_ec - (Cbloodcell/Kpp)* funionized_ic))/ Vve#venous blood
      dCar <- (Qlu * (Clu_ec / Kplu * BP) - Qlu * Car - PER * SA_bloodcell * 0.001 * (Car* funionized_ec - (Cbloodcell/Kpp)* funionized_ic))/ Var 	#arterial blood
      
      # Extracellular sub-compartment
      dChe_ec <- (Qhe * (Car - (Che_ec/Kphe) * BP) -(0.001 * PER * SA_myo * (Che_ec * fuec_he * funionized_ec - Cmyo_ict/(Kpec_he * Kpp) * funionized_ic)) - (0.001 * 0.0000756 *  SA_other * (Che_ec * fu_ec * funionized_ec - Cother_ict /(Kpec_he * Kpp) * funionized_ic)) ) / Vhe_ec
      dCad_ec <- (Qad * (Car - (Cad_ec/Kpad) * BP) - (0.001 * PER * SA_ad * (Cad_ec * fuec_ad * funionized_ec - Cad_ict/(Kpec_ad * Kpp) * funionized_ic)))/ Vad_ec 
      dCbo_ec <- (Qbo * (Car - (Cbo_ec/Kpbo) * BP) - (0.001 * PER * SA_bo * (Cbo_ec * fuec_bo * funionized_ec - Cbo_ict/(Kpec_bo * Kpp) * funionized_ic)))/ Vbo_ec
      dCbr_ec <- (Qbr * (Car - (Cbr_ec/Kpbr) * BP) - (0.001 * PER * SA_br * (Cbr_ec * fuec_br * funionized_ec - Cbr_ict/(Kpec_br * Kpp) * funionized_ic)))/ Vbr_ec
      dCgu_ec <- (Qgu * (Car - (Cgu_ec/Kpgu) * BP) - (0.001 * PER * SA_gu * (Cgu_ec * fuec_gu * funionized_ec - Cgu_ict/(Kpec_gu * Kpp) * funionized_ic)))/ Vgu_ec
      dCki_ec <- ((Qki * (Car - (Cki_ec/Kpki) * BP) - CL_renal * Ckidneyfree) - (0.001 * PER * SA_ki * (Cki_ec * fuec_ki * funionized_ec - Cki_ict/(Kpec_ki * Kpp) * funionized_ic)))/ Vki_ec
      dCli_ec <- (((Qha * Car + Qgu * (Cgu_ec / Kpgu * BP) + Qsp * (Csp_ec / Kpsp * BP) - Qli * (Cli_ec / Kpli * BP) - CL_hepatic * Cliverfree)) - (0.001 * PER * SA_li * (Cli_ec * fuec_li * funionized_ec - Cli_ict/(Kpec_li * Kpp) * funionized_ic)))/ Vli_ec
      dClu_ec <- ((Qlu * Cve - Qlu * (Clu_ec / Kplu * BP)) - (0.001 * PER * SA_lu * (Clu_ec * fuec_lu * funionized_ec - Clu_ict/(Kpec_lu * Kpp) * funionized_ic)))/ Vlu_ec
      dCmu_ec <- (Qmu * (Car - (Cmu_ec/Kpmu) * BP) - (0.001 * PER * SA_mu * (Cmu_ec * fuec_mu * funionized_ec - Cmu_ict/(Kpec_mu * Kpp) * funionized_ic)))/ Vmu_ec
      dCsk_ec <- (Qsk * (Car - (Csk_ec/Kpsk) * BP) - (0.001 * PER * SA_sk * (Csk_ec * fuec_sk * funionized_ec - Csk_ict/(Kpec_sk * Kpp) * funionized_ic)))/ Vsk_ec
      dCsp_ec <- (Qsp * (Car - (Csp_ec/Kpsp) * BP) - (0.001 * PER * SA_sp * (Csp_ec * fuec_sp * funionized_ec - Csp_ict/(Kpec_sp * Kpp) * funionized_ic)))/ Vsp_ec
      
      # Subcompartments intracellular total concentration
      dCmyo_ict <- (0.001 * PER * SA_myo * ((Che_ec * fu_ec * funionized_ec - Cmyo_ict/(Kpec_he * Kpp) * funionized_ic ) - (Cmyo_ict * fu_heart * CLint_heart * (Vmyo_ic/Vhe))) / Vmyo_ic) - (kon1 * Cmyo_ict * Cmyo_E1 - koff1 * Cmyo_E1R) - (kon2 * Cmyo_ict * Cmyo_E2 - koff2 * Cmyo_E2R ) - (kon3 * Cmyo_ict * Cmyo_E3 - koff3 * Cmyo_E3R )
      dCother_ict <- (0.001 * 0.0000756 * SA_other * ((Che_ec * fu_ec * funionized_ec - Cother_ict/(Kpec_he * Kpp) * funionized_ic) - (Cother_ict * fu_heart * CLint_heart * (Vother_ic/Vhe)))/Vother_ic) - (kon1 * Cother_ict * Cother_E1 - koff1 * Cother_E1R) - (kon2 * Cother_ict * Cother_E2 - koff2 * Cother_E2R ) - (kon3 * Cother_ict * Cother_E3 - koff3 * Cother_E3R )
      dCad_ict <- (0.001 * PER * SA_ad * ((Cad_ec * fuec_ad * funionized_ec - Cad_ict/(Kpec_ad * Kpp) * funionized_ic)) / Vad_ic) - (kon1 * Cad_ict * Cad_E1 - koff1 * Cad_E1R) - (kon2 * Cad_ict * Cad_E2 - koff2 * Cad_E2R) - (kon3 * Cad_ict * Cad_E3 - koff3 * Cad_E3R) 
      dCbo_ict <- (0.001 * PER * SA_bo * ((Cbo_ec * fuec_bo * funionized_ec - Cbo_ict/(Kpec_bo * Kpp) * funionized_ic)) / Vbo_ic) - (kon1 * Cbo_ict * Cbo_E1 - koff1 * Cbo_E1R) - (kon2 * Cbo_ict * Cbo_E2 - koff2 * Cbo_E2R) - (kon3 * Cbo_ict * Cbo_E3 - koff3 * Cbo_E3R) 
      dCbr_ict <- (0.001 * PER * SA_br * ((Cbr_ec * fuec_br * funionized_ec - Cbr_ict/(Kpec_br * Kpp) * funionized_ic)) / Vbr_ic) - (kon1 * Cbr_ict * Cbr_E1 - koff1 * Cbr_E1R) - (kon2 * Cbr_ict * Cbr_E2 - koff2 * Cbr_E2R) - (kon3 * Cbr_ict * Cbr_E3 - koff3 * Cbr_E3R) 
      dCgu_ict <- (0.001 * PER * SA_gu * ((Cgu_ec * fuec_gu * funionized_ec - Cgu_ict/(Kpec_gu * Kpp) * funionized_ic)) / Vgu_ic) - (kon1 * Cgu_ict * Cgu_E1 - koff1 * Cgu_E1R) - (kon2 * Cgu_ict * Cgu_E2 - koff2 * Cgu_E2R) - (kon3 * Cgu_ict * Cgu_E3 - koff3 * Cgu_E3R) 
      dCki_ict <- (0.001 * PER * SA_ki * ((Cki_ec * fuec_ki * funionized_ec - Cki_ict/(Kpec_ki * Kpp) * funionized_ic)) / Vki_ic) - (kon1 * Cki_ict * Cki_E1 - koff1 * Cki_E1R) - (kon2 * Cki_ict * Cki_E2 - koff2 * Cki_E2R) - (kon3 * Cki_ict * Cki_E3 - koff3 * Cki_E3R) 
      dCli_ict <- (0.001 * PER * SA_li * ((Cli_ec * fuec_li * funionized_ec - Cli_ict/(Kpec_li * Kpp) * funionized_ic)) / Vli_ic) - (kon1 * Cli_ict * Cli_E1 - koff1 * Cli_E1R) - (kon2 * Cli_ict * Cli_E2 - koff2 * Cli_E2R) - (kon3 * Cli_ict * Cli_E3 - koff3 * Cli_E3R) 
      dClu_ict <- (0.001 * PER * SA_lu * ((Clu_ec * fuec_lu * funionized_ec - Clu_ict/(Kpec_lu * Kpp) * funionized_ic)) / Vlu_ic) - (kon1 * Clu_ict * Clu_E1 - koff1 * Clu_E1R) - (kon2 * Clu_ict * Clu_E2 - koff2 * Clu_E2R) - (kon3 * Clu_ict * Clu_E3 - koff3 * Clu_E3R) 
      dCmu_ict <- (0.001 * PER * SA_mu * ((Cmu_ec * fuec_mu * funionized_ec - Cmu_ict/(Kpec_mu * Kpp) * funionized_ic)) / Vmu_ic) - (kon1 * Cmu_ict * Cmu_E1 - koff1 * Cmu_E1R) - (kon2 * Cmu_ict * Cmu_E2 - koff2 * Cmu_E2R) - (kon3 * Cmu_ict * Cmu_E3 - koff3 * Cmu_E3R) 
      dCsk_ict <- (0.001 * PER * SA_sk * ((Csk_ec * fuec_sk * funionized_ec - Csk_ict/(Kpec_sk * Kpp) * funionized_ic)) / Vsk_ic) - (kon1 * Csk_ict * Csk_E1 - koff1 * Csk_E1R) - (kon2 * Csk_ict * Csk_E2 - koff2 * Csk_E2R) - (kon3 * Csk_ict * Csk_E3 - koff3 * Csk_E3R) 
      dCsp_ict <- (0.001 * PER * SA_sp * ((Csp_ec * fuec_sp * funionized_ec - Csp_ict/(Kpec_sp * Kpp) * funionized_ic)) / Vsp_ic) - (kon1 * Csp_ict * Csp_E1 - koff1 * Csp_E1R) - (kon2 * Csp_ict * Csp_E2 - koff2 * Csp_E2R) - (kon3 * Csp_ict * Csp_E3 - koff3 * Csp_E3R) 
      
      dCbloodcell <- (0.001 * PER * SA_bloodcell * (Cve * funionized_ec - (Cbloodcell/Kpp)* funionized_ic) + 0.001 * PER * SA_bloodcell * (Car* funionized_ec - (Cbloodcell/Kpp)* funionized_ic))/ Vrb - (kon1 * Cbloodcell * Cbloodcell_E1 - koff1 * Cbloodcell_E1R)
      dCbloodcell_E1R <- kon1 * Cbloodcell * Cbloodcell_E1 - koff1 * Cbloodcell_E1R
      dCbloodcell_E1 <- -(kon1 * Cbloodcell * Cbloodcell_E1 - koff1 * Cbloodcell_E1R)      
      
      # Intracellular binding-------------------------------------------------------
      #heart
      #Myocardial
      dCmyo_E1R = kon1 * Cmyo_ict * Cmyo_E1 - koff1 * Cmyo_E1R
      dCmyo_E1 = - (kon1 * Cmyo_ict * Cmyo_E1 - koff1 * Cmyo_E1R) 
      
      dCmyo_E2R = kon2 * Cmyo_ict * Cmyo_E2 - koff2 * Cmyo_E2R 
      dCmyo_E2 = - (kon2 * Cmyo_ict * Cmyo_E2 - koff2 * Cmyo_E2R) 
      
      dCmyo_E3R = kon3 * Cmyo_ict * Cmyo_E3 - koff3 * Cmyo_E3R 
      dCmyo_E3 = - (kon3 * Cmyo_ict * Cmyo_E3 - koff3 * Cmyo_E3R) 
      
      dCother_E1R = kon1 * Cother_ict * Cother_E1 - koff1 * Cother_E1R
      dCother_E1 = - (kon1 * Cother_ict * Cother_E1 - koff1 * Cother_E1R) 
      
      dCother_E2R = kon2 * Cother_ict * Cother_E2 - koff2 * Cother_E2R 
      dCother_E2 = - (kon2 * Cother_ict * Cother_E2 - koff2 * Cother_E2R) 
      
      dCother_E3R = kon3 * Cother_ict * Cother_E3 - koff3 * Cother_E3R 
      dCother_E3 = - (kon3 * Cother_ict * Cother_E3 - koff3 * Cother_E3R) 
      
      # Adipose
      dCad_E1R = kon1 * Cad_ict * Cad_E1 - koff1 * Cad_E1R
      dCad_E1 = - (kon1 * Cad_ict * Cad_E1 - koff1 * Cad_E1R) 
      
      dCad_E2R = kon2 * Cad_ict * Cad_E2 - koff2 * Cad_E2R 
      dCad_E2 = - (kon2 * Cad_ict * Cad_E2 - koff2 * Cad_E2R) 
      
      dCad_E3R = kon3 * Cad_ict * Cad_E3 - koff3 * Cad_E3R 
      dCad_E3 = - (kon3 * Cad_ict * Cad_E3 - koff3 * Cad_E3R) 
      
      # Bone
      dCbo_E1R = kon1 * Cbo_ict * Cbo_E1 - koff1 * Cbo_E1R
      dCbo_E1 = - (kon1 * Cbo_ict * Cbo_E1 - koff1 * Cbo_E1R) 
      
      dCbo_E2R = kon2 * Cbo_ict * Cbo_E2 - koff2 * Cbo_E2R 
      dCbo_E2 = - (kon2 * Cbo_ict * Cbo_E2 - koff2 * Cbo_E2R) 
      
      dCbo_E3R = kon3 * Cbo_ict * Cbo_E3 - koff3 * Cbo_E3R 
      dCbo_E3 = - (kon3 * Cbo_ict * Cbo_E3 - koff3 * Cbo_E3R) 
      
      # Brain
      dCbr_E1R = kon1 * Cbr_ict * Cbr_E1 - koff1 * Cbr_E1R
      dCbr_E1 = - (kon1 * Cbr_ict * Cbr_E1 - koff1 * Cbr_E1R) 
      
      dCbr_E2R = kon2 * Cbr_ict * Cbr_E2 - koff2 * Cbr_E2R 
      dCbr_E2 = - (kon2 * Cbr_ict * Cbr_E2 - koff2 * Cbr_E2R) 
      
      dCbr_E3R = kon3 * Cbr_ict * Cbr_E3 - koff3 * Cbr_E3R 
      dCbr_E3 = - (kon3 * Cbr_ict * Cbr_E3 - koff3 * Cbr_E3R) 
      
      # Gut
      dCgu_E1R = kon1 * Cgu_ict * Cgu_E1 - koff1 * Cgu_E1R
      dCgu_E1 = - (kon1 * Cgu_ict * Cgu_E1 - koff1 * Cgu_E1R) 
      
      dCgu_E2R = kon2 * Cgu_ict * Cgu_E2 - koff2 * Cgu_E2R 
      dCgu_E2 = - (kon2 * Cgu_ict * Cgu_E2 - koff2 * Cgu_E2R) 
      
      dCgu_E3R = kon3 * Cgu_ict * Cgu_E3 - koff3 * Cgu_E3R 
      dCgu_E3 = - (kon3 * Cgu_ict * Cgu_E3 - koff3 * Cgu_E3R) 
      
      # Kidney
      dCki_E1R = kon1 * Cki_ict * Cki_E1 - koff1 * Cki_E1R
      dCki_E1 = - (kon1 * Cki_ict * Cki_E1 - koff1 * Cki_E1R) 
      
      dCki_E2R = kon2 * Cki_ict * Cki_E2 - koff2 * Cki_E2R 
      dCki_E2 = - (kon2 * Cki_ict * Cki_E2 - koff2 * Cki_E2R) 
      
      dCki_E3R = kon3 * Cki_ict * Cki_E3 - koff3 * Cki_E3R 
      dCki_E3 = - (kon3 * Cki_ict * Cki_E3 - koff3 * Cki_E3R) 
      
      # Liver
      dCli_E1R = kon1 * Cli_ict * Cli_E1 - koff1 * Cli_E1R
      dCli_E1 = - (kon1 * Cli_ict * Cli_E1 - koff1 * Cli_E1R) 
      
      dCli_E2R = kon2 * Cli_ict * Cli_E2 - koff2 * Cli_E2R 
      dCli_E2 = - (kon2 * Cli_ict * Cli_E2 - koff2 * Cli_E2R) 
      
      dCli_E3R = kon3 * Cli_ict * Cli_E3 - koff3 * Cli_E3R 
      dCli_E3 = - (kon3 * Cli_ict * Cli_E3 - koff3 * Cli_E3R) 
      
      #lung
      dClu_E1R = kon1 * Clu_ict * Clu_E1 - koff1 * Clu_E1R
      dClu_E1 = - (kon1 * Clu_ict * Clu_E1 - koff1 * Clu_E1R) 
      
      dClu_E2R = kon2 * Clu_ict * Clu_E2 - koff2 * Clu_E2R 
      dClu_E2 = - (kon2 * Clu_ict * Clu_E2 - koff2 * Clu_E2R) 
      
      dClu_E3R = kon3 * Clu_ict * Clu_E3 - koff3 * Clu_E3R 
      dClu_E3 = - (kon3 * Clu_ict * Clu_E3 - koff3 * Clu_E3R) 
      
      # Muscle
      dCmu_E1R = kon1 * Cmu_ict * Cmu_E1 - koff1 * Cmu_E1R
      dCmu_E1 = - (kon1 * Cmu_ict * Cmu_E1 - koff1 * Cmu_E1R) 
      
      dCmu_E2R = kon2 * Cmu_ict * Cmu_E2 - koff2 * Cmu_E2R 
      dCmu_E2 = - (kon2 * Cmu_ict * Cmu_E2 - koff2 * Cmu_E2R) 
      
      dCmu_E3R = kon3 * Cmu_ict * Cmu_E3 - koff3 * Cmu_E3R 
      dCmu_E3 = - (kon3 * Cmu_ict * Cmu_E3 - koff3 * Cmu_E3R) 
      
      # Skin
      dCsk_E1R = kon1 * Csk_ict * Csk_E1 - koff1 * Csk_E1R
      dCsk_E1 = - (kon1 * Csk_ict * Csk_E1 - koff1 * Csk_E1R) 
      
      dCsk_E2R = kon2 * Csk_ict * Csk_E2 - koff2 * Csk_E2R 
      dCsk_E2 = - (kon2 * Csk_ict * Csk_E2 - koff2 * Csk_E2R) 
      
      dCsk_E3R = kon3 * Csk_ict * Csk_E3 - koff3 * Csk_E3R 
      dCsk_E3 = - (kon3 * Csk_ict * Csk_E3 - koff3 * Csk_E3R) 
      
      # Spleen
      dCsp_E1R = kon1 * Csp_ict * Csp_E1 - koff1 * Csp_E1R
      dCsp_E1 = - (kon1 * Csp_ict * Csp_E1 - koff1 * Csp_E1R) 
      
      dCsp_E2R = kon2 * Csp_ict * Csp_E2 - koff2 * Csp_E2R 
      dCsp_E2 = - (kon2 * Csp_ict * Csp_E2 - koff2 * Csp_E2R) 
      
      dCsp_E3R = kon3 * Csp_ict * Csp_E3 - koff3 * Csp_E3R 
      dCsp_E3 = - (kon3 * Csp_ict * Csp_E3 - koff3 * Csp_E3R) 
      
      list(
        c(dINFUSION,
          dCre,
          dCve,
          dCar,
          
          dChe_ec,
          dCad_ec,
          dCbo_ec,
          dCbr_ec,
          dCgu_ec,
          dCki_ec,
          dCli_ec,
          dClu_ec,
          dCmu_ec,
          dCsk_ec,
          dCsp_ec,
          
          dCmyo_ict,
          dCother_ict,
          dCad_ict,
          dCbo_ict,
          dCbr_ict,
          dCgu_ict,
          dCki_ict,
          dCli_ict,
          dClu_ict,
          dCmu_ict,
          dCsk_ict,
          dCsp_ict,
          
          dCbloodcell,
          dCbloodcell_E1R,
          dCbloodcell_E1,
          
          dCmyo_E1R,
          dCmyo_E1,
          dCmyo_E2R,
          dCmyo_E2,
          dCmyo_E3R,
          dCmyo_E3,
          
          dCother_E1R,
          dCother_E1,
          dCother_E2R,
          dCother_E2,
          dCother_E3R,
          dCother_E3,
          
          dCad_E1R,
          dCad_E1,
          dCad_E2R,
          dCad_E2,
          dCad_E3R,
          dCad_E3,
          
          dCbo_E1R,
          dCbo_E1,
          dCbo_E2R,
          dCbo_E2,
          dCbo_E3R,
          dCbo_E3,
          
          dCbr_E1R,
          dCbr_E1,
          dCbr_E2R,
          dCbr_E2,
          dCbr_E3R,
          dCbr_E3,
          
          dCgu_E1R,
          dCgu_E1,
          dCgu_E2R,
          dCgu_E2,
          dCgu_E3R,
          dCgu_E3,
          
          dCki_E1R,
          dCki_E1,
          dCki_E2R,
          dCki_E2,
          dCki_E3R,
          dCki_E3,
          
          dCli_E1R,
          dCli_E1,
          dCli_E2R,
          dCli_E2,
          dCli_E3R,
          dCli_E3,
          
          dClu_E1R,
          dClu_E1,
          dClu_E2R,
          dClu_E2,
          dClu_E3R,
          dClu_E3,
          
          dCmu_E1R,
          dCmu_E1,
          dCmu_E2R,
          dCmu_E2,
          dCmu_E3R,
          dCmu_E3,
          
          dCsk_E1R,
          dCsk_E1,
          dCsk_E2R,
          dCsk_E2,
          dCsk_E3R,
          dCsk_E3,
          
          dCsp_E1R,
          dCsp_E1,
          dCsp_E2R,
          dCsp_E2,
          dCsp_E3R,
          dCsp_E3
        ),
        logPL = log10(Cplasmavenous),
        PL = Cplasmavenous,
        BLCELL = Cbloodcell
      )
    })
  }
  out <-
    ode(
      y = state,
      times = times,
      func = PBPKModel,
      parm = parameters
    )
  results <- data.frame(out)
  return(results)
}

# APPLYING THE MODEL ------------------------------------------------------
for (i in pop3$rn) {
  nam <- paste("output", i, sep = "")
  assign (
    nam,
    ModelVar(
      pop3$CO[i],
      pop3$Qad[i],
      pop3$Qbo[i],
      pop3$Qbr[i],
      pop3$Qgu[i],
      pop3$Qha[i],
      pop3$Qhe[i],
      pop3$Qki[i],
      pop3$Qli[i],
      pop3$Qlu[i],
      pop3$Qmu[i],
      pop3$Qre[i],
      pop3$Qsk[i],
      pop3$Qsp[i],
      pop3$Vad[i],
      pop3$Vad_ec[i],
      pop3$Vad_ic[i],
      pop3$Var[i],
      pop3$Vbl[i],
      pop3$Vbo[i],
      pop3$Vbo_ec[i],
      pop3$Vbo_ic[i],
      pop3$Vbr[i],
      pop3$Vbr_ec[i],
      pop3$Vbr_ic[i],
      pop3$Vgu[i],
      pop3$Vgu_ec[i],
      pop3$Vgu_ic[i],
      pop3$Vhe[i],
      pop3$Vhe_ec[i],
      pop3$Vki[i],
      pop3$Vki_ec[i],
      pop3$Vki_ic[i],
      pop3$Vli[i],
      pop3$Vli_ec[i],
      pop3$Vli_ic[i],
      pop3$Vlu[i],
      pop3$Vlu_ec[i],
      pop3$Vlu_ic[i],
      pop3$Vmu[i],
      pop3$Vmu_ec[i],
      pop3$Vmu_ic[i],
      pop3$Vmyo[i],
      pop3$Vmyo_ic[i],
      pop3$Vother[i],
      pop3$Vother_ic[i],
      pop3$Vpl[i],
      pop3$Vplas_art[i],
      pop3$Vplas_ven[i],
      pop3$Vrb[i],
      pop3$Vre[i],
      pop3$Vre_ec[i],
      pop3$Vre_ic[i],
      pop3$Vsk[i],
      pop3$Vsk_ec[i],
      pop3$Vsk_ic[i],
      pop3$Vsp[i],
      pop3$Vsp_ec[i],
      pop3$Vsp_ic[i],
      pop3$Vve[i],
      pop3$SA_other[i],
      pop3$SA_myo[i],
      pop3$mtDNA_ad[i],
      pop3$mtDNA_bo[i],
      pop3$mtDNA_br[i],
      pop3$mtDNA_gu[i],
      pop3$mtDNA_he[i],
      pop3$mtDNA_ki[i],
      pop3$mtDNA_li[i],
      pop3$mtDNA_lu[i],
      pop3$mtDNA_mu[i],
      pop3$mtDNA_myo[i],
      pop3$mtDNA_other[i],
      pop3$mtDNA_sk[i],
      pop3$mtDNA_sp[i],
      pop3$BP[i],
      pop3$fup[i],
      pop3$Kpgu[i],
      pop3$Kphe[i],
      pop3$Kpki[i],
      pop3$Kpli[i],
      pop3$Kplu[i],
      pop3$Kpmu[i],
      pop3$Kpp[i],
      pop3$Kppl[i],
      pop3$Kpre[i],
      pop3$Kpsk[i],
      pop3$Kpsp[i],
      pop3$Kpad[i],
      pop3$Kpbo[i],
      pop3$Kpbr[i],
      pop3$Kpec_he[i],
      pop3$Kpec_ad[i],
      pop3$Kpec_bo[i],
      pop3$Kpec_br[i],
      pop3$Kpec_gu[i],
      pop3$Kpec_ki[i],
      pop3$Kpec_li[i],
      pop3$Kpec_lu[i],
      pop3$Kpec_mu[i],
      pop3$Kpec_sk[i],
      pop3$Kpec_sp[i],
      pop3$Kpec_pa[i],
      t_end,
      pop3$inf_dose[i]
    )
  )
  print(paste("finished subject male",i))
}

results_list = lapply(ls(pattern = "output[0-9]"), get)

newDF <- bind_rows(results_list, .id = "id")
#####PLOT DOXORUBICIN CONCENTRATION IN PLASMA
ggplot(newDF, aes(time, PL, colour = id)) +
  geom_line() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Simulation results - DOXORUBICIN in plasma") +
  labs(list(x = "Time [h]", y = "Concentration in plasma [umol/L]", colour =
              "Individuals"))

#####PLOT DOXORUBICIN CONCENTRATION IN CARDIAC TISSUE
ggplot(newDF, aes(time, Che_ec, colour = id)) +
  geom_line() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Simulation results - Doxorubicin in heart") +
  labs(list(x = "Time [h]", y = "Concentration in heart tissue [umol/L]", colour =
              "Individuals"))

PL <- newDF$PL

{Summaryplasma<-data.frame( 
  Timerun=as.double(tapply(newDF$time,paste(newDF$time),median)),
  
  Median=as.double(tapply(PL,paste(newDF$time),median)),
  
  Lower=as.double(tapply(PL,paste(newDF$time),quantile,0.05)),
  
  Upper=as.double(tapply(PL,paste(newDF$time),quantile,0.95)),
  
  Lowermin=as.double(tapply(PL,paste(newDF$time),quantile,0.00)),
  
  Uppermax=as.double(tapply(PL,paste(newDF$time),quantile,1.00))
)
  
  #Order data
  Summaryplasma <- Summaryplasma[order(Summaryplasma$Timerun,Summaryplasma$Median),]
  
  #plot PLASMA
  plot(times,Summaryplasma$Median,ylim=c(min(0.01),max(100)),log='y',xlab="Time (h)",ylab="umol/L", type="n", main="Plasma concentration") # set y-axis tick marks
  lines(times,Summaryplasma$Upper,col="darkgrey", type="l",lwd=2)
  lines(times,Summaryplasma$Lower,col="darkgrey", type="l",lwd=2)
  polygon(c(times, rev(times)), c(Summaryplasma$Upper, rev(Summaryplasma$Lower)),
          col = "grey90", lwd=2, border = NA)
  lines(times,Summaryplasma$Uppermax,lty=3,col="black",lwd=1)
  lines(times,Summaryplasma$Lowermin,lty=3,col="black",lwd=1)
  lines(times,Summaryplasma$Median,col="darkred",type="l",lwd=2)
}

# Add data points to the plot
#Greene, 1983 Dose:75 ADT:15min Cancer
points(c(0,0.05,0.167,0.5,1,3,6,24,48,72,98), c(6.25,2.778,1.3,0.355,0.169,0.094,0.078,0.035,0.021,0.0156,0.0101), pch=16, col= "#0072B2")

#Camaggi 1988 Dose:60 bolus Age:42-72 Cancer
points(c(0.25,0.5,1,2,4,8,12,24,36,48,72,96,120,144,168), c(1.1339,0.1991,0.0983,0.0647,0.0473,0.04,0.0291,0.0246,0.0178,0.015,0.0113,0.0079,0.0049,0.0039,0.0031), pch=16, col="#D55E00")
#Benjamin, 1973 Dose:60 ADT: 1-5min Cancer
points(c(0.083,0.167,0.25,0.33,0.5,0.75,1), c(5.66,2.6,1.31,0.753,0.353,0.236,0.197), pch=16, col="#CC79A7")
#Johnson, 1992 Dose:60 bolus Age:20-76 Hepatocellular carcinoma
points(c(0.176,0.204,0.377,0.559,1.058,2.064,3.06,4.297,12.574,20.444,24.264,30.606,48.564,72.481),c(6.757,1.946,0.574,0.366,0.166,0.142,0.115,0.0897,0.0622,0.0476,0.0413,0.0348,0.0242,0.0139), pch=16, col= "black")

#Piscitelli, 1993 Dose:45 - 72 mg/m2 Age:55 ADT:1hour Cancer
points(c(1.270,1.581,2.217,3.164,4.159,6.152,8.101,12.095,24.076,36.058,47.995),c(1.726,0.2,0.125,0.092,0.074,0.056,0.048,0.041,0.029,0.020,0.014), pch=16, col= "#0072B2")


#Muller, 1993 Dose:36 Bolus Age:70 Cancer
points(c(0.083,0.167,0.25,05,1,0.95,1.166,2.149,8.054,24.292,48.563,71.766,96.037,119.79,144.299,168.528,216.022),c(2.728,2.00,1.066,0.552,0.149,0.09,0.061,0.0389,0.0279,0.02,0.018,0.012,0.0104,0.009,0.007,0.004,0.003), pch=16, col= "black")

#Robert, 1983 Dose:35 Age:16-67 ADT:3min Cancer
points(c(0.05,0.1,0.167,0.25,0.33), c(5.207,2.73,1.48,0.54,0.332), pch=16, col= "#009E73")
points(c(0.05,0.1,0.167,0.25,0.33), c(3.72,1.862,0.901,0.387,0.215), pch=16, col= "#F0E442")

#Yoshida, 1994 Dose:0.9 mg/kg Age:47-72 ADT: 30min Cancer
points(c(0.5742,0.8134,1,1.292,1.531,2.536,4.498,8.518,24.45,43.5),c(2.4638,0.2566,0.0896,0.0645,0.0568,0.0435,0.0323,0.0248,0.0152,0.0078), pch=16, col= "#009E73")

#Speth, 1987 Dose:30 Q24x3 bolus age: 17-67 Cancer (+24,+48)
points(c(0.04166667,0.25,0.5,1,2,7,24), c(4.388,0.977,0.289,0.259,0.187,0.139,0.086), pch=16, col="#000000")
points(c(0.04166667,0.5,1,2,7,24), c(4.832,0.566,0.254,0.174,0.140,0.091), pch=16, col="#E69F00")
points(c(0.04166667,0.333,1,2,7,24,48,120,144,168,192), c(4.633,0.321,0.256,0.220,0.192,0.086,0.066,0.0469,0.0544,0.0502,0.0309), pch=16, col= "#56B4E9")

#Erttmann 1988 Dose: 15 mg/m2 IV bolus Time:5min  Age: 5-24
points(c(0.1133,0.1136,0.2152,0.2719,0.4531,0.09629,2.005,2.991,3.942,5.007,6.0153,7.545,7.998,8.247,8.496,9.176,10.071,10.671),c(1.979,0.772,0.275,0.165,0.0772,0.0494,0.0375,0.0296,0.026,0.022,0.0173,0.0162,0.01998,0.0184,0.0169,0.0173,0.0163,0.0144),pch=16, col= "black")


#Speth, 1987 Dose:9 mg/m2 per day (36 mg/m2 in total) Advanced multiple myeloma
points(c(0.945,2.833,5.722,19.484,23.837,27.65,47.769,56.62,71.71,81.86,93.53,95.61,99.58,105.61,118.34,142.52,167.57,191.52),c(0.00378,0.00392,0.0149,0.0172,0.0196,0.0255,0.0237,0.0248,0.0303,0.0279,0.0295,0.0314,0.0126,0.0136,0.0093,0.00577,0.00253,0.00329), pch=16, col= "black")
#Muller, 1993 9 mg/m2 per day (36 mg/m2 in total) Age:70 Cancer
points(c(24.04,48.33,71.87,96.40,96.13,97.92,100.23,104.62,120.12,144.38,168.63,216.14),c(0.0213,0.0222,0.0293,0.0261,0.0228,0.0196,0.0178,0.0175,0.0169,0.014,0.0104,0.008), pch=16, col= "#56B4E9")
#Bugat 1989 Dose:15mg/m2/day (60 mg/m2 in total) infusion iv Age: 43-70 Cancer
points(c(4.517,12.277,24.613,36.504,48.473,60.278,72.169,83.99,96.26,98.26,108.06,111.94,119.91,132.29,144.17),c(0.0194,0.0291,0.0358,0.0364,0.0321,0.0357,0.036,0.0437,0.0368,0.0277,0.0219,0.0212,0.0184,0.0162,0.015),pch=16, col= "black")


#Kerr 1986 Dose: 40 mg/m2 IV bolus Age:median: 52 small cell lung cancer
points(c(0.2695,0.4383,0.6395,1.0382,1.4328,2.0060,3.9918,5.9521,7.9841,9.9807,12.0130,15.9709,23.9269,47.9309),c(2.058,0.911,0.314,0.295,0.214,0.127,0.057,0.051,0.043,0.038,0.033,0.028,0.025,0.011),pch=16, col= "black")

#Serum not plasma Gunvén 1986 Dose: 10mg Intraoperative IV injection Time: 5-10 min Gastrointestinal cancer
points(c(0.127,0.277,0.657,0.988),c(2.054,0.362,0.151,0.056),pch=16, col= "black")
points(c(0.159,0.328,0.826,1.489),c(0.902,0.244,0.039,0.016),pch=16, col= "black")
points(c(0.330,0.494,0.659,0.989),c(0.118,0.043,0.041,0.012),pch=16, col= "black")
points(c(0.247,0.660,1.488),c(0.103,0.032,0.015),pch=16, col= "black")
points(c(0.161,0.278,0.350,0.496),c(0.122,0.052,0.022,0.013),pch=16, col= "black")
points(c(0.080,0.192,0.280,0.351,0.574,0.826),c(0.034,0.047,0.038,0.014,0.014,0.012),pch=16, col= "black")
#Skeletal muscle concentration (unit = nmol per gram tissue)
points(c(0.187,0.512,1.054,1.529),c(0.043,0.043,0.077,0.055),pch=16, col= "black")
points(c(0.244,0.569,1.177,2.665),c(0.390,0.321,0.413,0.240),pch=16, col= "black")
points(c(0.430,1.016),c(0.413,0.304),pch=16, col= "black")
points(c(0.242,0.512,0.689,1.013,1.714),c(0.291,0.183,0.220,0.274,0.236),pch=16, col= "black")
points(c(0.163,0.351,0.975),c(0.139,0.194,0.210),pch=16, col= "black")
points(c(0.351,0.784,1.057),c(0.211,0.188,0.060),pch=16, col= "black")
points(c(0.487,0.689,0.972,1.231),c(0.216,0.137,0.113,0.101),pch=16, col= "black")
points(c(0.533,1.349),c(0.115,0.164),pch=16, col= "black")

#Eksborg, 1986 Dose: 40mg IV bolus Time: 3 min Age:42-70 Ovarian carcinoma (Time after adm (hr)) 
points(c(0.000+0.05,0.141+0.05,0.449+0.05,0.652+0.05,0.905+0.05,1.914+0.05,2.950+0.05,5.962+0.05,11.952+0.05,17.914+0.05,23.927+0.05),c(2.078,0.431,0.101,0.054,0.046,0.025,0.023,0.014,0.009,0.007,0.009),pch=16, col= "black")

#Chan, 1978 IV bolus Time: 1-2 minSolid tumors
# Patient 1: hepatoma, normal renal/hep function, 30 mg/m2
points(c(0.170,0.150,0.356,0.608,0.788,1.029,1.759,2.755,3.819,4.791),c(1.098,0.740,0.359,0.239,0.185,0.149,0.101,0.112,0.076,0.059),pch=16, col= "black")
# Patient 7: soft tissue sarcoma, normal ren/hep function, 35 mg/m2
points(c(0.214,0.295,0.639,1.177,2.199,2.941,3.992,6.240,7.506,13.028,23.432,32.487),c(0.929,0.670,0.257,0.127,0.117,0.140,0.105,0.068,0.078,0.080,0.061,0.060),pch=16, col= "black")
# Patient 11: embryo cell carcinoma, normal ren/hep function, 30 mg/m2
points(c(0.180,0.255,0.670,1.027,1.557,1.938,2.680,3.977,4.961,5.960,22.962),c(1.401,0.585,0.117,0.145,0.096,0.112,0.124,0.086,0.090,0.080,0.047),pch=16, col= "#009E73")
# Patient 8: bronchogenic carcinoma, normal ren/hep, 20 mg/m2
points(c(0.675,0.872,1.046,1.233,1.768,2.492,3.167,4.055,27.926),c(2.646,1.789,0.568,0.209,0.168,0.103,0.103,0.076,0.064),pch=16, col= "black")
# Patient 10: colon carcinoma, normal ren/hep, 30 mg/m2
points(c(0.165,0.436,0.924,1.367,4.768,19.829),c(2.220,0.508,0.194,0.159,0.128,0.058),pch=16, col= "#F0E442")
points(c(0.164,0.521,0.994,1.382,4.922,19.987),c(0.257,0.062,0.045,0.026,0.021,0.025),pch=16, col= "#56B4E9")

# Cummings, 1986 Dose: 25 mg/m2 IV bolus Cancer DOX liver concentration (ng/g) 27 min +- 16 min 5570 +- 1500

# Eksborg, 1985 Dose: 50 mg or 33.6 mg/m2  IV bolus Time: 3 min Age:63 Colon cancer, normal liver function
points(c(0.020,0.147,0.430,0.672,0.914,1.930,2.890,5.858,11.833,17.935,23.928),c(3.865,0.387,0.163,0.111,0.081,0.055,0.045,0.038,0.024,0.020,0.013),pch=16, col= "black")



