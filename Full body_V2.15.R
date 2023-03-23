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
  #age (y)
  age <- runif(N,18,60)     #age (range) multiple virtual male individuals (uniform distribution)
  
  #Height(cm)/Weight(kg)/BSA(m2) MALE
  Height_male <- runif(N,160,190) 
  Weight_male <- runif(N,50,100) 
  BSA_male <- 0.007184 * Height_male^0.725*Weight_male^0.425 #Body surface area according to [DuBois-DuBois 1916]
    
  t_end <- 10 #[h] time of the end of simulation
  times <- seq(0, t_end, by = 0.1) #time of simulation
  
  pKa <- 8.46 # [amine]
  MW <- 543.52 # g/mol
  
  oral_dose <- 0 #[mg] oral bolus dose
  inf_dose_mg <- 60 * BSA_male #[mg] infusion dose according to Pfizer guidance : https://www.pfizermedicalinformation.com/en-us/doxorubicin/dosage-admin
  inf_dose <- (inf_dose_mg * 0.001 / MW ) * 1000000 # [umol]
  inf_time <- 1/30 #[h] infusion time
  
  CO  <- 1.1 * BSA_male - 0.05 * age + 5.5 #[L/min] cardiac output from Tylukia
  CO <- CO * 60 #[L/h] cardiac output units change from [L/min] to [L/h]
  
  CO <- BSA_male * (110 + (184.974 * (exp(-0.0378 * age) -exp(-0.2477 * age)))) #[journal.pcbi.1008786]

  #SEX DEPENDENT BLOOD FLOWS for healthy population according to [Simcyp]
  # BLOOD FLOWS [L/h] -------------------------------------------------------
  Qbr <- CO  * 0.12 #Brain
  Qhe <- CO  * 0.04 #Heart
  Qsk <- CO  * 0.05 #Skin
  Qmu <- CO  * 0.17 #Muscle
  Qad <- CO  * 0.05 #Adipose
  Qsp <- CO  * 0.02 #Spleen
  Qgu <- CO  * 0.16 #Gut
  Qki <- CO  * 0.19 #Kidney
  Qha <- CO  * 0.065#Hepatic artery
  Qbo <- CO  * 0.05 #Bone
  Qre <- CO  * 0.085#Rest
  Qlu <- CO #lung
  Qli <- Qha + Qsp + Qgu#Liver
  
  #SEX DEPENDENT BLOOD FLOWS for healthy population according to [journal.pcbi.1008786]
  # BLOOD FLOWS [L/h] -------------------------------------------------------
  Qbr <- CO  * (( 10 + 2290 *(exp(-0.608*age) - exp(-0.639*age)))/100) #Brain
  Qad <- CO  * 0.05 #Adipose
  Qbo <- CO  * 0.05 #Bone
  Qhe <- CO  * 0.04 #Heart
  Qki <- CO  * (( 4.53 + (14.63*age^1/(0.188^1 + age^1)))/100)
  Qmu <- CO  * ((6.03 + (12*age^2.5/(11^2.5 + age^2.5)))/100)
  Qsk <- CO  * ((1.0335 + (4*age^5/(5.16^5 + age^5)))/100)
  Qsp <- CO  * 0.02 #Spleen
  Qgu <- CO  * 0.15 #Gut
  Qha <- CO  * 0.065#Hepatic artery
  Qli <- CO  * 0.235#Liver
  Qre <- CO - Qbr - Qad - Qbo - Qhe - Qki - Qmu - Qsk - Qsp - Qgu - Qha
  Qre <- ifelse (Qre<0,Qre,0)
  Qlu <- CO * 1- Qre # if Qrestmale would become negative Qlungmale is Qcarout - the negative value (so, increase in Qlung) to keep fluid flow balance, otherwise fraction of CO=1
  Qre[Qre<0]<-0.1 #included to set negative restflow to 0.1
  
  # ORGAN VOLUMES [L] -----------------------------------------------------------
  #BW fraction (according to Simcyp Simulator) * random BW / tissue density
  Vad <- (0.259 * Weight_male) / 0.923
  Vbo <- (0.090 * Weight_male) / 1.850
  Vbr <- (0.017 * Weight_male) / 1.04
  Vgu <- (0.016 * Weight_male) / 1.04
  Vhe <- (0.005 * Weight_male) / 1.04
  Vki <- (0.004 * Weight_male) / 1.05
  Vli <- (0.022 * Weight_male) / 1.08
  Vlu <- (0.007 * Weight_male) / 1.05
  Vmu <- (0.403 * Weight_male) / 1.04
  Vsk <- (0.043 * Weight_male) / 1.1
  Vsp <- (0.002 * Weight_male) / 1.06
  Vre <- (0.057 * Weight_male) / 1.05
  Vpl <- (0.044 * Weight_male) / 1.025
  Vrb <- (0.031 * Weight_male) / 1.125
  Vbl <- Vpl + Vrb
  
  # ORGAN VOLUMES [L] (according to [journal.pcbi.1008786])-----------------------------------------------------------
  
  Vbr <- (10 * (age + 0.315) / (9 + 6.92 * age)) / 1.04
  Vlu <- ((29.08 * (Height_male/ 100) * Weight_male ^ 0.5 + 11.06 + 35.47 * (Height_male/100) * Weight_male^0.5 +5.53)/1000)/1.05
  Vad <- (1.36 * Weight_male) / (Height_male/ 100) - 42 #adipose (>12y)
  Vad[Vad<0] <- 0.01
  Vad <- ifelse((age >12.3), Vad, 0)
  
  Vhe <- (( 22.81 * (Height_male/100) * Weight_male^0.5 -4.15)/1000)/1.05
  Vki <- (4.214 * Weight_male^0.823 + 4.456*Weight_male^0.795)/1000
  Vmu <- (0.3 + ((0.54-0.3)/18)* age) * (Weight_male - Vad * 0.92)/1.04
  Vsk <- (BSA_male/1000) * 45.655+ (BSA_male/1000) * 1240
  Vsp <- ((8.74 * (Height_male /100) * Weight_male^0.5 + 11.06)/1000)*1.06
  Vgu <- (0.021* (Weight_male - Vad * 0.92))/1.05
  Vli <- ((576.9*(Height_male/100) + 8.9*Weight_male - 159.7)/1000)/1.05
  
  Vbo_ic <- 22.5 / 100 * Vbo
  Vbo_ec <- 77.5 / 100 * Vbo
  
  Vbr_ic <- 87.5 / 100 * Vbr
  Vbr_ec <- 12.5 / 100 * Vbr
  
  Vsk_ic <- 87.5 / 100 * Vsk
  Vsk_ec <- 12.5 / 100 * Vsk
  
  Vli_ic <- 80.75 / 100 * Vli
  Vli_ec <- 19.25 / 100 * Vli
  
  Vki_ic <- 83.34 / 100 * Vki
  Vki_ec <- 16.66 / 100 * Vki
  
  Vlu_ic <- 72.89 / 100 * Vlu
  Vlu_ec <- 27.11 / 100 * Vlu
  
  Vmu_ic <- 72.5 / 100 * Vmu
  Vmu_ec <- 27.5 / 100 * Vmu
  
  Vre_ic <- 87.5 / 100 * Vre
  Vre_ec <- 12.5 / 100 * Vre
  
  Vsk_ic <- 72.5 / 100 * Vsk
  Vsk_ec <- 27.5 / 100 * Vsk
  
  Vsp_ic <- 74.50 / 100 * Vsp
  Vsp_ec <- 25.50 / 100 * Vsp
  
  Vgu_ic <- 82.09 / 100 * Vgu
  Vgu_ec <- 17.91 / 100 * Vgu
  
  Vad_ic <- 87.5 / 100 * Vad
  Vad_ec <- 12.5 / 100 * Vad
}

{
  # MODEL -------------------------------------------------------------------
  #Arguments of the function are the model parameters that vary
  ModelVar <- function (CO,
                        MPPGL,
                        Vad,
                        Vbl,
                        Vrb,
                        Vbo,
                        Vbr,
                        Vgu,
                        Vheart,
                        Vki,
                        Vli,
                        Vlu,
                        Vpl,
                        Vmu,
                        Vsk,
                        Vsp,
                        Vre,
                        Qre,
                        Qad,
                        Qbo,
                        Qbr,
                        Qgu,
                        Qheart,
                        Qki,
                        Qh,
                        Qlu,
                        Qmu,
                        Qsk,
                        Qsp,
                        CYP3A4,
                        tlag,
                        Fabs,
                        t_end,
                        fup,
                        BP,
                        MV,
                        MSA)
  
  times <- seq(0, t_end, by = 0.1)
  
  # PHYSIOLOGICAL PARAMETERS -------------------------------------------------------
  liver_density <- 1080 #[g/L]
  heart_density <- 1055 #[g/L] [Alexandra 2019]
  
  #Tissue volumes [L] -------------------------------------------------------
  Vmyo = 0.8 * Vhe #myocardial
  Vother = 0.2 * Vhe #other
  Vother_ic = 87.5 / 100 * Vother #intracellular 
  Vmyo_ic = 87.5 / 100 * Vmyo #intracellular of myocardial
  Vhe_ec = 12.5 / 100 * Vhe #extracellular space of heart tissue
  Vve = (2 / 3) * Vbl		#venous blood; assumed 2/3 of total blood according to volmues published in CPT. Regarding the distribution of blood volume within the circulation, the greatest volume resides in the venous vasculature, where 70-80% of the blood volume is found. -> http://www.cvphysiology.com/Blood%20Pressure/BP019
  Var = Vbl - Vve		#arterial blood
  Vplas_ven = Vpl * (Vve / (Vve + Var))  #venous plasma
  Vplas_art = Vpl * (Var / (Vve + Var)) 	#arterial plasma
  
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
  
  DNA_br <- 1.5 #umol/L Rapidly perfused organ
  DNA_lu <- 1.5 #umol/L 
  
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
    mtDNA_li <- (li_cell * (cn_li * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vli
    mtDNA_he <- (he_cell * (cn_he * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vhe
    mtDNA_ki <- (ki_cell * (cn_ki * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vki
    mtDNA_bo <- (bo_cell * (cn_bo * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vbo
    mtDNA_gu <- (gu_cell * (cn_gu * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vgu
    
    mtDNA_mu <- (mu_cell * (cn_mu * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vmu
    mtDNA_ad <- (ad_cell * (cn_ad * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vad
    mtDNA_sk <- (sk_cell * (cn_sk * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vsk
    
    mtDNA_br <- (br_cell * (cn_br * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vbr
    mtDNA_lu <- (lu_cell * (cn_lu * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vlu
    mtDNA_sp <- (sp_cell * (cn_sp * 330 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vsp
  }
  
  DNA_other <- 0.05 * DNA_he
  DNA_myo <- 0.95 * DNA_he
  
  mtDNA_other <- 0.05 * mtDNA_he
  mtDNA_myo <- 0.95 * mtDNA_he
  
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
  
  # CARDIOMYOCYTE VOLUME AND SURFACE AREA according to [Polak 2012] -------------------------------------------------------
  MV <- exp(age * 0.04551 + 7.36346) #[um^3]
  
  MSA <-exp(0.860 * log(MV) )#[um^2]
  
  MVol <- MV * (10 ^ -12) #[L] random age dependent myocate volume in cm3 -> changing to liters
  
  #cells amounts -------------------------------------------------------
  cell_amount_other <- Vother_ic / MVol #other heart tissue
  cell_amount_myo <- Vmyo_ic / MVol #myocardial
  
  SA_other <- (cell_amount_other * MSA) / (10 ^ 8) #[cm^2]
  SA_myo <- (cell_amount_myo * MSA) / (10 ^ 8) #[cm^2]
  
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
  r = 244 #[mg]
  t = inf_time #time of infusion [h]
  inf = r / t #infusion rate [mg/h]
  
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
  
  # TISSUE TO PLASMA PARTITION COEFFICIENT -------------------------------------------------------
  
  {
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
    }
    
    {
      # Relative Volume of Wet Tissue (%)
      # Adipose
      EW_ad <- 0.141
      IW_ad <- 0.039
      NL_ad <- 0.79
      NP_ad <- 0.002
      AP_ad <- 0.4 #mg/g the concentration of acidic phospholipids AP in adipose
      
      # Bone
      EW_bo <- 0.098
      IW_bo <- 0.341
      NL_bo <- 0.074
      NP_bo <- 0.0011
      AP_bo <- 0.67 #mg/g the concentration of acidic phospholipids AP in Bone
      
      # Brain
      EW_br <- 0.092
      IW_br <- 0.678
      NL_br <- 0.051
      NP_br <- 0.0565
      AP_br <- 0.4 #mg/g the concentration of acidic phospholipids AP in Brain
      
      # Gut
      EW_gu <- 0.267
      IW_gu <- 0.451
      NL_gu <- 0.0487
      NP_gu <- 0.0163
      AP_gu <- 2.84 #mg/g the concentration of acidic phospholipids AP in Gut 
      
      # Heart
      EW_he <- 0.313
      IW_he <- 0.445
      NL_he <- 0.0115
      NP_he <- 0.0166
      AP_he <- 3.07 #mg/g the concentration of acidic phospholipids AP in Heart 
      
      # Kidney
      EW_ki <- 0.283
      IW_ki <- 0.50
      NL_ki <- 0.0207
      NP_ki <- 0.0162
      AP_ki <- 2.48 #mg/g the concentration of acidic phospholipids AP in kidney 
      
      # Liver
      EW_li <- 0.165
      IW_li <- 0.586
      NL_li <- 0.0348
      NP_li <- 0.0252
      AP_li <- 5.09 #mg/g the concentration of acidic phospholipids AP in liver 
      
      # Lung
      EW_lu <- 0.348
      IW_lu <- 0.463
      NL_lu <- 0.003
      NP_lu <- 0.009
      AP_lu <- 0.5 #mg/g the concentration of acidic phospholipids AP in Lung 
      
      # Muscle
      EW_mu <- 0.091
      IW_mu <- 0.669
      NL_mu <- 0.0238
      NP_mu <- 0.0072
      AP_mu <- 2.49 #mg/g the concentration of acidic phospholipids AP in muscle 
      
      # Pancreas
      EW_pa <- 0.12
      IW_pa <- 0.664
      NL_pa <- 0.041
      NP_pa <- 0.0093
      AP_pa <- 1.67 #mg/g the concentration of acidic phospholipids AP in Pancreas 
      
      # Skin
      EW_sk <- 0.623
      IW_sk <- 0.0947
      NL_sk <- 0.0284
      NP_sk <- 0.0111
      AP_sk <- 1.32 #mg/g the concentration of acidic phospholipids AP in Skin 
      
      # Spleen
      EW_sp <- 0.208
      IW_sp <- 0.579
      NL_sp <- 0.0201
      NP_sp <- 0.0198
      AP_sp <- 2.81 #mg/g the concentration of acidic phospholipids AP in Spleen 
      
      # Plasma
      EW_pl <- 0.945
      IW_pl <- 0
      NL_pl <- 0.0035
      NP_pl <- 0.0023
      AP_pl <- 0.04 #mg/g the concentration of acidic phospholipids AP in Plasma 
      
      # RBC
      EW_rbc <- 0
      IW_rbc <- 0.666
      NL_rbc <- 0.0017
      NP_rbc <- 0.0029
      AP_rbc <- 0.44 #mg/g the concentration of acidic phospholipids AP in Plasma  
    }
    
    {
      #MALE tissue composition
      EW_ad = (32.154 - 2.7863 * log10(age))*(14.1/18/100)                    #EW: extracellular water
      IW_ad = (32.154 - 2.7863 * log10(age))*(3.9/18/100)                     #IW: intracellular water
      NL_ad = (35.5 + 43.46 * age /(1.5 + age))*(79/79.2/100)                 #NL: neutral lipid
      NP_ad = (35.5 + 43.46 * age /(1.5 + age))*(0.2/79.2/100)                #NP: neutral phospholipid
      
      EW_bo=(64.179 - 1.2697 * age)*(9.8/43.9/100)
      IW_bo=(64.179 - 1.2697 * age)*(34.1/43.9/100)
      NL_bo= (0.2 + 0.3655 * age)*(7.4/7.51/100)
      NP_bo= (0.2 + 0.3655 * age)*(0.11/7.51/100)
      
      EW_gu= (75.378 - 0.3932 * log10(age))*(26.7/71.8/100)
      IW_gu=  (75.378 - 0.3932 * log10(age))*(45.1/71.8/100)
      NL_gu= (2.5 + 0.185 * age)*(4.87/6.5/100)
      NP_gu= (2.5 + 0.185 * age)*(1.63/6.5/100)
      
      EW_he= (84.523 - 0.4249 * age)*(31.3/75.8/100)
      IW_he= (84.523 - 0.4249 * age)*(44.5/75.8/100)
      NL_he= (2.3159 + 0.0797 * age)*(1.15/2.81/100)
      NP_he= (2.3159 + 0.0797 * age)*(1.66/2.81/100)
      
      EW_ki= (83.278 - 0.2162 * age)*(28.3/78.3/100)
      IW_ki= (83.278 - 0.2162 * age)*(50/78.3/100)
      NL_ki= (2.73 + 1.995 * age /(2.59 + age))*(2.07/3.69/100)
      NP_ki= (2.73 + 1.995 * age /(2.59 + age))*(1.62/3.69/100)
      
      EW_li= (75.69 - 0.573 * log10(age))*(16.5/75.1/100)
      IW_li= (75.69 - 0.573 * log10(age))*(58.6/75.1/100)
      NL_li= (3 + 3.089 * age/(1.8 + age))*(3.48/6/100)
      NP_li= (3 + 3.089 * age/(1.8 + age))*(2.52/6/100)
      
      EW_lu= (80.973 - 0.4916 * log10(age))*(34.8/81.1/100)
      IW_lu= (80.973 - 0.4916 * log10(age))*(46.3/81.1/100)
      NL_lu= (1.857 - 0.211 * log10(age))*(0.3/1.2/100)
      NP_lu= (1.857 - 0.211 * log10(age))*(0.9/1.2/100)
      
      EW_mu= (77.211 - 0.4321 * log10(age))*(9.1/76/100)
      IW_mu= (77.211 - 0.4321 * log10(age))*(66.9/76/100)
      NL_mu= (1.9852 + 0.0649 * age)*(2.38/3.1/100)
      NP_mu= (1.9852 + 0.0649 * age)*(0.72/3.1/100)
      
      EW_sk= (72.395 - 1.1462 * log10(age))*(62.3/71.77/100)
      IW_sk= (72.395 - 1.1462 * log10(age))*(9.47/71.77/100)
      NL_sk=3.95*(2.84/3.95/100)
      NP_sk=3.95*(1.11/3.95/100)
      
      EW_sp= (79.952 - 0.4178 * log10(age))*(20.8/78.7/100)
      IW_sp= (79.952 - 0.4178 * log10(age))*(57.9/78.7/100)
      NL_sp=(1.5 + 0.015 * age)*(2.01/3.99/100)
      NP_sp=(1.5 + 0.015 * age)*(1.98/3.99/100)
      
      NL_pl= (0.5578 + 0.036 * log10(age))*(0.35/0.57/100)
      NP_pl= (0.5578 + 0.036 * log10(age))*(0.22/0.57/100)
      
      IW_rbc=66/100
      NL_rbc=0.3*(0.17/0.46/100)
      NP_rbc=0.3*(0.29/0.46/100)
      AP_rbc <- 0.44 #mg/g the concentration of acidic phospholipids AP in Plasma  
    }
    
    #Hematocrit
    #MALE
    HCT_male=(53 - ((43.0 * age^1.12 / (0.05^1.12 + age^1.12)) * (1 + (-0.93 * age^0.25 / (0.10^0.25 + age^0.25)))))/100
    HCT_male_CV=rnorm(N,0,0.065)
    HCT_male= HCT_male*exp(HCT_male_CV)
    
    #Blood-plasma ratio
    # BP=1-Ht + EP*Ht 
    BP <- 1- HCT_male + 1.34 * HCT_male  #blood plasma ratio
    
    Kpu_rbc <- (BP + HCT_male - 1) / (HCT_male * fup)
    KaAP <- (Kpu_rbc - (X_rbc / X_ew * IW_rbc) - ((logPow * NL_rbc + (0.3 * logPow + 0.7) * NP_rbc) /X_ew)) * (X_ew /  (AP_rbc * Y_rbc))#affinity constant for acidic phospholipids (AP) 
    
    Kpu_ad <- EW_ad + (X_iw / X_ew) * IW_ad + ( Pad * NL_ad + (0.3 * P + 0.7) * NP_ad) / EW_ad + KaAP * AP_ad * Y_iw / X_ew
    Kpu_bo <- EW_bo + (X_iw / X_ew) * IW_bo + ( P * NL_bo + (0.3 * P + 0.7) * NP_bo) / EW_bo + KaAP * AP_bo * Y_iw / X_ew
    Kpu_br <- EW_br + (X_iw / X_ew) * IW_br + ( P * NL_br + (0.3 * P + 0.7) * NP_br) / EW_br + KaAP * AP_br * Y_iw / X_ew
    Kpu_gu <- EW_gu + (X_iw / X_ew) * IW_gu + ( P * NL_gu + (0.3 * P + 0.7) * NP_gu) / EW_gu + KaAP * AP_gu * Y_iw / X_ew
    Kpu_he <- EW_he + (X_iw / X_ew) * IW_he + ( P * NL_he + (0.3 * P + 0.7) * NP_he) / EW_he + KaAP * AP_he * Y_iw / X_ew
    Kpu_ki <- EW_ki + (X_iw / X_ew) * IW_ki + ( P * NL_ki + (0.3 * P + 0.7) * NP_ki) / EW_ki + KaAP * AP_ki * Y_iw / X_ew
    Kpu_li <- EW_li + (X_iw / X_ew) * IW_li + ( P * NL_li + (0.3 * P + 0.7) * NP_li) / EW_li + KaAP * AP_li * Y_iw / X_ew
    Kpu_lu <- EW_lu + (X_iw / X_ew) * IW_lu + ( P * NL_lu + (0.3 * P + 0.7) * NP_lu) / EW_lu + KaAP * AP_lu * Y_iw / X_ew
    Kpu_mu <- EW_mu + (X_iw / X_ew) * IW_mu + ( P * NL_mu + (0.3 * P + 0.7) * NP_mu) / EW_mu + KaAP * AP_mu * Y_iw / X_ew
    Kpu_pa <- EW_pa + (X_iw / X_ew) * IW_pa + ( P * NL_pa + (0.3 * P + 0.7) * NP_pa) / EW_pa + KaAP * AP_pa * Y_iw / X_ew
    Kpu_sk <- EW_sk + (X_iw / X_ew) * IW_sk + ( P * NL_sk + (0.3 * P + 0.7) * NP_sk) / EW_sk + KaAP * AP_sk * Y_iw / X_ew
    Kpu_sp <- EW_sp + (X_iw / X_ew) * IW_sp + ( P * NL_sp + (0.3 * P + 0.7) * NP_sp) / EW_sp + KaAP * AP_sp * Y_iw / X_ew
    Kpu_pl <- EW_pl + (X_iw / X_ew) * IW_pl + ( P * NL_pl + (0.3 * P + 0.7) * NP_pl) / EW_pl + KaAP * AP_pl * Y_iw / X_ew
    
    Kpad <- Kpu_ad * fup
    Kpbo <- Kpu_bo * fup
    Kpbr <- Kpu_br * fup
    Kpgu <- Kpu_gu * fup
    Kphe <- Kpu_he * fup
    Kpki <- Kpu_ki * fup
    Kpli <- Kpu_li * fup
    Kplu <- Kpu_lu * fup
    Kpmu <- Kpu_mu * fup
    Kpre <- Kpu_pa * fup
    Kpsk <- Kpu_sk * fup
    Kpsp <- Kpu_sp * fup
    Kppl <- Kpu_pl * fup
    
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
    Kpec_he <-  (1- he_ratio) * fup + he_ratio
    Kpec_ad <-  (1- ad_ratio) * fup + ad_ratio
    Kpec_bo <-  (1- bo_ratio) * fup + bo_ratio
    Kpec_br <-  (1- br_ratio) * fup + br_ratio
    Kpec_gu <-  (1- gu_ratio) * fup + gu_ratio
    Kpec_ki <-  (1- ki_ratio) * fup + ki_ratio
    Kpec_li <-  (1- li_ratio) * fup + li_ratio
    Kpec_lu <-  (1- lu_ratio) * fup + lu_ratio
    Kpec_mu <-  (1- mu_ratio) * fup + mu_ratio
    Kpec_sk <-  (1- sk_ratio) * fup + sk_ratio
    Kpec_sp <-  (1- sp_ratio) * fup + sp_ratio
    Kpec_pa <-  (1- pa_ratio) * fup + pa_ratio
  }
  
  # CL clearance [Leandro 2019] -------------------------------------------------------
  CL_renal <- 0.66 #[L/ h] renal clearance
  CL_hepatic <- 29.97 #[L/ h] hepatic clearance
  CLint_heart <- 0 #[L/ h] heart clearance CYP3A4 was not detected in heart tissue
  CL_total <- 30.6 #[L/ h] total clearance
  
  # MODEL -------------------------------------------------------------------
  parameters <- c(
    BP = BP,
    Kplu = Kplu,
    Kpli = Kpli,
    Kpad = Kpad,
    Kpbo = Kpbo,
    Kpbr = Kpbr,
    Kpgu = Kpgu,
    Kpki = Kpki,
    Kpmu = Kpmu,
    Kpsk = Kpsk,
    Kpsp = Kpsp,
    Kpre = Kpre,
    Kphe = Kphe,
    Vother = Vother,
    Vmyo = Vmyo,
    fup = fup,
    funionized_ic = funionized_ic,
    funionized_ec = funionized_ec,
    fu_ec = fu_ec,
    CL_renal = CL_renal,
    CL_hepatic = CL_hepatic
  )

  # Initialize compartments PBPK model --------------------------------------
  PBPKInitmale<- function(inf_dose) 
  {
    # initialize male
    # State variables -------------------------------------------------------
    state <- c(
      INFUSION = r,
      Aad = 0,
      Abo = 0,
      Abr = 0,
      Agu = 0,
      Aki = 0,
      Ali = 0,
      Alu = 0,
      Amu = 0,
      Ask = 0,
      Asp = 0,
      Ahe = 0,
      Ave = 0,
      Aar = 0,
      Are = 0,
      Ahe_ec = 0,
      Aad_ec = 0,
      Abo_ec = 0,
      Abr_ec = 0,
      Agu_ec = 0,
      Aki_ec = 0,
      Ali_ec = 0,
      Alu_ec = 0,
      Amu_ec = 0,
      Ask_ec = 0,
      Asp_ec = 0,
      Abloodcell = 0,
      
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
      
      Cmyo_E1R = 0,
      Cmyo_E1 = 7.885,
      Cmyo_E2R = 0,
      Cmyo_E2 = 41.61,
      Cmyo_E3R = 0,
      Cmyo_E3 = 7.885,
      
      Cother_E1R = 0,
      Cother_E1 = DNA_other,
      Cother_E2R = 0,
      Cother_E2 = Cardiolipin_other,
      Cother_E3R = 0,
      Cother_E3 = 0.7885,
      
      Cad_E1R = 0,
      Cad_E1 = 4.5,
      Cad_E2R = 0,
      Cad_E2 = 15,
      Cad_E3R = 0,
      Cad_E3 = 0.45,
      
      Cbo_E1R = 0,
      Cbo_E1 = 19.1,
      Cbo_E2R = 0,
      Cbo_E2 = 25,
      Cbo_E3R = 0,
      Cbo_E3 = 1.91,
      
      Cbr_E1R = 0,
      Cbr_E1 = 1.5,
      Cbr_E2R = 0,
      Cbr_E2 = 30,
      Cbr_E3R = 0,
      Cbr_E3 = 0.15,
      
      Cgu_E1R = 0,
      Cgu_E1 = 4.5,
      Cgu_E2R = 0,
      Cgu_E2 = 25,
      Cgu_E3R = 0,
      Cgu_E3 = 0.45,
      
      Cki_E1R = 0,
      Cki_E1 = 16.2,
      Cki_E2R = 0,
      Cki_E2 = 52.3,
      Cki_E3R = 0,
      Cki_E3 = 1.62,
      
      Cli_E1R = 0,
      Cli_E1 = 23.7,
      Cli_E2R = 0,
      Cli_E2 = 44.6,
      Cli_E3R = 0,
      Cli_E3 = 2.37,
      
      Clu_E1R = 0,
      Clu_E1 = 1.5,
      Clu_E2R = 0,
      Clu_E2 = 30,
      Clu_E3R = 0,
      Clu_E3 = 0.15,
      
      Cmu_E1R = 0,
      Cmu_E1 = 4.5,
      Cmu_E2R = 0,
      Cmu_E2 = 15,
      Cmu_E3R = 0,
      Cmu_E3 = 0.45,
      
      Csk_E1R = 0,
      Csk_E1 = 4.5,
      Csk_E2R = 0,
      Csk_E2 = 15,
      Csk_E3R = 0,
      Csk_E3 = 0.45,
      
      Csp_E1R = 0,
      Csp_E1 = 4.5,
      Csp_E2R = 0,
      Csp_E2 = 15,
      Csp_E3R = 0,
      Csp_E3 = 0.45
    )
    
    return(state)
  }
  
  ###Differential equations - mg/h/L -------------------------------------------------------
  PBPKModel = function(times, state, parameters) {
    with(as.list(c(state, parameters)), {
      inf <- ifelse(times <= t, inf, 0)
      #DOX concentrations:
      Cadipose <- Aad / Vad    #adipose
      Cbone <- Abo / Vbo		   #bone
      Cbrain <- Abr / Vbr		   #brain
      Cgut <- Agu / Vgu			   #gut
      Ckidney <- Aki / Vki	   #kidney
      Cliver <- Ali / Vli		   #liver
      Cliverfree <-  Cliver * (fup / BP)  #liver free concentration
      Ckidneyfree <- Ckidney * (fup / BP) #kidney free concentration
      Clung <- Alu / Vlu		   #lung
      Cmuscle <- Amu / Vmu	   #muscle
      Cskin <- Ask / Vsk		   #skin
      Cspleen <- Asp / Vsp	   #spleen
      Cheart <- Ahe / Vhe	    #heart
      Crest <- Are / Vre 			#rest of body
      Cvenous <- Ave / Vve     #venous blood
      Carterial <- Aar / Var	 #arterial blood
      Cplasmavenous <- Cvenous / BP	#venous plasma concentration
      Cbloodcell <- Abloodcell / Vrb#blood cell concentration
      
      Che_ec <- Ahe_ec / Vhe_ec   #heart extracellular fluid
      Cad_ec <- Aad_ec / Vad_ec 
      Cbo_ec <- Abo_ec / Vbo_ec
      Cbr_ec <- Abr_ec / Vbr_ec
      Cgu_ec <- Agu_ec / Vgu_ec
      Cki_ec <- Aki_ec / Vki_ec
      Cli_ec <- Ali_ec / Vli_ec
      Clu_ec <- Alu_ec/ Vlu_ec
      Cmu_ec <- Amu_ec/ Vmu_ec
      Csk_ec <- Ask_ec/ Vsk_ec
      Csp_ec <- Asp_ec / Vsp_ec
      
      # Extracellular sub-compartment
      dAhe_ec <-  Qhe * (Carterial - (Che_ec/Kphe) * BP) -(0.001 * PER * SA_myo * (Che_ec * fuec_he * funionized_ec - Cmyo_ict/(Kpec_he * Kpp) * funionized_ic)) - (0.001 *PER *  SA_other * (Che_ec * fu_ec * funionized_ec - Cother_ict /(Kpec_he * Kpp) * funionized_ic))
      dAad_ec <- Qad * (Carterial - (Cad_ec/Kpad) * BP) - (0.001 * PER * SA_ad * (Cad_ec * fuec_ad * funionized_ec - Cad_ict/(Kpec_ad * Kpp) * funionized_ic))
      dAbo_ec <- Qbo * (Carterial - (Cbo_ec/Kpbo) * BP) - (0.001 * PER * SA_bo * (Cbo_ec * fuec_bo * funionized_ec - Cbo_ict/(Kpec_bo * Kpp) * funionized_ic))  
      dAbr_ec <- Qbr * (Carterial - (Cbr_ec/Kpbr) * BP) - (0.001 * PER * SA_br * (Cbr_ec * fuec_br * funionized_ec - Cbr_ict/(Kpec_br * Kpp) * funionized_ic))
      dAgu_ec <- Qgu * (Carterial - (Cgu_ec/Kpgu) * BP) - (0.001 * PER * SA_gu * (Cgu_ec * fuec_gu * funionized_ec - Cgu_ict/(Kpec_gu * Kpp) * funionized_ic))
      dAki_ec <- Qki * (Carterial - (Cki_ec/Kpki) * BP) - (0.001 * PER * SA_ki * (Cki_ec * fuec_ki * funionized_ec - Cki_ict/(Kpec_ki * Kpp) * funionized_ic))
      dAli_ec <- Qli * (Carterial - (Cli_ec/Kpli) * BP) - (0.001 * PER * SA_li * (Cli_ec * fuec_li * funionized_ec - Cli_ict/(Kpec_li * Kpp) * funionized_ic))
      dAlu_ec <- Qlu * (Carterial - (Clu_ec/Kplu) * BP) - (0.001 * PER * SA_lu * (Clu_ec * fuec_lu * funionized_ec - Clu_ict/(Kpec_lu * Kpp) * funionized_ic))
      dAmu_ec <- Qmu * (Carterial - (Cmu_ec/Kpmu) * BP) - (0.001 * PER * SA_mu * (Cmu_ec * fuec_mu * funionized_ec - Cmu_ict/(Kpec_mu * Kpp) * funionized_ic))
      dAsk_ec <- Qsk * (Carterial - (Csk_ec/Kpsk) * BP) - (0.001 * PER * SA_sk * (Csk_ec * fuec_sk * funionized_ec - Csk_ict/(Kpec_sk * Kpp) * funionized_ic))
      dAsp_ec <- Qsp * (Carterial - (Csp_ec/Kpsp) * BP) - (0.001 * PER * SA_sp * (Csp_ec * fuec_sp * funionized_ec - Csp_ict/(Kpec_sp * Kpp) * funionized_ic))
      
      # Subcompartments intracellular total concentration
      dCmyo_ict <- (0.001 * PER * SA_myo * ((Che_ec * fu_ec * funionized_ec - Cmyo_ict/(Kpec_he * Kpp) * funionized_ic ) - (Cmyo_ict * fu_heart * CLint_heart * (Vmyo_ic/Vhe))) / Vmyo_ic) - (kon1 * Cmyo_ict * Cmyo_E1 - koff1 * Cmyo_E1R) - (kon2 * Cmyo_ict * Cmyo_E2 - koff2 * Cmyo_E2R ) - (kon3 * Cmyo_ict * Cmyo_E3 - koff3 * Cmyo_E3R )
      dCother_ict <- (0.001 * PER * SA_other * ((Che_ec * fu_ec * funionized_ec - Cother_ict/(Kpec_he * Kpp) * funionized_ic) - (Cother_ict * fu_heart * CLint_heart * (Vother_ic/Vhe)))/Vother_ic) - (kon1 * Cother_ict * Cother_E1 - koff1 * Cother_E1R) - (kon2 * Cother_ict * Cother_E2 - koff2 * Cother_E2R ) - (kon3 * Cother_ict * Cother_E3 - koff3 * Cother_E3R )
      dCad_ict <- (0.001 * PER * SA_ad * ((Cad_ec * fuec_ad * funionized_ec - Cad_ict/(Kpec_ad * Kpp) * funionized_ic)) / Vad_ic) -(kon1 * Cad_ict * Cad_E1 - koff1 * Cad_E1R) - (kon2 * Cad_ict * Cad_E2 - koff2 * Cad_E2R) - (kon3 * Cad_ict * Cad_E3 - koff3 * Cad_E3R) 
      dCbo_ict <- (0.001 * PER * SA_bo * ((Cbo_ec * fuec_bo * funionized_ec - Cbo_ict/(Kpec_bo * Kpp) * funionized_ic)) / Vbo_ic)-(kon1 * Cbo_ict * Cbo_E1 - koff1 * Cbo_E1R) - (kon2 * Cbo_ict * Cbo_E2 - koff2 * Cbo_E2R) - (kon3 * Cbo_ict * Cbo_E3 - koff3 * Cbo_E3R) 
      dCbr_ict <- (0.001 * PER * SA_br * ((Cbr_ec * fuec_br * funionized_ec - Cbr_ict/(Kpec_br * Kpp) * funionized_ic)) / Vbr_ic) -(kon1 * Cbr_ict * Cbr_E1 - koff1 * Cbr_E1R) - (kon2 * Cbr_ict * Cbr_E2 - koff2 * Cbr_E2R) - (kon3 * Cbr_ict * Cbr_E3 - koff3 * Cbr_E3R) 
      dCgu_ict <- (0.001 * PER * SA_gu * ((Cgu_ec * fuec_gu * funionized_ec - Cgu_ict/(Kpec_gu * Kpp) * funionized_ic)) / Vgu_ic) -(kon1 * Cgu_ict * Cgu_E1 - koff1 * Cgu_E1R) - (kon2 * Cgu_ict * Cgu_E2 - koff2 * Cgu_E2R) - (kon3 * Cgu_ict * Cgu_E3 - koff3 * Cgu_E3R) 
      dCki_ict <- (0.001 * PER * SA_ki * ((Cki_ec * fuec_ki * funionized_ec - Cki_ict/(Kpec_ki * Kpp) * funionized_ic)) / Vki_ic) -(kon1 * Cki_ict * Cki_E1 - koff1 * Cki_E1R) - (kon2 * Cki_ict * Cki_E2 - koff2 * Cki_E2R) - (kon3 * Cki_ict * Cki_E3 - koff3 * Cki_E3R) 
      dCli_ict <- (0.001 * PER * SA_li * ((Cli_ec * fuec_li * funionized_ec - Cli_ict/(Kpec_li * Kpp) * funionized_ic)) / Vli_ic) -(kon1 * Cli_ict * Cli_E1 - koff1 * Cli_E1R) - (kon2 * Cli_ict * Cli_E2 - koff2 * Cli_E2R) - (kon3 * Cli_ict * Cli_E3 - koff3 * Cli_E3R) 
      dClu_ict <- (0.001 * PER * SA_lu * ((Clu_ec * fuec_lu * funionized_ec - Clu_ict/(Kpec_lu * Kpp) * funionized_ic)) / Vlu_ic) -(kon1 * Clu_ict * Clu_E1 - koff1 * Clu_E1R) - (kon2 * Clu_ict * Clu_E2 - koff2 * Clu_E2R) - (kon3 * Clu_ict * Clu_E3 - koff3 * Clu_E3R) 
      dCmu_ict <- (0.001 * PER * SA_mu * ((Cmu_ec * fuec_mu * funionized_ec - Cmu_ict/(Kpec_mu * Kpp) * funionized_ic)) / Vmu_ic) -(kon1 * Cmu_ict * Cmu_E1 - koff1 * Cmu_E1R) - (kon2 * Cmu_ict * Cmu_E2 - koff2 * Cmu_E2R) - (kon3 * Cmu_ict * Cmu_E3 - koff3 * Cmu_E3R) 
      dCsk_ict <- (0.001 * PER * SA_sk * ((Csk_ec * fuec_sk * funionized_ec - Csk_ict/(Kpec_sk * Kpp) * funionized_ic)) / Vsk_ic) -(kon1 * Csk_ict * Csk_E1 - koff1 * Csk_E1R) - (kon2 * Csk_ict * Csk_E2 - koff2 * Csk_E2R) - (kon3 * Csk_ict * Csk_E3 - koff3 * Csk_E3R) 
      dCsp_ict <- (0.001 * PER * SA_sp * ((Csp_ec * fuec_sp * funionized_ec - Csp_ict/(Kpec_sp * Kpp) * funionized_ic)) / Vsp_ic)-(kon1 * Csp_ict * Csp_E1 - koff1 * Csp_E1R) - (kon2 * Csp_ict * Csp_E2 - koff2 * Csp_E2R) - (kon3 * Csp_ict * Csp_E3 - koff3 * Csp_E3R) 
      
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
      
      ## rates of changes
      dINFUSION <- -inf
      dAad <- Qad * (Carterial - Cadipose / Kpad * BP) #adipose
      dAbo <- Qbo * (Carterial - Cbone / Kpbo * BP) #bone
      dAbr <- Qbr * (Carterial - Cbrain / Kpbr * BP) #brain
      dAgu <- Qgu * (Carterial - Cgut / Kpgu * BP) #gut
      dAki <- Qki * (Carterial - Ckidney / Kpki * BP) - CL_renal * Ckidneyfree  #kidney
      dAli <- Qha * Carterial + Qgu * (Cgut / Kpgu * BP) + Qsp * (Cspleen / Kpsp * BP) - Qli * (Cliver / Kpli * BP) - CL_hepatic * Cliverfree #liver
      dAlu <- Qlu * Cvenous - Qlu * (Clung / Kplu * BP) #lung
      dAmu <- Qmu * (Carterial - Cmuscle / Kpmu * BP)   #muscle
      dAsk <- Qsk * (Carterial - Cskin / Kpsk * BP)  		#skin
      dAsp <- Qsp * (Carterial - Cspleen / Kpsp * BP)  	#spleen
      dAhe <- Qhe * (Carterial - (Cheart/ Kphe) * BP) #heart
      dAre <- Qre * (Carterial - Crest / Kpre * BP)  		#rest of body
      dAve <- inf + Qad * (Cadipose / Kpad * BP) + Qbo * (Cbone / Kpbo * BP) + Qbr * (Cbrain / Kpbr * BP) + Qki* (Ckidney / Kpki * BP) + Qli * (Cliver / Kpli * BP) + Qmu* (Cmuscle / Kpmu * BP) + Qsk* (Cskin / Kpsk * BP)+ Qhe * (Che_ec / Kphe * BP) + Qre*(Crest / Kpre * BP) - Qlu * Cvenous - PER * SA_bloodcell * 0.001 * (Cvenous - Cbloodcell) #venous blood
      dAar <- Qlu * (Clung / Kplu * BP) - Qad * Carterial - Qbo * Carterial - Qbr * Carterial - Qgu * Carterial- Qki * Carterial- Qha * Carterial- Qmu * Carterial- Qsk * Carterial- Qsp * Carterial-	Qre * Carterial - Qhe * Carterial	#arterial blood
      dAbloodcell <- PER * SA_bloodcell * 0.001 * (Cvenous - Cbloodcell) + PER * SA_bloodcell * 0.001 * (Carterial - Cbloodcell)
      
      #return the rate of changes
      list(
        c(dINFUSION,
          dAad,
          dAbo,
          dAbr,
          dAgu,
          dAki,
          dAli,
          dAlu,
          dAmu,
          dAsk,
          dAsp,
          dAhe,
          dAve,
          dAar,
          dAre,
          
          dAhe_ec,
          dAad_ec,
          dAbo_ec,
          dAbr_ec,
          dAgu_ec,
          dAki_ec,
          dAli_ec,
          dAlu_ec,
          dAmu_ec,
          dAsk_ec,
          dAsp_ec,
          dAbloodcell,
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
        
        Cadipose = Aad / Vad,
        Cbone = Abo / Vbo	,
        Cbrain = Abr / Vbr,
        Cgut = Agu / Vgu,
        Ckidney = Aki / Vki,
        Cliver = Ali / Vli,
        Clung = Alu / Vlu,
        Cmuscle = Amu / Vmu,
        Cskin = Ask / Vsk,
        Cspleen = Asp / Vsp,
        Cheart = Ahe / Vhe,
        Crest = Are / Vre,
        logPL = log10(Cplasmavenous),
        PL = Cplasmavenous,
        BLCELL = Cbloodcell
      )
    })
  }
}

MyParms <- cbind (BP, BSA_male, CL_hepatic, CL_renal, CL_total,CO,fup,HCT_male,Height_male,inf_dose,inf_time,Kon_cardiolipin,Koff_cardiolipin,Kon_DNA,Koff_DNA,Kon_mtDNA,Koff_mtDNA,kon1,kon2,kon3,koff1,koff2,koff3,Kpad,Kpbo,Kpbr,Kpec_ad,Kpec_bo,Kpec_br,Kpec_gu,Kpec_he,Kpec_ki,Kpec_li,Kpec_lu,Kpec_mu,Kpec_pa,Kpec_sk,Kpec_sp,Kpgu,Kphe,Kpki,Kpli,Kplu,Kpmu,Kpp,Kppl,
Kpre,Kpsk,Kpsp,Kpu_ad,Kpu_bo,Kpu_br,Kpu_gu,Kpu_he,Kpu_ki,Kpu_li,Kpu_lu,Kpu_mu,Kpu_pa,Kpu_pl,Kpu_rbc,Kpu_sk,Kpu_sp,MSA,mtDNA_ad,mtDNA_bo,mtDNA_br,mtDNA_gu,mtDNA_he,mtDNA_ki,mtDNA_li,mtDNA_lu,mtDNA_mu,mtDNA_myo,mtDNA_other,mtDNA_sk,mtDNA_sp,MV,MVol,MW,N,PER,Qad,Qbo,Qbr,Qgu,Qha,Qhe,Qki,Qli,Qlu,Qmu,Qre,Qsk,Qsp,r,SA_ad,SA_bloodcell,SA_bo,SA_br,SA_gu,SA_ki,SA_li,SA_lu,
SA_mu,SA_myo,SA_other,SA_pa,SA_sk,SA_sp,Vad,Vad_ec,Vad_ic,Var,Vbl,Vbo,Vbo_ec,Vbo_ic,Vbr,Vbr_ec,Vbr_ic,Vgu,Vgu_ec,Vgu_ic,Vhe,Vhe_ec,Vki,Vki_ec,Vki_ic,Vli,Vli_ec,Vli_ic,Vlu,Vlu_ec,Vlu_ic,Vmu,Vmu_ec,Vmu_ic,Vmyo,Vmyo_ic,Vother,Vother_ic,
Vpl,Vplas_art,Vplas_ven,Vrb,Vre,Vre_ec,Vre_ic,Vsk,Vsk_ec,Vsk_ic,Vsp,Vsp_ec,Vsp_ic,Vve,Weight_male)
                  
#male
for (i in 1:N) 
{  
  outmale <- as.data.frame(lsoda(PBPKInitmale(inf_dose),times, PBPKModel, MyParms[i,]))
  names(outmale)<-list("time","Cadipose","Cbone","Cbrain","Cgut","Ckidney","Cliver","Cheart","Cmuscle","Cskin","Cspleen","Crest","logPL","PL","BLCELL")
  if (i>1)
  { simset<-rbind(simset,outmale)
  }
  else
  {
    simset<-outmale
  }
  print(paste("finished subject male",i))
}

#------------------------Plotting data ------------------------------------
# Summary of simulation results PLASMA (using function tapply)

# Set the margin and plot region size
par(mar = c(5, 4, 4, 4) + 0.1, oma = c(0, 0, 2, 0))

PL <- simset$PL

Summaryplasma<-data.frame( 
  Timerun=as.double(tapply(simset$time,paste(simset$time),median)),
  
  Median=as.double(tapply(PL,paste(simset$time),median)),
  
  Lower=as.double(tapply(PL,paste(simset$time),quantile,0.05)),
  
  Upper=as.double(tapply(PL,paste(simset$time),quantile,0.95)),
  
  Lowermin=as.double(tapply(PL,paste(simset$time),quantile,0.00)),
  
  Uppermax=as.double(tapply(PL,paste(simset$time),quantile,1.00))
)

#Order data
Summaryplasma <- Summaryplasma[order(Summaryplasma$Timerun,Summaryplasma$Median),]

#plot PLASMA
plot(times,Summaryplasma$Median,ylim=c(min(0.01),max(100)),log='y',xlab="Time (h)",ylab="umol/L", type="n", main="Plasma concentration")
lines(times,Summaryplasma$Upper,col="darkgrey", type="l",lwd=2)
lines(times,Summaryplasma$Lower,col="darkgrey", type="l",lwd=2)
polygon(c(times, rev(times)), c(Summaryplasma$Upper, rev(Summaryplasma$Lower)),
        col = "grey90", lwd=2, border = NA)
lines(times,Summaryplasma$Uppermax,lty=3,col="black",lwd=1)
lines(times,Summaryplasma$Lowermin,lty=3,col="black",lwd=1)
lines(times,Summaryplasma$Median,col="darkred",type="l",lwd=2)

# Summary of simulation results heart concentration (using function tapply)
Summaryhe<-data.frame( 
  Timerun=as.double(tapply(simset$time,paste(simset$time),median)),
  
  Median=as.double(tapply(simset$Cheart,paste(simset$time),median)),
  
  Lower=as.double(tapply(simset$Cheart,paste(simset$time),quantile,0.05)),
  
  Upper=as.double(tapply(simset$Cheart,paste(simset$time),quantile,0.95)),
  
  Lowermin=as.double(tapply(simset$Cheart,paste(simset$time),quantile,0.00)),
  
  Uppermax=as.double(tapply(simset$Cheart,paste(simset$time),quantile,1.00))
)

#Order data
Summaryhe<-Summaryhe[order(Summaryhe$Timerun,Summaryhe$Median),]

#plot heart concentration
plot(times,Summaryhe$Median,ylim=c(min(0.00001),max(1000)),log='y',xlab="Time (h)",ylab="umol/L", type="n", main="heart concentration")
lines(times,Summaryhe$Upper,col="darkgrey", type="l",lwd=2)
lines(times,Summaryhe$Lower,col="darkgrey", type="l",lwd=2)
polygon(c(times, rev(times)), c(Summaryhe$Upper, rev(Summaryhe$Lower)),
        col = "grey90", lwd=2, border = NA)
lines(times,Summaryhe$Uppermax,lty=3,col="black",lwd=1)
lines(times,Summaryhe$Lowermin,lty=3,col="black",lwd=1)
lines(times,Summaryhe$Median,col="darkred",type="l",lwd=2)


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                "#CC79A7")

plot2 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=MID_ict,col= "MID"),lty=1,size=1.5)+
  geom_line(aes(y=ENDO_ict,col="ENDO"),lty=1,size=1.5)+
  geom_line(aes(y=EPI_ict,col="EPI"),lty=1,size=1.5)+
  geom_line(aes(y=EC,col="EC"),lty=1,size=1.5)+
  ylab(" Concentration [umol/L]")+ xlab("Time (hour)")+
  scale_fill_manual( breaks = c("MID","ENDO","EPI","EC"),
                     values = c( "#E69F00", "#56B4E9","#009E73","#000000"),
                     labels = c("MID","ENDO","EPI","EC"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Total concentration of sub-compartment intracellular total comcentration", size=1)
plot2
ggplotly(plot2)

plot3 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=Cheart,col="Mean heart"),lty=1,size=1.5)+
  geom_line(aes(y=Cmyo_ict,col= "myocardial"),lty=1,size=1.5)+
  geom_line(aes(y=BL,col= "plasma"),lty=1,size=1.5)+
  ylab(" Concentration [umol/L]")+ xlab("Time (hour)")+
  scale_x_continuous(limits = c(0, 45))+
  scale_fill_manual( breaks = c("Mean heart","Midmyocardial","plasma"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Mean heart","Midmyocardial","plasma"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of sub-compartment layers", size=1)
plot3

plot4 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=Cendo_icfree,col="Cendo_icfree"),lty=1,size=1.5)+
  geom_line(aes(y=Cmid_icfree,col= "Cmid_icfree"),lty=1,size=1.5)+
  geom_line(aes(y=Cepi_icfree,col= "Cepi_icfree"),lty=1,size=1.5)+
  ylab(" Concentration [umol/L]")+ xlab("Time (hour)")+
  scale_x_continuous(limits = c(0, 48))+
  scale_fill_manual( breaks = c("Cendo_icfree","Cmid_icfree","Cepi_icfree"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Cendo_icfree","Cmid_icfree","Cepi_icfree"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of free DOX after DNA/cardiolipin/mtDNA", size=1)
plot4

plot5 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=Cadipose,col="Cadipose"),lty=1,size=1.5)+
  geom_line(aes(y=Cbone,col= "Cbone"),lty=1,size=1.5)+
  geom_line(aes(y=Cbrain,col= "Cbrain"),lty=1,size=1.5)+
  geom_line(aes(y=Cgut,col= "Cgut"),lty=1,size=1.5)+
  geom_line(aes(y=Ckidney,col= "Ckidney"),lty=1,size=1.5)+
  geom_line(aes(y=Cliver,col= "Cliver"),lty=1,size=1.5)+
  geom_line(aes(y=Clung,col= "Clung"),lty=1,size=1.5)+
  geom_line(aes(y=Cmuscle,col= "Cmuscle"),lty=1,size=1.5)+
  geom_line(aes(y=Cskin,col= "Cskin"),lty=1,size=1.5)+
  geom_line(aes(y=Cheart,col= "Cheart"),lty=1,size=1.5)+
  geom_line(aes(y=Cspleen,col= "Cspleen"),lty=1,size=1.5)+
  geom_line(aes(y=Crest,col= "Crest"),lty=1,size=1.5)+
  ylab(" Concentration [umol/L]")+ xlab("Time (hour)")+
  scale_x_continuous(limits = c(0, 3))+
  scale_fill_manual( breaks = c("Cadipose","Cbone","Cbrain","Cgut","Ckidney","Cliver","Clung","Cmuscle","Cskin","Cheart","Cspleen","Crest"),
                     values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7"),
                     labels = c("Cadipose","Cbone","Cbrain","Cgut","Ckidney","Cliver","Clung","Cmuscle","Cskin","Cheart","Cspleen","Crest"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of organs", size=1)
plot5
ggplotly(plot5)


#fumic (fumic is fraction of DOX unbound in an in vitro microsomal preparation)
fumic <- # [Venkatakrishnan 2001]
  
  # MPPGL(the microsomal protein density (MPPGL)) [mg/g] according to [Barter 2008] 
  MPPGL <- 10 ^ (1.407 + 0.0158 * age - 0.00038 * (age ^ 2) + 0.0000024 *(age ^ 3))
# the content of CYP1A2 and CYP3A4 in liver [pmol/mg protein]
CYP1A2_L <- 52 
CYP3A4_L <- 137 

#1) LIVER
#ISEF
#values from Simcyp. rCYP system: Lymph B
ISEF1A2 = 11.1
ISEF3A4 = 3.92


#Metabolism (hydroxylation)
#LIVER (L)
#Vmax for DOX after [pmol/min/pmol CYP]
#Km for DOX [mcM]

#1.CYP1A2
V_1A2 <- 1.79 * MW * 10 ^ -9 #[mg/min/pmol CYP]
K_1A2 <- 63.5 * MW * 10 ^ -3 #[mg/L]
CLint_1A2 <- (ISEF1A2 * (V_1A2 / (K_1A2 + Cliver)) * CYP1A2_L) / fumic  #[L/min/mg of microsomal protein]
#2.CYP3A4
V_3A4 <- 3.37 * MW * 10 ^ -9 #[mg/min/pmol CYP]#[Ghahramani 1997]
K_3A4 <- 213.8 * MW * 10 ^ -3 #[mg/L]
CLint_3A4 <- (ISEF3A4 * (V_3A4 / (K_3A4 + Cliver)) * CYP3A4_L) / fumic  #[L/min/mg of microsomal protein]

#sum of intrinsic clearances for demethylation for all CYPs isoforms
CLint_demethylation_L <- (CLint_1A2 + CLint_3A4)  * 60 #[L/h/mg of microsomal protein]

#Hepatic intrinsic clearance:
CLint_L <- (CLint_demethylation_L + CLint_hydroxylation_L) * MPPGL * Vli * liver_density #[L/h]


#HEART (H) CYP3A4 was not detected in heart tissue: assumed no metabolism of DOX in heart tissue
CYP3A4_H <- 0 #CYP 3A4 abundance in average human heart [pmol/mg tissue][Thum 2000]



write.xlsx(out, '~/Desktop/new_file.xlsx')
