# Based on V 2.13.1
# The concentration ratio of myo and other (PER for other is set to 1000 lower than myo)
# The blood cell intracellular binding
# Kd only has a little negligible effect
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
  require(pksensi)
  require(sensitivity)
}

{
  t_end <- 200 #[h] time of the end of simulation
  times <- seq(0, t_end, by = 1) #time of simulation
  
  pKa <- 8.46 # [amine]
  MW <- 543.52 # g/mol
  
  #Physiological parameters of patients-------------------------------------------------------------------
  age <- 50 #the age of chosen sample
  gender <- 'male'
  weight <- 70 #[kg]
  height <- 180 #[cm]
  BSA <- weight ^ 0.425 * height ^ 0.725 * 0.007184 #[m2] Body surface area according to [DuBois-DuBois 1916]
  
  oral_dose <- 0 #[mg] oral bolus dose
  inf_dose_mg <- 36 * BSA #【mg】
  
  inf_dose <- (inf_dose_mg * 0.001 / MW ) * 1000000 # [umol]
  inf_time <- 1/60 #[h] infusion time
  
  CO  <- 1.1 * BSA - 0.05 * age + 5.5 #[L/min] cardiac output from Tylukia
  CO <- CO * 60 #[L/h] cardiac output units change from [L/min] to [L/h]
  CO <- BSA * (110 + (184.974 * (exp(-0.0378 * age) -exp(-0.2477 * age)))) #[journal.pcbi.1008786]

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
  
  # ORGAN VOLUMES [L] -----------------------------------------------------------
  #BW fraction (according to Simcyp Simulator) * random BW / tissue density
  Vad <- (0.259 * weight) / 0.923
  Vbo <- (0.090 * weight) / 1.850
  Vbr <- (0.017 * weight) / 1.04
  Vgu <- (0.016 * weight) / 1.04
  Vhe <- (0.005 * weight) / 1.04
  Vki <- (0.004 * weight) / 1.05
  Vli <- (0.022 * weight) / 1.08
  Vlu <- (0.007 * weight) / 1.05
  Vmu <- (0.403 * weight) / 1.04
  Vsk <- (0.043 * weight) / 1.1
  Vsp <- (0.002 * weight) / 1.06
  Vre <- (0.057 * weight) / 1.05
  Vpl <- (0.044 * weight) / 1.025
  Vrb <- (0.031 * weight) / 1.125
  Vbl <- Vpl + Vrb
  
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
  
  #the concentration of DNA derive from [Daniel 2002]
  DNA_li <- 23.7  #umol/L 
  DNA_he <- 8.3  #umol/L 
  DNA_ki <- 16.2  #umol/L 
  DNA_bo <- 19.1  #umol/L 
  DNA_gu <- 25.2  #umol/L 
  
  DNA_mu <- 4.5  #umol/L Slowly perfused organs
  DNA_ad <- 4.5  #umol/L 
  DNA_sk <- 4.5  #umol/L 
  DNA_sp <- 4.5  #umol/L 
  
  DNA_br <- 15 #umol/L Rapidly perfused organ
  DNA_lu <- 15  #umol/L 
  
  DNA_bloodcell <- 0.01 #µmol/L assumption
  
  #the concentration of DNA derive from [Huahe]
  #DNA_lu <- 23500 #umol/L 
  #DNA_li <- 6430
  #DNA_gu <- 20500
  #DNA_sp <- 54100
  #DNA_ki <- 12500
  #DNA_he <- 45900
  
  Cardiolipin_li <- 44.6  #umol/L 
  Cardiolipin_he <- 43.8  #umol/L 
  Cardiolipin_ki <- 52.3  #umol/L 
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
    mtDNA_li <- (li_cell * (cn_li * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vli
    mtDNA_he <- (he_cell * (cn_he * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vhe  
    mtDNA_ki <- (ki_cell * (cn_ki * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vki  
    mtDNA_bo <- (bo_cell * (cn_bo * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vbo
    mtDNA_gu <- (gu_cell * (cn_gu * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vgu 
    
    mtDNA_mu <- (mu_cell * (cn_mu * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vmu
    mtDNA_ad <- (ad_cell * (cn_ad * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vad 
    mtDNA_sk <- (sk_cell * (cn_sk * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vsk 
    
    mtDNA_br <- (br_cell * (cn_br * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vbr 
    mtDNA_lu <- (lu_cell * (cn_lu * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vlu 
    mtDNA_sp <- (sp_cell * (cn_sp * 660 * 16535/ (6.022 * 10^14)) * (1.52 * 10^-9) ) / Vsp 
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
  
  SA_bo = 381752.22 #[cm2] [Simcyp]
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
  
  SA_bloodcell <- 2580000 #[cm2] [Huahe paper]
  #SA_lu <- 1560000
  #SA_li <- 2850000
  #SA_gu <- 3250000
  #SA_sp <- 275000
  #SA_ki <- 519000
  #SA_he <- 603000
  
  # PARAMETERS FOR ICF and ECF in heart tissue -------------------------------------------------------
  pH_ic <- 7.2 #[Vaugha-Jones 2009, Zheng 2005]
  pH_ec <- 7.4 #[Vaugha-Jones 2009, Zheng 2005]
  
  #Henderson_Hasselbalch equation for base compound -> fraction of un-ionized base in heart compartments: -------------------------------------------------------
  funionized_ic <- 1 / (1 + 10 ^ (pKa - pH_ic))
  funionized_ec <- 1 / (1 + 10 ^ (pKa - pH_ec))
  
  Kpp <- (1 + 10 ^ (pKa - pH_ic)) / (1 + 10 ^ (pKa - pH_ec)) 
  #Kp_lu <- 3.38
  #Kp_gu <- 1.26
  #Kp_sp <- 5.74
  #Kp_li <- 10.9
  #Kp_ki <- 5.91
  #Kp_he <- 5.63
  #Kp_blood <- 4.07
  #Kp_re <- 13.3
  
  #IV INFUSION RATE
  r = inf_dose #[mg]
  t = inf_time #time of infusion [h]
  inf = r / t #infusion rate [mg/h]
  
  #DISTRIBUTION  -------------------------------------------------------
  #Drug binding
  fup <- 0.26 #fraction unbound in plasma (Huahe)
  
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
    
    #Hematocrit
    #MALE
    HCT <- (53 - ((43.0 * age^1.12 / (0.05^1.12 + age^1.12)) * (1 + (-0.93 * age^0.25 / (0.10^0.25 + age^0.25)))))/100
    
    #Blood-plasma ratio
    # BP=1-Ht + EP*Ht 
    BPmale <- 1- HCT + 1.34 * HCT  #blood plasma ratio
    
    fup <- 0.26 #fraction unbound in plasma (Huahe)
    BP <- 1.15 #blood to plasma ratio [Dong 2022]
    
    Kpu_rbc <- (BP + HCT - 1) / (HCT * fup)
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
    Kpu_rbc <- EW_rbc + (X_iw / X_ew) * IW_rbc + ( P * NL_rbc + (0.3 * P + 0.7) * NP_rbc) / EW_rbc + KaAP * AP_rbc * Y_iw / X_ew
    
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
      dChe_ec <- (Qhe * (Car - (Che_ec/Kphe) * BP) -(0.001 * PER * SA_myo * (Che_ec * fuec_he * funionized_ec - Cmyo_ict/(Kpec_he * Kpp) * funionized_ic)) - (0.001 * 0.0000756 *  SA_other * (Che_ec * fuec_he * funionized_ec - Cother_ict /(Kpec_he * Kpp) * funionized_ic)) ) / Vhe_ec
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
      dCmyo_ict <- (0.001 * PER * SA_myo * ((Che_ec * fuec_he * funionized_ec - Cmyo_ict/(Kpec_he * Kpp) * funionized_ic ) - (Cmyo_ict * fu_heart * CLint_heart * (Vmyo_ic/Vhe))) / Vmyo_ic) - (kon1 * Cmyo_ict * Cmyo_E1 - koff1 * Cmyo_E1R) - (kon2 * Cmyo_ict * Cmyo_E2 - koff2 * Cmyo_E2R ) - (kon3 * Cmyo_ict * Cmyo_E3 - koff3 * Cmyo_E3R )
      dCother_ict <- (0.001 * 0.0000756 * SA_other * ((Che_ec * fuec_he * funionized_ec - Cother_ict/(Kpec_he * Kpp) * funionized_ic) - (Cother_ict * fu_heart * CLint_heart * (Vother_ic/Vhe)))/Vother_ic) - (kon1 * Cother_ict * Cother_E1 - koff1 * Cother_E1R) - (kon2 * Cother_ict * Cother_E2 - koff2 * Cother_E2R ) - (kon3 * Cother_ict * Cother_E3 - koff3 * Cother_E3R )
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
}

out <-
  ode(y = state,
      times = times,
      func = PBPKModel,
      parm = parameters,
      hmax= 0.01,
      rtol = 1e-8,
      atol = 1e-8
  )

results <- data.frame(out)

plot(
  results$time,
  results$BLCELL,
  type = "l",
  col = "blue",
  xlab = "Time [h]",
  ylab = "Blood cell Concentration [umol/L]"
  #log = "y", # set y-axis to log scale
  #yticks = c(10000, 1000, 100, 10, 1, 0.1, 0.01, 0.001) # set y-axis tick marks
) 

plot(
  results$time,
  results$PL,
  type = "l",
  col = "blue",
  xlab = "Time [h]",
  ylab = "Plasma Concentration [umol/L]",
  log = "y", # set y-axis to log scale
  yticks = c(10000, 1000, 100, 10, 1, 0.1, 0.01, 0.001) # set y-axis tick marks
) 

{
# Validation data point
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
points(c(0.083,0.167,0.25,0.5,1,0.95,1.166,2.149,8.054,24.292,48.563,71.766,96.037,119.79,144.299,168.528,216.022),c(2.728,2.00,1.066,0.552,0.149,0.09,0.061,0.0389,0.0279,0.02,0.018,0.012,0.0104,0.009,0.007,0.004,0.003), pch=16, col= "black")

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
}

plot5 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=Cad_ec,col="Cadipose"),lty=1,size=1.5)+
  geom_line(aes(y=Cbo_ec,col= "Cbone"),lty=1,size=1.5)+
  geom_line(aes(y=Cbr_ec,col= "Cbrain"),lty=1,size=1.5)+
  geom_line(aes(y=Cgu_ec,col= "Cgut"),lty=1,size=1.5)+
  geom_line(aes(y=Cki_ec,col= "Ckidney"),lty=1,size=1.5)+
  geom_line(aes(y=Cli_ec,col= "Cliver"),lty=1,size=1.5)+
  geom_line(aes(y=Clu_ec,col= "Clung"),lty=1,size=1.5)+
  geom_line(aes(y=Cmu_ec,col= "Cmuscle"),lty=1,size=1.5)+
  geom_line(aes(y=Csk_ec,col= "Cskin"),lty=1,size=1.5)+
  geom_line(aes(y=Che_ec,col= "Cheart"),lty=1,size=1.5)+
  geom_line(aes(y=Csp_ec,col= "Cspleen"),lty=1,size=1.5)+
  geom_line(aes(y=Cre,col= "Crest"),lty=1,size=1.5)+
  ylab(" Concentration [umol/L]")+ xlab("Time (hour)")+
  scale_x_continuous(limits = c(0, 10))+
  scale_fill_manual( breaks = c("Cadipose","Cbone","Cbrain","Cgut","Ckidney","Cliver","Clung","Cmuscle","Cskin","Cheart","Cspleen","Crest"),
                     values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7"),
                     labels = c("Cadipose","Cbone","Cbrain","Cgut","Ckidney","Cliver","Clung","Cmuscle","Cskin","Cheart","Cspleen","Crest"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of organs", size=1)
plot5
ggplotly(plot5)

{
#Evaluate the effect of Kd/ Koff (negligible effect)
#Expand Kd to 10 times larger (keep koff the same and lower Kon)
params<-as.list(parameters)
changedpars1<-within(params, {
  Kd_DNA <- 3.23 # 3.23 umol/L = 0.00000323 mol/L = 0.00323 mM = 3.23 x 10^-6 M = 3230 nmol/L
  Koff_DNA <- 3056.4 #509.4 min-1 = 30564 h-1 
  Kon_DNA <- Koff_DNA / Kd_DNA 
  
  Kd_mtDNA <- 1 # 100nM to Molar (assume)
  Koff_mtDNA <- 3056.4 #509.4 min-1 = 30564 h-1
  Kon_mtDNA <- Koff_mtDNA / Kd_mtDNA
  
  Kd_cardiolipin <- 4 # 400nM to Molar (assume)
  Koff_cardiolipin <- 3056.4 #509.4 min-1 = 30564 h-1
  Kon_cardiolipin <- Koff_cardiolipin / Kd_cardiolipin
})  

# Kd to 10 times smaller (keep koff the same and increase Kon)
changedpars2<-within(params, {
  Kd_DNA <- 3.23 # 3.23 umol/L = 0.00000323 mol/L = 0.00323 mM = 3.23 x 10^-6 M = 3230 nmol/L
  Koff_DNA <- 305640 #509.4 min-1 = 30564 h-1 
  Kon_DNA <- Koff_DNA / Kd_DNA 
  
  Kd_mtDNA <- 1 # 100nM to Molar (assume)
  Koff_mtDNA <- 305640 #509.4 min-1 = 30564 h-1
  Kon_mtDNA <- Koff_mtDNA / Kd_mtDNA
  
  Kd_cardiolipin <- 4 # 400nM to Molar (assume)
  Koff_cardiolipin <- 305640 #509.4 min-1 = 30564 h-1
  Kon_cardiolipin <- Koff_cardiolipin / Kd_cardiolipin
})    

# executing the additional simulations
doutder1<-NULL
doutder1<-data.frame(ode(y = state,
                           times = times,
                           func = PBPKModel,
                           parm = params,
                           hmax= 0.01,
                           rtol = 1e-8,
                           atol = 1e-8))
doutder2<-NULL
doutder2<-data.frame(ode(y = state,
                         times = times,
                         func = PBPKModel,
                         parm = changedpars1,
                         hmax= 0.01,
                         rtol = 1e-8,
                         atol = 1e-8))

doutder3<-NULL
doutder3<-data.frame(ode(y = state,
                         times = times,
                         func = PBPKModel,
                         parm = changedpars2,
                         hmax= 0.01,
                         rtol = 1e-8,
                         atol = 1e-8))
PL_kd <- data.frame(time=doutder1$time,Kd=doutder1$PL, Kd_10=doutder2$PL, Kd_0.1=doutder2$PL)
Heartec_kd <- data.frame(time=doutder1$time,Kd=doutder1$Che_ec, Kd_10=doutder2$Che_ec, Kd_0.1=doutder2$Che_ec)
Myoict_kd <- data.frame(time=doutder1$time,Kd=doutder1$Cmyo_ict, Kd_10=doutder2$Cmyo_ict, Kd_0.1=doutder2$Cmyo_ict)

plot_PL_kd <-ggplot(data=data.frame(PL_kd), aes(x=time))+
    geom_line(aes(y=Kd,col="Kd"),lty=1,size=1.5)+
    geom_line(aes(y=Kd_10,col="Kd_10"),lty=1,size=1.5)+
    geom_line(aes(y=Kd_0.1,col="Kd_0.1"),lty=1,size=1.5)+
    ylab(" Plasma concentration")+ xlab("Time (hours)")+
    scale_fill_manual( breaks = c("Kd","Kd_10","Kd_0.1"),
                       values = c("#000000", "#E69F00", "#56B4E9"),
                       labels = c("Kd","Kd_10","Kd_0.1"))+
    theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
    labs(title="The effect of Kd on plasma concentration", size=1)
plot_PL_kd

plot_Heartec_kd <-ggplot(data=data.frame(Heartec_kd), aes(x=time))+
  geom_line(aes(y=Kd,col="Kd"),lty=1,size=1.5)+
  geom_line(aes(y=Kd_10,col="Kd_10"),lty=1,size=1.5)+
  geom_line(aes(y=Kd_0.1,col="Kd_0.1"),lty=1,size=1.5)+
  ylab(" Heart ec concentration")+ xlab("Time (hours)")+
  scale_fill_manual( breaks = c("Kd","Kd_10","Kd_0.1"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Kd","Kd_10","Kd_0.1"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="The effect of Kd on Heart ec concentration", size=1)
plot_Heartec_kd
  
plot_Myoict_kd <-ggplot(data=data.frame(Myoict_kd), aes(x=time))+
  geom_line(aes(y=Kd,col="Kd"),lty=1,size=1.5)+
  geom_line(aes(y=Kd_10,col="Kd_10"),lty=1,size=1.5)+
  geom_line(aes(y=Kd_0.1,col="Kd_0.1"),lty=1,size=1.5)+
  ylab(" Myocardial concentration")+ xlab("Time (hours)")+
  scale_fill_manual( breaks = c("Kd","Kd_10","Kd_0.1"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Kd","Kd_10","Kd_0.1"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="The effect of Kd on Myocardial concentration", size=1)
plot_Myoict_kd
  

#Evaluate the effect of the concentration
#Expand concentration to 10 times larger
params<-as.list(parameters)
changedpars3<-within(params, {
  DNA_li <- DNA_li * 10
  DNA_he <- DNA_he * 10
  DNA_ki <- DNA_ki * 10
  DNA_bo <- DNA_bo * 10
  DNA_gu <- DNA_gu * 10
  DNA_mu <- DNA_mu * 10
  DNA_ad <- DNA_ad * 10
  DNA_sk <- DNA_sk * 10
  DNA_sp <- DNA_sp * 10
  DNA_br <- DNA_br * 10
  DNA_lu <- DNA_lu * 10
  DNA_bloodcell <- DNA_bloodcell * 10
  
  mtDNA_li <- mtDNA_li * 10
  mtDNA_he <- mtDNA_he * 10
  mtDNA_ki <- mtDNA_ki * 10
  mtDNA_bo <- mtDNA_bo * 10
  mtDNA_gu <- mtDNA_gu * 10
  mtDNA_mu <- mtDNA_mu * 10
  mtDNA_ad <- mtDNA_ad * 10
  mtDNA_sk <- mtDNA_sk * 10
  mtDNA_sp <- mtDNA_sp * 10
  mtDNA_br <- mtDNA_br * 10
  mtDNA_lu <- mtDNA_lu * 10
  
  Cardiolipin_li <- Cardiolipin_li * 10
  Cardiolipin_he <- Cardiolipin_he * 10
  Cardiolipin_ki <- Cardiolipin_ki * 10
  Cardiolipin_bo <- Cardiolipin_bo * 10
  Cardiolipin_gu <- Cardiolipin_gu * 10
  Cardiolipin_mu <- Cardiolipin_mu * 10
  Cardiolipin_ad <- Cardiolipin_ad * 10
  Cardiolipin_sk <- Cardiolipin_sk * 10
  Cardiolipin_sp <- Cardiolipin_sp * 10
  Cardiolipin_br <- Cardiolipin_br * 10
  Cardiolipin_lu <- Cardiolipin_lu * 10
})  

changedpars4<-within(params, {
  DNA_li <- DNA_li * 0.1
  DNA_he <- DNA_he * 0.1
  DNA_ki <- DNA_ki * 0.1
  DNA_bo <- DNA_bo * 0.1
  DNA_gu <- DNA_gu * 0.1
  DNA_mu <- DNA_mu * 0.1
  DNA_ad <- DNA_ad * 0.1
  DNA_sk <- DNA_sk * 0.1
  DNA_sp <- DNA_sp * 0.1
  DNA_br <- DNA_br * 0.1
  DNA_lu <- DNA_lu * 0.1
  DNA_bloodcell <- DNA_bloodcell * 0.1
  
  mtDNA_li <- mtDNA_li * 0.1
  mtDNA_he <- mtDNA_he * 0.1
  mtDNA_ki <- mtDNA_ki * 0.1
  mtDNA_bo <- mtDNA_bo * 0.1
  mtDNA_gu <- mtDNA_gu * 0.1
  mtDNA_mu <- mtDNA_mu * 0.1
  mtDNA_ad <- mtDNA_ad * 0.1
  mtDNA_sk <- mtDNA_sk * 0.1
  mtDNA_sp <- mtDNA_sp * 0.1
  mtDNA_br <- mtDNA_br * 0.1
  mtDNA_lu <- mtDNA_lu * 0.1
  
  Cardiolipin_li <- Cardiolipin_li * 0.1
  Cardiolipin_he <- Cardiolipin_he * 0.1
  Cardiolipin_ki <- Cardiolipin_ki * 0.1
  Cardiolipin_bo <- Cardiolipin_bo * 0.1
  Cardiolipin_gu <- Cardiolipin_gu * 0.1
  Cardiolipin_mu <- Cardiolipin_mu * 0.1
  Cardiolipin_ad <- Cardiolipin_ad * 0.1
  Cardiolipin_sk <- Cardiolipin_sk * 0.1
  Cardiolipin_sp <- Cardiolipin_sp * 0.1
  Cardiolipin_br <- Cardiolipin_br * 0.1
  Cardiolipin_lu <- Cardiolipin_lu * 0.1
})  

# executing the additional simulations
doutder4<-NULL
doutder4<-data.frame(ode(y = state,
                         times = times,
                         func = PBPKModel,
                         parm = params,
                         hmax= 0.01,
                         rtol = 1e-8,
                         atol = 1e-8))
doutder5<-NULL
doutder5<-data.frame(ode(y = state,
                         times = times,
                         func = PBPKModel,
                         parm = changedpars3,
                         hmax= 0.01,
                         rtol = 1e-8,
                         atol = 1e-8))

doutder6<-NULL
doutder6<-data.frame(ode(y = state,
                         times = times,
                         func = PBPKModel,
                         parm = changedpars4,
                         hmax= 0.01,
                         rtol = 1e-8,
                         atol = 1e-8))

PL_concentration <- data.frame(time=doutder4$time,C=doutder4$PL, C_10=doutder5$PL, C_0.1=doutder6$PL)
Heartec_concentration <- data.frame(time=doutder4$time,C=doutder4$Che_ec, C_10=doutder5$Che_ec, C_0.1=doutder6$Che_ec)
Myoict_concentration <- data.frame(time=doutder4$time,C=doutder4$Cmyo_ict, C_10=doutder5$Cmyo_ict, C_0.1=doutder6$Cmyo_ict)

plot_PL_concentration <-ggplot(data=data.frame(PL_concentration), aes(x=time))+
  geom_line(aes(y=C,col="Concentration"),lty=1,size=1.5)+
  geom_line(aes(y=C_10,col="Concentration_10"),lty=1,size=1.5)+
  geom_line(aes(y=C_0.1,col="Concentration_0.1"),lty=1,size=1.5)+
  ylab(" Plasma concentration")+ xlab("Time (hours)")+
  scale_fill_manual( breaks = c("Concentration","Concentration_10","Concentration_0.1"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Concentration","Concentration_10","Concentration_0.1"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="The effect of DNA/ mtDNA/ Cardiolipin concentration on plasma concentration", size=1)
plot_PL_concentration

plot_Heartec_concentration <-ggplot(data=data.frame(Heartec_concentration), aes(x=time))+
  geom_line(aes(y=C,col="Concentration"),lty=1,size=1.5)+
  geom_line(aes(y=C_10,col="Concentration_10"),lty=1,size=1.5)+
  geom_line(aes(y=C_0.1,col="Concentration_0.1"),lty=1,size=1.5)+
  ylab(" Heart ec concentration")+ xlab("Time (hours)")+
  scale_fill_manual( breaks = c("Concentration","Concentration_10","Concentration_0.1"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Concentration","Concentration_10","Concentration_0.1"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="The effect of DNA/ mtDNA/ Cardiolipin concentration on Heart ec concentration", size=1)
plot_Heartec_concentration

plot_Myoict_concentration <-ggplot(data=data.frame(Myoict_concentration), aes(x=time))+
  geom_line(aes(y=C,col="Concentration"),lty=1,size=1.5)+
  geom_line(aes(y=C_10,col="Concentration_10"),lty=1,size=1.5)+
  geom_line(aes(y=C_0.1,col="Concentration_0.1"),lty=1,size=1.5)+
  ylab(" Myo ICT concentration")+ xlab("Time (hours)")+
  scale_fill_manual( breaks = c("Concentration","Concentration_10","Concentration_0.1"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Concentration","Concentration_10","Concentration_0.1"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="The effect of DNA/ mtDNA/ Cardiolipin concentration on Heart ec concentration", size=1)
plot_Myoict_concentration
ggplotly(plot_Myoict_concentration)
}

# 4.3 Application of pksensi in sensitivity analysis
# 1. Set parameter distribution (assign parms, dist, q.qarg)
parameters <- c(
  "BP",
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

q <- "qunif"
q.arg <- list(list(0.115, 11.5),
              list(0.026, 2.6),
              list(0.237, 23.7),
              list(0.083, 8.3),
              list(0.162, 16.2),
              list(0.191, 19.1),
              list(0.252, 25.2),
              list(0.045, 4.5),
              list(0.045, 4.5),
              list(0.045, 4.5),
              list(0.045, 4.5),
              list(0.015, 1.5),
              list(0.015, 1.5),
              list(0.00006119827,0.006119827),
              list(0.0000152633,0.00152633),
              list(6.00148e-06,6.00148e-04),
              list(7.65197e-08,7.65197e-06),
              list(2.460056e-07,2.460056e-05),
              list(1.326618e-07,1.326618e-05),
              list(9.52288e-07,9.52288e-05),
              list(0.00001972968,0.001972968),
              list(0.0000683203,0.00683203),
              list(0.00002139697,0.002139697),
              list(0.446,44.6),
              list(0.438,43.8),
              list(0.523,52.3),
              list(0.25,25),
              list(0.25,25),
              list(0.15,15),
              list(0.15,15),
              list(0.15,15),
              list(0.15,15),
              list(0.3,30),
              list(0.3,30),
              list(0.3,30),
              list(764.1,76410),
              list(3056.4,305640),
              list(946.2539,94625.39),
              list(3056.4,305640),
              list(3056.4,305640),
              list(3056.4,305640),
              list(3056.4,305640),
              list(0.066,6.6),
              list(2.997,299.7))









