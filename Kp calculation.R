pKa <- 8.15 # (amine)
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

HCT <- 0.45 #hematocrit (Adult males: 41% to 50%)
fup <- 0.26 #fraction unbound in plasma (Huahe)
BP <- 1.15 #blood to plasma ratio [Dong 2022]

Kpu_rbc <- (BP * HCT + (1 - HCT)) / fup 
KaAP <- (Kpu_rbc - (X_rbc / X_ew * IW_rbc) - ((logPow * NL_rbc + (0.3 * logPow + 0.7) * NP_rbc) /X_ew)) * (X_ew /  (AP_rbc * Y_rbc))#affinity constant for acidic phospholipids (AP) 

# Rodger and Rowland method to calculate moderate to strong bases (pKa > 7) and ampholytes

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









