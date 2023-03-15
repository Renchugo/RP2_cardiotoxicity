{
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
age <- 25
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
}

HCT <- 0.45 #hematocrit (Adult males: 41% to 50%)
fup <- 0.26 #fraction unbound in plasma (Huahe)
BP <- 1.15 #blood to plasma ratio [Dong 2022]

Kpu_rbc <- (BP + HCT - 1) / (HCT * fup)
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

Kpad <- Kpu_ad * fup
Kpbo <- Kpu_bo * fup
Kpbr <- Kpu_br * fup
Kpgu <- Kpu_gu * fup
Kphe <- Kpu_he * fup
Kpki <- Kpu_ki * fup
Kpli <- Kpu_li * fup
Kplu <- Kpu_lu * fup
Kpmu <- Kpu_mu * fup
Kppa <- Kpu_pa * fup
Kpsk <- Kpu_sk * fup
Kpsp <- Kpu_sp * fup
Kppl <- Kpu_pl * fup

Vss <- Vpl/ fup + Vad * Kpad + Vbo * Kpbo + Vbr * Kpbr + Vgu * Kpgu + Vhe * Kphe + Vki * Kpki  + Vli * Kpli + Vlu * Kplu + Vmu * Kpmu + Vpa * Kppa + Vsk * Kpsk + Vsp * Kpsp
  
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


# Define the original code as a character string
original_code <- '  
  EWbonemale=(64.179 - 1.2697 * age)*(9.8/43.9/100)
  IWbonemale=(64.179 - 1.2697 * age)*(34.1/43.9/100)
  NLbonemale= (0.2 + 0.3655 * age)*(7.4/7.51/100)
  NPbonemale= (0.2 + 0.3655 * age)*(0.11/7.51/100)
  
  EWgutmale= (75.378 - 0.3932 * log10(age))*(26.7/71.8/100)
  IWgutmale=  (75.378 - 0.3932 * log10(age))*(45.1/71.8/100)
  NLgutmale= (2.5 + 0.185 * age)*(4.87/6.5/100)
  NPgutmale= (2.5 + 0.185 * age)*(1.63/6.5/100)
  
  EWheartmale= (84.523 - 0.4249 * age)*(31.3/75.8/100)
  IWheartmale= (84.523 - 0.4249 * age)*(44.5/75.8/100)
  NLheartmale= (2.3159 + 0.0797 * age)*(1.15/2.81/100)
  NPheartmale= (2.3159 + 0.0797 * age)*(1.66/2.81/100)
  
  EWkidneymale= (83.278 - 0.2162 * age)*(28.3/78.3/100)
  IWkidneymale= (83.278 - 0.2162 * age)*(50/78.3/100)
  NLkidneymale= (2.73 + 1.995 * age /(2.59 + age))*(2.07/3.69/100)
  NPkidneymale= (2.73 + 1.995 * age /(2.59 + age))*(1.62/3.69/100)
  
  EWlivermale= (75.69 - 0.573 * log10(age))*(16.5/75.1/100)
  IWlivermale= (75.69 - 0.573 * log10(age))*(58.6/75.1/100)
  NLlivermale= (3 + 3.089 * age/(1.8 + age))*(3.48/6/100)
  NPlivermale= (3 + 3.089 * age/(1.8 + age))*(2.52/6/100)
  
  EWlungmale= (80.973 - 0.4916 * log10(age))*(34.8/81.1/100)
  IWlungmale= (80.973 - 0.4916 * log10(age))*(46.3/81.1/100)
  NLlungmale= (1.857 - 0.211 * log10(age))*(0.3/1.2/100)
  NPlungmale= (1.857 - 0.211 * log10(age))*(0.9/1.2/100)
  
  EWmusclemale= (77.211 - 0.4321 * log10(age))*(9.1/76/100)
  IWmusclemale= (77.211 - 0.4321 * log10(age))*(66.9/76/100)
  NLmusclemale= (1.9852 + 0.0649 * age)*(2.38/3.1/100)
  NPmusclemale= (1.9852 + 0.0649 * age)*(0.72/3.1/100)
  
  EWskinmale= (72.395 - 1.1462 * log10(age))*(62.3/71.77/100)
  IWskinmale= (72.395 - 1.1462 * log10(age))*(9.47/71.77/100)
  NLskinmale=3.95*(2.84/3.95/100)
  NPskinmale=3.95*(1.11/3.95/100)
  
  EWspleenmale= (79.952 - 0.4178 * log10(age))*(20.8/78.7/100)
  IWspleenmale= (79.952 - 0.4178 * log10(age))*(57.9/78.7/100)
  NLspleenmale=(1.5 + 0.015 * age)*(2.01/3.99/100)
  NPspleenmale=(1.5 + 0.015 * age)*(1.98/3.99/100)
  
  NLplasmamale= (0.5578 + 0.036 * log10(age))*(0.35/0.57/100)
  NPplasmamale= (0.5578 + 0.036 * log10(age))*(0.22/0.57/100)
  
  IWRBCmale=66/100
  NLRBCmale=0.3*(0.17/0.46/100)
  NPRBCmale=0.3*(0.29/0.46/100)'

# Define the character string you want to replace
to_replace <- 'male'

# Define the replacement character string
replacement <- ''

# Use gsub() to replace all occurrences of the character string in the original code
modified_code <- gsub(to_replace, replacement, original_code)

# Print the modified code
cat(modified_code)



