# Whole-body PBPK model for Doxorubicin 
#DNA content: 2.15 ug
rm(list=ls(all=TRUE))

#REQUIRED PACKAGES:
require(distr)
require(data.table)
require(dplyr)
require(ggplot2)
require(deSolve)
require(plotly)
{
  t_end <- 45 #[h] time of the end of simulation
  times <- seq(0, t_end, by = 0.1) #time of simulation
  
  age <- 25 #the age of chosen sample
  gender <- 'male'
  weight <- 70 #[kg]
  height <- 180 #[cm]
  
  BSA <- weight ^ 0.425 * height ^ 0.725 * 0.007184 #[m2] Body surface area according to [DuBois-DuBois 1916]
  oral_dose <- 0 #[mg] oral bolus dose
  inf_dose <- 50 * BSA #[mg] infusion dose according to Pfizer guidance : https://www.pfizermedicalinformation.com/en-us/doxorubicin/dosage-admin
  inf_time <- 2 #[h] infusion time
  
  CO  <- 1.1 * BSA - 0.05 * age + 5.5 #[L/min] cardiac output
  CO <- CO * 60 #[L/h] cardiac output units change from [L/min] to [L/h]
  
  #SEX DEPENDENT BLOOD FLOWS for healthy population according to [Simycp Simulator v.16]
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
  Qlu <- CO#lung
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
  
  # CARDIOMYOCYTE VOLUME AND SURFACE AREA according to [Polak 2012] -------------------------------------------------------
  MV <- exp(age * 0.04551 + 7.36346) #[cm^3]
  MSA <-exp(sqrt(0.102 ^ 2 + (log(MV)) ^ 2 * 0.002939 ^ 2)) #[cm^2]
}

{
  # MODEL -------------------------------------------------------------------
  #Arguments of the function are the model parameters that vary
  ModelVar <- function (BW,
                        CO,
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
  
  # PHYSICO-CHEMICAL PARAMETERS OF DOX -------------------------------------------------------
  MW <-  543.52 #[g/mol] Molecular weight
  pKa <- 8.22 # [Zahra 2020]
  HBD <- 6 #number of hydrogen bond donors: [PubChem Compound Database; CID = 31703];
  PSA <- 206 #Polar Surface Area:  [PubChem Compound Database; CID = 31703]
  logD74 <- 0.02 #octanol/water distribution coefficient at pH 7.4 [Alves 2017]
  
  # PHYSIOLOGICAL PARAMETERS -------------------------------------------------------
  liver_density <- 1080 #[g/L]
  heart_density <- 1055 #[g/L] [Alexandra 2019]
  
  #Tissue volumes [L] -------------------------------------------------------
  Vendo = 0.5 * Vhe #endocardial
  Vmid = 0.3 * Vhe #midmyocardial
  Vepi = 0.2 * Vhe #epicardial
  Vendo_ic = 87.5 / 100 * Vendo #intracellular space of endocardial [Sjogaard 1982] -> data for skeletal muscle; calculated the proportion of extracellular water to total water in skeletal muscle
  Vepi_ic = 87.5 / 100 * Vepi #intracellular space of epicardial
  Vmid_ic = 87.5 / 100 * Vmid #intracellular space of midmyocardial
  Vhe_ec = 12.5 / 100 * Vhe #extracellular space of heart tissue
  Vve = (2 / 3) * Vbl		#venous blood; assumed 2/3 of total blood according to volmues published in CPT. Regarding the distribution of blood volume within the circulation, the greatest volume resides in the venous vasculature, where 70-80% of the blood volume is found. -> http://www.cvphysiology.com/Blood%20Pressure/BP019
  Var = Vbl - Vve		#arterial blood
  Vplas_ven = Vpl * (Vve / (Vve + Var))  #venous plasma
  Vplas_art = Vpl * (Var / (Vve + Var)) 	#arterial plasma
  
  #myocyte volume -------------------------------------------------------
  ML <- 134 #[mcm] myocyte length [Tracy 2011, Gerdes 1995]
  MB <- ML / 7 #=2r [mcm] myocyte breadth ML:MB = 7:1 [Tracy 2011, Gerdes 1995]
  MVol <- MV * (10 ^ -15) #[L] random age dependent myocate volume in cm3 -> changing to liters
  
  #cells amounts -------------------------------------------------------
  cell_amount_epi <- Vepi_ic / MVol #epicardial
  cell_amount_mid <- Vmid_ic / MVol #midmyocardial
  cell_amount_endo <- Vendo_ic / MVol #endocardial
  
  #cells concentration -------------------------------------------------------
  cell_concentration_epi <- cell_amount_epi / Vhe #epicardial
  cell_concentration_mid <- cell_amount_mid / Vhe #midmyocardial
  cell_concentration_endo <- cell_amount_endo / Vhe#endocardial
  
  #heart DNA: 90.6 ng/ul = 90.6 mg/l from literature -------------------------------------------------------
  DNA_epi <- 2.15 * 0.001 / Vhe
  DNA_mid <- 2.15 * 0.001 / Vhe
  DNA_endo <- 2.15 * 0.001 / Vhe
  
  mtDNA_epi <- 2.15 * 0.001 * 0.01 / Vhe
  mtDNA_mid <- 2.15 * 0.001 * 0.01 / Vhe
  mtDNA_endo <- 2.15 * 0.001 * 0.01 / Vhe
  
  #DNA concentration (6pg = 6 * 10^-9 mg DNA per cell)  # DNA and mtDNA concentration, assume epi:mid:endo = 0.2 : 0.3 : 0.5 [DOI:10.6000/1927-5129.2017.13.35]
  #DNA_epi <- cell_concentration_epi * 6 * 10 ^ -9
  #DNA_mid <- cell_concentration_mid * 6 * 10 ^ -9
  #DNA_endo <- cell_concentration_endo * 6 * 10 ^ -9
  
  #mtDNA_epi <- DNA_epi * 0.01
  #mtDNA_mid <- DNA_mid * 0.01
  #mtDNA_endo <- DNA_endo * 0.01 
  
  #PER：cytoplasmic membrane permeability coefficient
  PER <- 0.0756  # cm/h （huahe Unpublished）
  
  #surface area -------------------------------------------------------
  SA_epi <- (cell_amount_epi * MSA) / (10 ^ 8) #[cm^2]
  SA_mid <- (cell_amount_mid * MSA) / (10 ^ 8) #[cm^2]
  SA_endo <- (cell_amount_endo * MSA) / (10 ^ 8) #[cm^2]
  SA_bloodcell <- 25800 #[dm2] [Huahe paper]
  
  # PARAMETERS FOR ICF and ECF in heart tissue -------------------------------------------------------
  pH_ic <- 7.2 #[Vaugha-Jones 2009, Zheng 2005]
  pH_ec <- 7.4 #[Vaugha-Jones 2009, Zheng 2005]
  
  #Henderson_Hasselbalch equation for base compound -> fraction of un-ionized base in heart compartments: -------------------------------------------------------
  funionized_ic <- 1 / (1 + 10 ^ (pKa - pH_ic))
  funionized_ec <- 1 / (1 + 10 ^ (pKa - pH_ec))
  
  Kpp <- (1 + 10 ^ (pKa - pH_ic)) / (1 + 10 ^ (pKa - pH_ec)) 
  
  #IV INFUSION RATE
  r = inf_dose #[mg]
  t = inf_time #time of infusion [h]
  inf = r / t #infusion rate [mg/h]
  
  #Absorption: -------------------------------------------------------
  PAMPA <- 1 / 3600 #[cm/s] [Eikenberry 2009]
  Pdiff_dox <- PAMPA #[cm/s]
  
  #DISTRIBUTION  -------------------------------------------------------
  #Drug binding
  fup <- 0.26 #fraction unbound in plasma (Huahe)
  
  fu_ec <- 1 #fraction unbound in extracellular fluid is assumed
  fu_heart <- 1 #fraction unbound in heart is assumed
  fuha <- 0.1230352 #fraction unbound in hepatic arterial
  
  # TISSUE TO PLASMA PARTITION COEFFICIENT -------------------------------------------------------
  # E/P values gets from Simcyp Simulator (ratio); Equations to calculate Kpec for the heart
  # other Kpvalues using KP values with KP scalar = 4; Vss = 26.248 L/kg; Compound type = Ampholyte; KP heart = 14.713 (KP scalar = 1)
  ratio <- 0.157
  Kpec <-  (1- ratio) * fup + ratio
  Kpad <- 7.7431
  Kpbo <- 13.553
  Kpbr <- 7.504
  Kpgu <- 54.531
  Kpki <- 37.792
  Kpli <- 96.797
  Kplu <- 18.689
  Kpmu <- 48.424
  Kpsk <- 25.527
  Kpsp <- 54.269
  Kpre <- 7.7431
  Kphe <- 14.713
  
  BP <- 1.15 #blood to plasma ratio from Simcyp
  
  # Kp, partition coefficient due to nonspecific protein binding is optimized.
  Kp_endo <- 45.63
  Kp_mid <- 45.63
  Kp_epi<- 45.63
  
  # Passive permeability surface area product in heart tissue -------------------------------------------------------
  PSA_epi = Pdiff_dox * SA_epi * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
  PSA_mid = Pdiff_dox * SA_mid * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
  PSA_endo = Pdiff_dox * SA_endo * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
  
  Kd_DNA <- 0.4  # nM to Molar
  Kd_mtDNA <- 5.6 # nM to Molar
  
  # CL clearance [Leandro 2019] -------------------------------------------------------
  CL_renal <- 0.66 #[L/ h] renal clearance
  CL_hepatic <- 29.97 #[L/ h] hepatic clearance
  CLint_heart <- 0 #[L/ h] heart clearance CYP3A4 was not detected in heart tissue
  
  # MODEL -------------------------------------------------------------------
  parameters <- c(
    BP = BP,
    Kpec = Kpec,
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
    Vendo = Vendo,
    Vmid = Vmid,
    Vepi = Vepi,
    fup = fup,
    funionized_ic = funionized_ic,
    funionized_ec = funionized_ec,
    fu_ec = fu_ec,
    PSA_epi = PSA_epi,
    PSA_mid = PSA_mid,
    PSA_endo = PSA_endo,
    CL_renal = CL_renal,
    CL_hepatic = CL_hepatic
  )
  
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
    Ave = 0,
    Aar = 0,
    Are = 0,
    Aheart_ec = 0,
    Aepi_ict = 0,
    Amid_ict = 0,
    Aendo_ict = 0,
    Abloodcell = 0,
    Cendo_ic = 0,
    Cmid_ic = 0,
    Cepi_ic = 0)
  
  ###Differential equations - mg/h -------------------------------------------------------
  PBPKModel = function(times, state, parameters) {
    with(as.list(c(state, parameters)), {
      inf <- ifelse(times <= t, inf, 0)
      #   if (times <= t)
      #     inf
      # else
      #   0
      
      #DOX concentrations:
      Cadipose <- Aad / Vad    #adipose
      Cbone <- Abo / Vbo		   #bone
      Cbrain <- Abr / Vbr		   #brain
      Cgut <- Agu / Vgu			   #gut
      Ckidney <- Aki / Vki	   #kidney
      Cliver <- Ali / Vli		   #liver
      Cliverfree <-  Cliver * (fup / BP)  #liver free concentration
      Ckidneyfree <- Ckidney * (fup / BP)	 #kidney free concentration
      Clung <- Alu / Vlu		   #lung
      Cmuscle <- Amu / Vmu	   #muscle
      Cskin <- Ask / Vsk		   #skin
      Cspleen <- Asp / Vsp	   #spleen
      Crest <- Are / Vre 			#rest of body
      Cvenous <- Ave / Vve     #venous blood
      Carterial <- Aar / Var	 #arterial blood
      Cplasmavenous <- Cvenous / BP	#venous plasma concentration
      Cbloodcell <- Abloodcell / Vrb#blood cell concentration
      Cheart_ec <- Aheart_ec / Vhe_ec   #heart extracellular fluid
      Cendo_ict <- Aendo_ict / Vendo_ic
      Cmid_ict <- Amid_ict / Vmid_ic
      Cepi_ict <- Aepi_ict / Vepi_ic
      
      # Nuclear sub-compartment
      Cendo_ic <- 0.5 * (sqrt((Cendo_ict + DNA_endo + Kd_DNA )^2 - 4* DNA_endo *Cendo_ict) + sqrt((Cendo_ict + mtDNA_endo + Kd_mtDNA )^2 - 4* mtDNA_endo *Cendo_ict) - (DNA_endo + Kd_DNA + mtDNA_endo + Kd_mtDNA) )
      Cmid_ic <- 0.5 * (sqrt((Cmid_ict + DNA_mid + Kd_DNA )^2 - 4* DNA_mid *Cmid_ict) + sqrt((Cmid_ict + mtDNA_mid + Kd_mtDNA )^2 - 4* mtDNA_mid *Cmid_ict) - (DNA_mid + Kd_DNA + mtDNA_mid + Kd_mtDNA) )
      Cepi_ic <- 0.5 * (sqrt((Cepi_ict + DNA_epi + Kd_DNA )^2 - 4* DNA_epi *Cepi_ict) + sqrt((Cepi_ict + mtDNA_epi + Kd_mtDNA )^2 - 4* mtDNA_epi *Cepi_ict) - (DNA_epi + Kd_DNA + mtDNA_epi + Kd_mtDNA) )
      
      ## rates of changes
      dINFUSION <- -inf
      dAad <- Qad * (Carterial - Cadipose / Kpad * BP) #adipose
      dAbo <- Qbo * (Carterial - Cbone / Kpbo * BP) #bone
      dAbr <- Qbr * (Carterial - Cbrain / Kpbr * BP) #brain
      dAgu <- Qgu * (Carterial - Cgut / Kpgu * BP) #gut
      dAki <- Qki * (Carterial - Ckidney / Kpki * BP) - CL_renal * Ckidney * (fup / BP)  #kidney
      dAli <- Qha * Carterial + Qgu * (Cgut / Kpgu * BP) + Qsp * (Cspleen / Kpsp * BP) - Qli * (Cliver / Kpli * BP) - Cliver * (fup / BP) * CL_hepatic #liver
      dAlu <- Qlu * Cvenous - Qlu * (Clung / Kplu * BP) #lung
      dAmu <- Qmu * (Carterial - Cmuscle / Kpmu * BP)   #muscle
      dAsk <- Qsk * (Carterial - Cskin / Kpsk * BP)  		#skin
      dAsp <- Qsp * (Carterial - Cspleen / Kpsp * BP)  	#spleen
      dAre <- Qre * (Carterial - Crest / Kpre * BP)  		#rest of body
      dAve <- inf + Qad * (Cadipose / Kpad * BP) + Qbo * (Cbone / Kpbo * BP) + Qbr * (Cbrain / Kpbr * BP) + Qgu * (Cgut / Kpgu * BP) + Qki* (Ckidney / Kpki * BP) + Qli * (Cliver / Kpli * BP) + Qmu* (Cmuscle / Kpmu * BP) + Qsk* (Cskin / Kpsk * BP)+ Qhe * (Cheart_ec / Kpec * BP) + Qre*(Crest / Kpre * BP) - Qlu * Cvenous - PER * SA_bloodcell * 0.1 * (Cvenous - Cbloodcell)  				#venous blood
      dAar <- Qlu * (Clung / Kplu * BP) - Qad * Carterial - Qbo * Carterial - Qbr * Carterial - Qgu * Carterial- Qki * Carterial- Qha * Carterial- Qmu * Carterial- Qsk * Carterial- Qsp * Carterial-	Qre * Carterial	#arterial blood
      dAbloodcell <- PER * SA_bloodcell * 0.1 * (Cvenous - Cbloodcell) + PER * SA_bloodcell * 0.1 * (Carterial - Cbloodcell)
      
      # Heart extracellular sub-compartment
      dAheart_ec <-  Qhe * (Carterial - (Cheart_ec/Kpec) * BP) - 0.001 * PER * SA_endo * (Cheart_ec * fu_ec * funionized_ec - Cendo_ic/(Kp_endo * Kpp) * funionized_ic ) -0.001 * PER * SA_mid * (Cheart_ec * fu_ec * funionized_ec - Cmid_ic/(Kp_mid * Kpp) * funionized_ic ) - 0.001 *PER * SA_epi * (Cheart_ec * fu_ec * funionized_ec - Cepi_ic/(Kp_epi * Kpp) * funionized_ic )
      dAendo_ict <-  0.001 * PER * SA_endo * (Cheart_ec * fu_ec * funionized_ec - Cendo_ic/(Kp_endo * Kpp) * funionized_ic ) - Cendo_ict * fu_heart * CLint_heart * (Vendo_ic/Vhe)
      dAmid_ict <-   0.001 * PER * SA_mid * (Cheart_ec * fu_ec * funionized_ec - Cmid_ic/(Kp_mid * Kpp) * funionized_ic ) - Cepi_ict * fu_heart * CLint_heart * (Vmid_ic/Vhe)
      dAepi_ict <-   0.001 * PER * SA_epi * (Cheart_ec * fu_ec * funionized_ec - Cepi_ic/(Kp_epi * Kpp) * funionized_ic ) - Cepi_ict * fu_heart * CLint_heart * (Vepi_ic/Vhe)
      
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
          dAve,
          dAar,
          dAre,
          dAheart_ec,
          dAepi_ict,
          dAmid_ict,
          dAendo_ict,
          dAbloodcell,
          Cendo_ic,
          Cmid_ic,
          Cepi_ic),
        
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
        Crest = Are / Vre,
        MID = (Amid_ict / Vmid_ic),
        ENDO = (Aendo_ict) / Vendo_ic,
        EPI = (Aepi_ict) / Vepi_ic,
        EC = Cheart_ec,
        logHT = log10((Aheart_ec + Aepi_ict + Amid_ict + Aendo_ict) / Vhe),
        logBL = log10(Cplasmavenous),
        BL = Cplasmavenous,
        BLCELL = Cbloodcell,
        HT = (Aheart_ec + Aepi_ict + Amid_ict + Aendo_ict) / Vhe
        )
    })
  }
}

out <-
  ode(
    y = state,
    times = times,
    func = PBPKModel,
    parm = parameters
  )
results <- data.frame(out)

plot(
  results$time,
  results$BL,
  type = "l",
  col = "blue",
  xlab = "Time [h]",
  ylab = "Blood Concentration [mg/L]"
)  

plot(
  results$time,
  results$BLCELL,
  type = "l",
  col = "blue",
  xlab = "Time [h]",
  ylab = "Blood cells Concentration [mg/L]"
)  

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                "#CC79A7")
plot1 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=HT,col="HT"),lty=1,size=1.5)+
  geom_line(aes(y=MID,col= "MID"),lty=1,size=1.5)+
  geom_line(aes(y=ENDO,col="ENDO"),lty=1,size=1.5)+
  geom_line(aes(y=EPI,col="EPI"),lty=1,size=1.5)+
  ylab(" Concentration (mg/L)")+ xlab("Time (hour)")+
  scale_fill_manual( breaks = c("HT","MID","ENDO","EPI"),
                     values = c("#000000", "#E69F00", "#56B4E9","#009E73"),
                     labels = c("HT","MID","ENDO","EPI"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of sub-compartment layers", size=1)
plot1

plot2 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=MID,col= "MID"),lty=1,size=1.5)+
  geom_line(aes(y=ENDO,col="ENDO"),lty=1,size=1.5)+
  geom_line(aes(y=EPI,col="EPI"),lty=1,size=1.5)+
  ylab(" Concentration (mg/L)")+ xlab("Time (hour)")+
  scale_fill_manual( breaks = c("MID","ENDO","EPI"),
                     values = c( "#E69F00", "#56B4E9","#009E73"),
                     labels = c("MID","ENDO","EPI"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of sub-compartment layers", size=1)
plot2
ggplotly(plot2)

plot3 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=HT,col="Mean heart"),lty=1,size=1.5)+
  geom_line(aes(y=MID,col= "Midmyocardial"),lty=1,size=1.5)+
  geom_line(aes(y=BL,col= "Blood"),lty=1,size=1.5)+
  ylab(" Concentration (mg/L)")+ xlab("Time (hour)")+
  scale_x_continuous(limits = c(0, 45))+
  scale_fill_manual( breaks = c("Mean heart","Midmyocardial","Blood"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Mean heart","Midmyocardial","Blood"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of sub-compartment layers", size=1)
plot3


plot4 <-ggplot(data=data.frame(results), aes(x=time))+
  geom_line(aes(y=Cendo_ic,col="Cendo_ic"),lty=1,size=1.5)+
  geom_line(aes(y=Cmid_ic,col= "Cmid_ic"),lty=1,size=1.5)+
  geom_line(aes(y=Cepi_ic,col= "Cepi_ic"),lty=1,size=1.5)+
  ylab(" Concentration (mg/L)")+ xlab("Time (hour)")+
  scale_x_continuous(limits = c(0, 45))+
  scale_fill_manual( breaks = c("Cendo_ic","Cmid_ic","Cepi_ic"),
                     values = c("#000000", "#E69F00", "#56B4E9"),
                     labels = c("Cendo_ic","Cmid_ic","Cepi_ic"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of layers", size=1)
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
  geom_line(aes(y=Crest,col= "Crest"),lty=1,size=1.5)+
  ylab(" Concentration (mg/L)")+ xlab("Time (hour)")+
  scale_x_continuous(limits = c(0, 45))+
  scale_fill_manual( breaks = c("Cadipose","Cbone","Cbrain","Cgut","Ckidney","Cliver","Clung","Cmuscle","Cskin","Crest"),
                     values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7"),
                     labels = c("Cadipose","Cbone","Cbrain","Cgut","Ckidney","Cliver","Clung","Cmuscle","Cskin","Crest"))+
  theme_bw()+theme(text=element_text(size=15),plot.margin = unit(c(5,5,5,5),"mm"))+#changed all plot margins from 5 to 10
  labs(title="Concentration of organs", size=1)
plot5
ggplotly(plot5)



# MPPGL [mg/g] according to [Barter 2008] 
MPPGL <- 10 ^ (1.407 + 0.0158 * age - 0.00038 * (age ^ 2) + 0.0000024 *(age ^ 3))
# the content of CYP1A2 and CYP3A4 in liver [pmol/mg protein]
CYP1A2_L <- 52 
CYP3A4_L <- 137 

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

#Hepatic intrinsic clearance:
CLint_L <- (CLint_1A2 + CLint_3A4) * MPPGL * Vli * liver_density #[L/h]

#HEART (H) CYP3A4 was not detected in heart tissue: assumed no metabolism of DOX in heart tissue

# ELIMINATION 
#1) LIVER
#ISEF
#values from Simcyp. rCYP system: Lymph B
ISEF1A2 = 11.1
ISEF3A4 = 3.92

#fumic
fumic <- 0.82# [Venkatakrishnan 2001]
CYP3A4_H <- 0 #CYP 3A4 abundance in average human heart [pmol/mg tissue][Thum 2000]
