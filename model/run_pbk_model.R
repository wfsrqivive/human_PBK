library(tictoc)
library(deSolve)
library(dplyr)
source("./functions/PBKmodel.R")

model_results <- read.csv("./input/inputDataFile.csv") %>%
  group_by(compound)%>%
  sample_n(2) %>%
  group_by(compound, Cmax, dose,original.reference, 
           method.KaFa,method.phys.chem.Fup,  method.Fup, method.phys.chem, 
           method.partition, method.Clint, 
           logP, frac.unionized, frac.ionized.acid, frac.ionized.base, Fup, Fuinc, Ka, Fa,
           Kpad, Kpbo, Kpbr, Kpgu, 
           Kphe, Kpki, Kpli,Kplu,Kpmu,Kpsk,Kpsp, 
           Clint, 
           original.reference.Clint) %>%
  do(model(pars = c(BW = 70,
                    dose =.$dose*70*.$Fa,
                    FVad = 0.21, FVbo = 0.085629, FVbr = 0.02, FVgu = 0.0171,	
                    FVhe = 0.0047, FVki = 0.0044, FVli = 0.021, FVlu = 0.0076,		
                    FVmu = 0.4, FVsk = 0.0371, FVsp = 0.0026, FVte = 0.01,                	 
                    FVve = 0.0514, FVar = 0.0257, FVpl = 0.0424, FVrb = 0.0347, 
                    FVre = 0.099771, 
                    QC = 389.988, 
                    FQad = 0.05, FQbo = 0.05, FQbr = 0.12, FQgu = 0.146462,     
                    FQhe = 0.04, FQki = 0.19, FQh = 0.215385, FQlu = 1,	         	
                    FQmu = 0.17, FQsk = 0.05, FQsp = 0.017231, FQte = 0.01076, 
                    FQre = 0.103855,        
                    Kpad = .$Kpad, Kpbo = .$Kpbo, Kpbr = .$Kpbr, Kpgu = .$Kpgu, 
                    Kphe = .$Kphe,  Kpki = .$Kpki,  Kpli = .$Kpli,	Kplu = .$Kplu,	
                    Kpmu = .$Kpmu, Kpsk = .$Kpsk, Kpsp = .$Kpsp,  Kpte = .$Kpmu, 
                    Kpre = .$Kpmu,  	
                    BP = 1, 
                    fup= .$Fup, 
                    fuhep = .$Fuinc,
                    CLint = .$Clint,
                    CLrenal = 6.7,
                    SF= .$scaling.factor,
                    MW = 1, 
                    Ka = .$Ka), 
           tout  = seq(0, 24, by = 0.1),
           state = c(Aad=0, Abo=0, Abr=0, Agu=0, 
                     Ahe=0, Aki=0, Ali=0, Alu=0, 
                     Amu=0, Ask=0, Asp=0, Ate=0, 
                     Ave=0, Aar=0, Are=0, D= .$dose*70*.$Fa,
                     AliClearance = 0, AkiClearance = 0)
  ))

write.csv(model_results, "./results/PBKresults.csv")
