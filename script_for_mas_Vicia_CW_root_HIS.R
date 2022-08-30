install.packages('tidyverse')
library(tidyverse)
install.packages('readxl')
library(readxl)

#СБОР ТАБЛИЧКИ ПОД АНАЛИЗ
Vicia_mass <- read_excel('Vicia_CW_root_HIS.xlsx')
Vicia_mass <- Vicia_mass[,-c((4:23))]
Vicia_Fmass <- Vicia_mass[-c((1:4),(8:17)),]
Vicia_Fmass.10mkM.HIS_1_mM <- Vicia_mass[-c((1:13),(17)),]
Vicia_Dmass.10mkM.HIS_1_mM <- Vicia_mass[-c((1:13),(17)),]
Vicia_Dmass.10mkM <- Vicia_Dmass.10mkM[-c((1:5),(9:17)),]
Vicia_Dmass <- Vicia_mass[-c((1:8),(12:17)),]
Vicia_Fmass <- Vicia_Fmass[-c(1:3),]
install.packages("data.table")
library(data.table)
?data:table
?full_join()
colnames(Vicia_Fmass) <- c('№','FW.root','FW.shoot', 'DW.root','DW.shoot')
colnames(Vicia_Dmass) <- c('№','DW.root','DW.shoot')
colnames(Vicia_Fmass.10mkM) <- c('№','FW.root','FW.shoot')
colnames(Vicia_Fmass.10mkM.HIS_0.5_mM) <- c('№','FW.root','FW.shoot')
colnames(Vicia_Fmass.10mkM.HIS_1_mM) <- c('№','FW.root','FW.shoot')
colnames(Vicia_Dmass.10mkM) <- c('№','DW.root','DW.shoot')
colnames(Vicia_Dmass.10mkM.HIS_0.5_mM) <- c('№','DW.root','DW.shoot')
colnames(Vicia_Dmass.10mkM.HIS_1_mM) <- c('№','DW.root','DW.shoot')
Vicia_F_D_mass<- left_join(Vicia_Fmass, Vicia_Dmass, by = "№")
Vicia_F_D_mass <- Vicia_F_D_mass %>% 
type_convert() %>% 
  round( 4) 
str(Vicia_F_D_mass)
Vicia_F_D_mass$hydration.root <- (Vicia_F_D_mass$FW.root - Vicia_F_D_mass$DW.root)/Vicia_F_D_mass$DW.root
Vicia_F_D_mass <- Vicia_F_D_mass[,-c(5:6)]
Vicia_F_D_mass$`№` <- c('Control.1', 'Control.2', 'Control.3')
Vicia_F_D_mass
Vicia_Fmass.10mkM
Vicia_Fmass.10mkM_HIS.0.5mM
Vicia_Fmass.10mkM_HIS.1mM
Vicia_Dmass.10mkM
Vicia_Dmass.10mkM_HIS.0.5mM
Vicia_Dmass.10mkM_HIS.1mM

Vicia_F_D_mass.10mkM<- left_join(Vicia_Fmass.10mkM, Vicia_Dmass.10mkM, by = "№")
Vicia_F_D_mass.10mkM <- Vicia_F_D_mass.10mkM %>% 
  type_convert() %>% 
  round( 4)
Vicia_F_D_mass.10mkM$hydration.shoot <- 
  (Vicia_F_D_mass.10mkM$FW.shoot - Vicia_F_D_mass.10mkM$DW.shoot)/Vicia_F_D_mass.10mkM$DW.shoot
Vicia_F_D_mass.10mkM %>% 
relocate(Vicia_F_D_mass.10mkM$FW.root, .before = Vicia_F_D_mass.10mkM$hydration.shoot)
Vicia_F_D_mass.10mkM$`№`<- c('10 mkM.1', '10 mkM.2', '10 mkM.3')

Vicia_F_D_mass.10mkM_HIS.0.5mM<- 
  left_join(Vicia_Fmass.10mkM.HIS_0.5_mM, Vicia_Dmass.10mkM.HIS_0.5_mM, by = "№")
Vicia_F_D_mass.10mkM_HIS.0.5mM<-Vicia_F_D_mass.10mkM_HIS.0.5mM %>% 
type_convert() %>% 
  round( 4)
Vicia_F_D_mass.10mkM_HIS.0.5mM$hydration.shoot <- 
  (Vicia_F_D_mass.10mkM_HIS.0.5mM$FW.shoot - Vicia_F_D_mass.10mkM_HIS.0.5mM$DW.shoot)/Vicia_F_D_mass.10mkM_HIS.0.5mM$DW.shoot
Vicia_F_D_mass.10mkM_HIS.0.5mM$`№`<- c('HIS_0.5mM.1', 'HIS_0.5mM.2', 'HIS_0.5mM.3')


Vicia_F_D_mass.10mkM_HIS.1mM<- 
  left_join(Vicia_Fmass.10mkM.HIS_1_mM, Vicia_Dmass.10mkM.HIS_1_mM, by = "№")
Vicia_F_D_mass.10mkM_HIS.1mM<- Vicia_F_D_mass.10mkM_HIS.1mM %>% 
type_convert() %>% 
  round( 4)
Vicia_F_D_mass.10mkM_HIS.1mM$hydration.shoot <- 
  (Vicia_F_D_mass.10mkM_HIS.1mM$FW.shoot - Vicia_F_D_mass.10mkM_HIS.1mM$DW.shoot)/Vicia_F_D_mass.10mkM_HIS.1mM$DW.shoot

Vicia_F_D_mass.10mkM_HIS.1mM$`№`<- c('HIS_1mM.1', 'HIS_1mM.2', 'HIS_1mM.3')
Vicia_FW_DW_Hydration_Mineralisation<- 
  bind_rows(Vicia_F_D_mass,Vicia_F_D_mass.10mkM,Vicia_F_D_mass.10mkM_HIS.0.5mM,Vicia_F_D_mass.10mkM_HIS.1mM)

Vicia_FW_DW_Hydration_Mineralisation <- Vicia_FW_DW_Hydration_Mineralisation %>% 
  type_convert()
Vicia_FW_DW_Hydration_Mineralisation <- Vicia_FW_DW_Hydration_Mineralisation$`№` <- c(1:9) %>% 
  round( 4) 
  
  
Vicia_FW_DW_Hydration_Mineralisation$hydration.shoot <- 
  (Vicia_FW_DW_Hydration_Mineralisation$FW.shoot - Vicia_FW_DW_Hydration_Mineralisation$DW.shoot)/Vicia_FW_DW_Hydration_Mineralisation$DW.shoot
  
readr::write_csv(Vicia_FW_DW_Hydration_Mineralisation, file = 'Vicia_mass_Miniralisation.csv')
Vicia_mass_Miniralisation <- readr::read_csv('Vicia_mass_Miniralisation.csv')


Vicia_mass <- read_excel('Vicia_CW_shoot_HIS.xlsx')
Vicia_mass <- Vicia_mass[-c(12:24),]
Vicia_mass <- Vicia_mass[-c(1,2),]
Vicia_mass <- Vicia_mass[,-c(6:9)]
colnames(Vicia_FW_DW_Hydration_Mineralisation) <- c('Variant','FW.root','FW.shoot', 'DW.root','DW.shoot','hydration.root','hydration.shoot')
Vicia_mass$`№` <- c('Control', 'Control', 'Control', 'Control', 'Control', 'Control', 'Control', 'Control', 'Control')
Vicia_mass<- Vicia_mass %>% 
  type_convert() 
Vicia_mass$`DW.root`
Vicia_mass$DW.root <-  round(Vicia_mass$DW.root, 4)


Vicia_mass
Vicia_mass$hydration.shoot <- 
  (Vicia_mass$FW.shoot - Vicia_mass$DW.shoot)/Vicia_mass$DW.shoot


Vicia_FW_DW_Hydration_Mineralisation <- 
  bind_rows(Vicia_mass, Vicia_FW_DW_Hydration_Mineralisation)


Vicia_mass <- read_excel('Vicia_CW_shoot_HIS (2).xlsx') [-c(1,10),-c((6:11))]
Vicia_mass<- Vicia_mass[-c(1:9),]

Vicia_mass$`№` <- c('10 mkM', '10 mkM', '10 mkM','Gln1', 'Gln1', 'Gln1', 'Gln5', 'Gln5', 'Gln5')
Vicia_mass %>% 
type_convert()

Vicia_FW_DW_Hydration_Mineralisation %>% 
  select(2:7) %>% 
  colMeans()
Vicia_FW_DW_Hydration_Mineralisation
install.packages("ggplot2")
library(ggplot2)
Vicia_FW_DW_Hydration_Mineralisation 
 
Vicia_mass <- read_excel('Vicia_CW_shoot_HIS (2).xlsx') [-c(1,10),-c((6:11))]
Vicia_mass<- Vicia_mass[-c(1:9),]
colnames(Vicia_mass) <- c('Variant','FW.root','FW.shoot', 'DW.root','DW.shoot')
Vicia_mass<- Vicia_mass[-c(1,2),]
Vicia_mass$`№` <- c('100 mkM', '100 mkM', '100 mkM',
                    '100 mkM His 0,5 mM', '100 mkM His 0,5 mM', 
                    '100 mkM His 0,5 mM', '100 mkM His 1 mM', 
                    '100 mkM His 1mM', '100 mkM His 1 mM')
Vicia_mass <- Vicia_mass %>% 
  type_convert() 
Vicia_mass[,c(2:5)] %>% 
  round(4)
Vicia_mass[6,3] <- 1.3215

Vicia_mass <- Vicia_mass[,-6]
  Vicia_mass$hydration.root <- 
  (Vicia_mass$FW.root - Vicia_mass$DW.root)/Vicia_mass$DW.root
Vicia_mass$hydration.shoot <- 
  (Vicia_mass$FW.shoot - Vicia_mass$DW.shoot)/Vicia_mass$DW.shoot
Vicia_mass_Miniralisation<- bind_rows(Vicia_mass_Miniralisation,Vicia_mass)

Vicia_mass_Miniralisation<- read_csv('Vicia_mass_Miniralisation.csv')


Vicia_mass <- read_excel('Vicia_CW_shoot_CW_from_trited_plants_3.xlsx') [-c(1:12), -c(6:11)]
colnames(Vicia_mass) <- c('Variant','FW.root','FW.shoot', 'DW.root','DW.shoot')
Vicia_mass <- Vicia_mass[-1,]
Vicia_mass$Variant <- c('Control', 'Control', 'Control',
                        '100 mkM Gln1', '100 mkM Gln1', '100 mkM Gln1', 
                        '100 mkM Gln5', '100 mkM Gln5','100 mkM Gln5')
                   

Vicia_mass <- Vicia_mass %>% 
  type_convert() 
Vicia_mass[,c(2:5)] %>% 
  round(4)
Vicia_mass$hydration.root <- 
  (Vicia_mass$FW.root - Vicia_mass$DW.root)/Vicia_mass$DW.root
Vicia_mass$hydration.shoot <- 
  (Vicia_mass$FW.shoot - Vicia_mass$DW.shoot)/Vicia_mass$DW.shoot
Vicia_mass_Miniralisation<- bind_rows(Vicia_mass_Miniralisation,Vicia_mass)

Vicia_mass_Miniralisation <- Vicia_mass_Miniralisation[-c(61:69),] 


readr::write_csv(Vicia_mass_Miniralisation, file = 'Vicia_mass_Miniralisation_03_07_2022.csv')


Vicia_mass_Miniralisation <- Vicia_mass_Miniralisation[-c(43:51),]
summary(Vicia_mass_Miniralisation)
install.packages('ez')
library(ez)
?ezANOVA
?mutate()
Vicia_mass_Miniralisation$hydration.root[71] <- (Vicia_mass_Miniralisation$FW.root[71] - Vicia_mass_Miniralisation$DW.root[71]) / Vicia_mass_Miniralisation$DW.root[71]

#ПОДГОТОВКА ДАННЫХ К АНОВЕ
V_m_M_aov <- Vicia_mass_Miniralisation %>% 
  mutate(as.factor(Variant),
         id = row_number())
rm(VM_aov_0_FW.Root)   

#СЫРАЯ МАССА КОРНЕЙ
VM_aov_FW.Root <- aov(FW.root ~ Variant, V_m_M_aov)
VM_aov_FW.Root
summary(VM_aov_FW.Root)
res_VM_aov_FW.Root<- TukeyHSD(VM_aov_FW.Root)
which(res_VM_aov_FW.Root$Variant[,'p adj'] < 0.05)

#СУХАЯ МАССА КОРНЕЙ
VM_aov_DW.Root <- aov(DW.root ~ Variant, V_m_M_aov)
VM_aov_DW.Root %>% 
  summary() 
res_VM_aov_DW.Root<- TukeyHSD(VM_aov_DW.Root)
which(res_VM_aov_DW.Root$Variant[,'p adj'] < 0.05)

#СЫРАЯ МАССА ПОБЕГОВ
VM_aov_FW.shoot <- aov(FW.shoot ~ Variant, V_m_M_aov)
VM_aov_FW.shoot %>% 
  summary() 
res_VM_aov_FW.shoot <- TukeyHSD(VM_aov_FW.shoot)
which(res_VM_aov_FW.shoot$Variant[,'p adj'] < 0.05)

#СУХАЯ МАССА ПОБЕГОВ
VM_aov_DW.shoot <- aov(DW.shoot ~ Variant, V_m_M_aov)
VM_aov_DW.shoot %>% 
  summary() 
res_VM_aov_DW.shoot <- TukeyHSD(VM_aov_DW.shoot)
which(res_VM_aov_DW.shoot$Variant[,'p adj'] < 0.05)

#ОВОДНЕННОСТЬ КОРНЕЙ
VM_aov_hydration.root <- aov(hydration.root ~ Variant, V_m_M_aov)
VM_aov_hydration.root %>% 
  summary() 
res_VM_aov_hydration.root <- TukeyHSD(VM_aov_hydration.root)
which(res_VM_aov_hydration.root$Variant[,'p adj'] < 0.05)
rm(Hydr.root.dif)

#ОВОДНЕННОСТЬ ПОБЕГОВ
VM_aov_hydration.shoot <- aov(hydration.shoot ~ Variant, V_m_M_aov)
VM_aov_hydration.shoot %>% 
  summary() 
res_Vicia_H_SH <- TukeyHSD(VM_aov_hydration.shoot)
which(res_Vicia_H_SH$Varf[,'p adj'] < 0.05)



Vicia <- read_excel('Vicia_CW_root_Gln_pretrit_HIS.XLSX', skip =1,3)[,-c(2:11)]
Vicia <- Vicia [,-c(4:7)]
Vicia <- Vicia[-c(10:22),]
colnames(Vicia) <- c('Variant', 'copper_per_FW', 'copper_per_DW')
Vicia$Variant <- c('10 mkM', '10 mkM','10 mkM', 
                   'Gln 1 mM', 'Gln 1 mM','Gln 1 mM',
                   'Gln 5 mM', 'Gln 5 mM', 'Gln 5 mM')

Vicia_Copper_by_solution <- type_convert(Vicia_Copper_by_solution)
Vicia_Copper_by_solution <- bind_rows(Vicia_Copper_by_solution,Vicia)

readr::write_csv(Vicia_mass_Miniralisation, file = 'Vicia_Copper_by_solution.csv')
