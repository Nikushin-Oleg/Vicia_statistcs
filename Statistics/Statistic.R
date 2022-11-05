library(tidyverse)
library(readxl)
library(ez)
library(effectsize)
library(effsize)
library(pwr)  
library(plotrix)
library(psych)
library(skimr)
library(apa)
#Vicia_Cell_wall_mass.share
#Vicia_Copper_DESORBTION_root
#Vicia_Copper_DESORBTION_shoot
#Vicia_mass
#Vicia_Copper_ENDOGEN_CONT_root
#Vicia_Copper_ENDOGEN_CONT_shoot
#Vicia_pH.root
#Vicia_pH.shoot
#Vicia_pH.Intact_Plants

Vicia_Copper_DESORBTION_root.statistica <- Vicia_Copper_DESORBTION_root %>% 
  group_by(Treatment) %>% 
  summarise(mean(DESORB_FW_ROOT, na.rm = TRUE), sd(DESORB_FW_ROOT, na.rm = TRUE), std.error(DESORB_FW_ROOT, na.rm = TRUE),
          mean(`DESORB_DW_ROOT`, na.rm = TRUE), sd(`DESORB_DW_ROOT`, na.rm = TRUE), std.error(`DESORB_DW_ROOT`, na.rm = TRUE),
          mean(`DESORB_DWCW_ROOT`, na.rm = TRUE), sd(DESORB_DWCW_ROOT, na.rm = TRUE), std.error(`DESORB_DWCW_ROOT`, na.rm = TRUE),
          length(`DESORB_FW_ROOT`),length(`DESORB_DW_ROOT`), length(`DESORB_DWCW_ROOT`))
Vicia_Copper_DESORBTION_shoot.statistica<- Vicia_Copper_DESORBTION_shoot %>% 
  group_by(Treatment) %>% 
  summarise(mean(DESORB_FW_SHOOT, na.rm = TRUE), sd(DESORB_FW_SHOOT, na.rm = TRUE), std.error(DESORB_FW_SHOOT, na.rm = TRUE),
            mean(`DESORB_DW_SHOOT`, na.rm = TRUE), sd(`DESORB_DW_SHOOT`, na.rm = TRUE), std.error(`DESORB_DW_SHOOT`, na.rm = TRUE),
            mean(`DESORB_DWCW_SHOOT`, na.rm = TRUE), sd(DESORB_DWCW_SHOOT, na.rm = TRUE), std.error(`DESORB_DWCW_SHOOT`, na.rm = TRUE),
            length(`DESORB_FW_SHOOT`),length(`DESORB_DW_SHOOT`), length(`DESORB_DWCW_SHOOT`))
Vicia_mass.statistica <- Vicia_mass %>% 
  group_by(Treatment) %>% 
  summarise(mean(FW.ROOT, na.rm = TRUE), sd(FW.ROOT, na.rm = TRUE), std.error(FW.ROOT, na.rm = TRUE),
            mean(FW.SHOOT, na.rm = TRUE), sd(FW.SHOOT, na.rm = TRUE), std.error(FW.SHOOT, na.rm = TRUE),
            mean(DW.ROOT, na.rm = TRUE), sd(DW.ROOT, na.rm = TRUE), std.error(DW.ROOT, na.rm = TRUE),
            mean(DW.SHOOT, na.rm = TRUE), sd(DW.SHOOT, na.rm = TRUE), std.error(DW.SHOOT, na.rm = TRUE),
            mean(HYDRATION.ROOT, na.rm = TRUE), sd(HYDRATION.ROOT, na.rm = TRUE), std.error(HYDRATION.ROOT, na.rm = TRUE),
            mean(HYDRATION.SHOOT, na.rm = TRUE), sd(HYDRATION.SHOOT, na.rm = TRUE), std.error(HYDRATION.SHOOT, na.rm = TRUE))
Vicia_Copper_ENDOGEN_CONT_root.statistica <- Vicia_Copper_ENDOGEN_CONT_root %>% 
  group_by(Treatment) %>% 
  summarise(mean(OZOL_FW_ROOT, na.rm = TRUE), sd(OZOL_FW_ROOT, na.rm = TRUE), std.error(OZOL_FW_ROOT, na.rm = TRUE),
            mean(OZOL_DW_ROOT, na.rm = TRUE), sd(OZOL_DW_ROOT, na.rm = TRUE), std.error(OZOL_DW_ROOT, na.rm = TRUE))

Vicia_Copper_ENDOGEN_CONT_shoot.statistica <- Vicia_Copper_ENDOGEN_CONT_shoot %>% 
  group_by(Treatment) %>% 
  summarise(mean(OZOL_FW_SHOOT, na.rm = TRUE), sd(OZOL_FW_SHOOT, na.rm = TRUE), std.error(OZOL_FW_SHOOT, na.rm = TRUE),
            mean(OZOL_DW_SHOOT, na.rm = TRUE), sd(OZOL_DW_SHOOT, na.rm = TRUE), std.error(OZOL_DW_SHOOT, na.rm = TRUE))

Vicia_pH.root.statistica <- Vicia_pH.root %>% 
  group_by(Treatment) %>% 
  summarise(mean(pH_Sorbtion_ROOT, na.rm = TRUE), sd(pH_Sorbtion_ROOT, na.rm = TRUE), std.error(pH_Sorbtion_ROOT, na.rm = TRUE))

Vicia_pH.shoot.statistica <- Vicia_pH.shoot %>% 
  group_by(Treatment) %>% 
  summarise(mean(pH_Sorbtion_SHOOT, na.rm = TRUE), sd(pH_Sorbtion_SHOOT, na.rm = TRUE), std.error(pH_Sorbtion_SHOOT, na.rm = TRUE))

Vicia_pH.Intact_Plants.statistica <- Vicia_pH.Intact_Plants %>% 
  group_by(Treatment) %>% 
  summarise(mean(pH_Sorbtion_PLANTS, na.rm = TRUE), sd(pH_Sorbtion_PLANTS, na.rm = TRUE), std.error(pH_Sorbtion_PLANTS, na.rm = TRUE))


Vicia_Cell_wall_mass.share.T.test<- Vicia_Cell_wall_mass.share %>% 
  filter(Treatment %in% c('10 mkM', 'Control'))
  
VM <- read_csv2('Масса_растений_08_09_2022.csv')
Vicia_mass %>% 
  arrange(Treatment) %>% 
  filter(Treatment %in% c('Gln 1 mM', 'Control')) %>% 
  View()

Vicia_mass.T.Test<- Vicia_mass %>% 
  filter(Treatment %in% c('Control', 'His 0.5 mM'))

t_test(DW.SHOOT ~ Treatment, data = Vicia_mass.T.Test) 
apa( format = 'docx')


Vicia_Copper_DESORBTION_root.t.test <- Vicia_Copper_DESORBTION_root %>% 
  filter(Treatment %in% c('100 mkM His 1 mM', '100 mkM Gln 1 mM'))
t_test(DESORB_DW_ROOT~ Treatment, data = Vicia_Copper_DESORBTION_root.t.test)
  apa(format = 'docx')

