library(tidyverse)
library(readxl)
library(ez)
library(effectsize)
library(effsize)
library(pwr)  
library(plotrix)
library(psych)
library(skimr)
install.packages("apa")
library(apa)
#Сбор таблички со статистиками.
Vicia_Copper_DESORBTION_shoot %>% 
  type_convert() %>% 
  group_by(Variant) %>% 
  summarise(mean(desorbtion_per_FW_shoot, na.rm = TRUE),
            sd(desorbtion_per_FW_shoot, na.rm = TRUE),
            std.error(desorbtion_per_FW_shoot, na.rm = TRUE),
            mean(desorbtion_per_DW_shoot, na.rm = TRUE),
            sd(desorbtion_per_DW_shoot, na.rm = TRUE),
            std.error(desorbtion_per_DW_shoot, na.rm = TRUE),
            mean(desorbtion_per_DWCW_shoot, na.rm = TRUE),
            sd(desorbtion_per_DWCW_shoot, na.rm = TRUE),
            std.error(desorbtion_per_DWCW_shoot, na.rm = TRUE),
            length(desorbtion_per_FW_shoot),
            length(desorbtion_per_DW_shoot),
            length(desorbtion_per_DWCW_shoot)) %>% 
 # select(1,8:13) %>% 
  filter (Variant %in% c('Gln 5 mM', 'Gln 1 mM', '100 mkM Gln 1 mM',
                         '100 mkM Gln 5 mM', 'Tr Gln 5 mM')) %>% 
  View()
statistic_mass <- function(x){
  x %>% 
    group_by(Variant) %>% 
    summarise(mean(CW.share.root, na.rm = TRUE), sd(CW.share.root, na.rm = TRUE),std.error(CW.share.root, na.rm = TRUE),
              mean(CW.share.shoot, na.rm = TRUE), sd(CW.share.shoot, na.rm = TRUE),std.error(CW.share.shoot, na.rm = TRUE),
              mean(FW.shoot, na.rm = TRUE), sd(FW.shoot, na.rm = TRUE),std.error(FW.shoot, na.rm = TRUE),
              mean(DW.shoot, na.rm = TRUE), sd(DW.shoot, na.rm = TRUE),std.error(DW.shoot, na.rm = TRUE),
              mean(hydration.root, na.rm = TRUE), sd(hydration.root, na.rm = TRUE),std.error(hydration.root, na.rm = TRUE),
              mean(hydration.shoot, na.rm = TRUE), sd(hydration.shoot, na.rm = TRUE),std.error(hydration.shoot, na.rm = TRUE),
              length(CW.share.root), length(CW.share.shoot), length(hydration.root),
              length(hydration.shoot))
}
statistic_mass()

Vicia_Cell_wall_mass.share %>% 
  group_by(treatment) %>% 
  summarise(mean(CW.share.root, na.rm = TRUE), sd(CW.share.root, na.rm = TRUE),std.error(CW.share.root, na.rm = TRUE),
            mean(CW.share.shoot, na.rm = TRUE), sd(CW.share.shoot, na.rm = TRUE),std.error(CW.share.shoot, na.rm = TRUE))
#t.testы
  Vicia_Cell_wall_mass.share.T.test <- Vicia_Cell_wall_mass.share %>% 
      filter(Variant %in% c('Control', 'Gln 5 mM')) 
    t.test(`CW.share.shoot`~`Variant`, data = Vicia_Cell_wall_mass.share.T.test)
    
    
Vicia_comparison_CW_VS_Intact_Plants.t.test <-  Vicia_comparison_CW_VS_Intact_Plants %>% 
      filter(Variant %in% c('Gln 5 mM'))
t.test(`mkM_Cu_per_DW` ~ `treatment.2`,Vicia_comparison_CW_VS_Intact_Plants.t.test)

Vicia_comparison_CW_VS_Intact_Plants %>% 
  group_by(treatment.2) %>% 
  summarise(mean(mkM_Cu_per_DW)) %>% 
  View()
Vicia_comparison_Oz_VS_IP %>% 
  group_by(Method.1) %>% 
  summarise(mean(mkM_Cu_per_DW)) %>% 
  View()
Vicia_Copper_DESORBTION_shoot.x3 <- Vicia_Copper_DESORBTION_shoot %>% 
  filter(Variant %in% c('10 mkM 450 ml', 'His 1 mM 450 ml', 'Gln 5 mM 450 ml', '30 mkM', 'His 3 mM 30 mkM', 'Gln 15 mM 30 mkM')) 
Vicia_Copper_DESORBTION_shoot.x3 <- Vicia_Copper_DESORBTION_shoot.x3 %>% 
  slice(-12, -13,-14)
Vicia_Copper_DESORBTION_shoot.x3
 colnames(Vicia_Copper_DESORBTION_shoot.x3) <- c('Variant', 'mkM Cu per FW','mkM Cu per DW','mkM Cu per DWCW')
Vicia_Copper_DESORBTION_SHOOT_VS_ROOT <- bind_rows(Vicia_Copper_DESORBTION_root.x3,Vicia_Copper_DESORBTION_shoot.x3)
Vicia_Copper_DESORBTION_SHOOT_VS_ROOT <- Vicia_Copper_DESORBTION_SHOOT_VS_ROOT %>% 
  mutate(part = c(rep('Root',39), rep('Shoot',14))) %>% 
  mutate(treatment = c(rep('10 mkM Root', 3),
                       rep('Gln 5 mM Root',3),
                       rep('10 mkM Root', 12),
                       rep('Gln 5 mM Root', 3),
                       rep('10 mkM Root', 3),
                       rep('His 1 mM Root', 3),
                       rep('10 mkM Root', 3),
                       rep('Gln 5 mM Root', 3),
                       rep('His 1 mM Root', 3),
                       rep('Gln 5 mM Root', 3),
                       rep('10 mkM Shoot', 2),
                       rep('His 1 mM Shoot', 3),
                       rep('Gln 5 mM Shoot', 3),
                       rep('10 mkM Shoot', 3),
                       rep('Gln 5 mM Shoot', 3))) %>% 
  mutate(treatment.2 = factor(treatment, levels = c('10 mkM Root','10 mkM Shoot',
                                                    'His 1 mM Root','His 1 mM Shoot',
                                                    'Gln 5 mM Root','Gln 5 mM Shoot'))) %>% 
  mutate(treatment.3 = c(rep('10 mkM', 3),
                       rep('Gln 5 mM',3),
                       rep('10 mkM', 12),
                       rep('Gln 5 mM', 3),
                       rep('10 mkM', 3),
                       rep('His 1 mM', 3),
                       rep('10 mkM', 3),
                       rep('Gln 5 mM', 3),
                       rep('His 1 mM', 3),
                       rep('Gln 5 mM', 3),
                       rep('10 mkM', 2),
                       rep('His 1 mM', 3),
                       rep('Gln 5 mM', 3),
                       rep('10 mkM', 3),
                       rep('Gln 5 mM', 3)))
mutate
colnames(Vicia_Copper_DESORBTION_SHOOT_VS_ROOT) <- c('Variant',
                                                     'mkM_Cu_per_FW',
                                                     'mkM_Cu_per_DW',
                                                     'mkM_Cu_per_DWCW',
                                                     'part',
                                                     'treatment',
                                                     'treatment.2',
                                                     'treatment.3')

Vicia_Copper_DESORBTION_SHOOT_VS_ROOT.t.test <-  Vicia_Copper_DESORBTION_SHOOT_VS_ROOT %>% 
  filter(treatment.3 %in% c('His 1 mM'))
t.test(`mkM_Cu_per_DWCW` ~ `part`,Vicia_Copper_DESORBTION_SHOOT_VS_ROOT.t.test)

Vicia_Copper_DESORBTION_SHOOT_VS_ROOT %>% 
  group_by(treatment.2) %>% 
  summarise(mean(mkM_Cu_per_DWCW, na.rm = TRUE), mean(mkM_Cu_per_DW,mean(mkM_Cu_per_FW)) %>% 
  View()

  
