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


Vicia_Copper_DESORBTION_SHOOT_VS_ROOT.t.test <-  Vicia_Copper_DESORBTION_SHOOT_VS_ROOT %>% 
  filter(treatment.3 %in% c('His 1 mM'))
t.test(`mkM_Cu_per_DWCW` ~ `part`,Vicia_Copper_DESORBTION_SHOOT_VS_ROOT.t.test)

Vicia_Copper_DESORBTION_SHOOT_VS_ROOT %>% 
  group_by(treatment.2) %>% 
  summarise(mean(mkM_Cu_per_DWCW, na.rm = TRUE), mean(mkM_Cu_per_DW,mean(mkM_Cu_per_FW))) %>% 
  View()

Vicia_pH.root.t.test <- Vicia_pH.root %>% 
  filter(Variant %in% c('Control', '100 mkM His 1 mM'))
t.test(pH_after_sorbtion.root ~ Variant, Vicia_pH.root.t.test, paired = FALSE)

Vicia_pH.shoot.t.test <- Vicia_pH.shoot %>% 
  filter(Variant %in% c('Control', 'Gln 1 mM'))
t.test(pH_after_sorbtion.shoot ~ Variant, Vicia_pH.shoot.t.test, paired = FALSE)

Vicia_Copper_DESORBTION_root

Vicia_Copper_DESORBTION_root.1 <- Vicia_Copper_DESORBTION_root %>% 
  arrange(Variant) %>% 
  slice(-c(87:98)) %>% 
  select(1:4) %>% 
  mutate(CW_from = c(rep('Root 10 mkM',21),
                     rep('Root 100 mkM',11),
                     rep('Root 100 mkM Gln 1 mM', 3),
                     rep('Root 100 mkM Gln 5 mM', 3),
                     rep('Root 100 mkM His 0.5 mM', 3),
                     rep('Root 100 mkM His 1 mM', 3),
                     rep('Root 50 mkM', 6),
                     rep('Root Gln 1 mM', 12),
                     rep('Root Gln 5 mM', 12),
                     rep('Root His 0.5 mM', 6),
                     rep('Root His 1 mM', 6),
                     rep('Root Tr 10 mkM', 9),
                     rep('Root Tr Gln 5 mM', 9),
                     rep('Root Tr His 1 mM', 9)))

colnames(Vicia_Copper_DESORBTION_shoot.1) <- c('Variant', 'desorbtion_per_FW',
                                              'desorbtion_per_DW', 'desorbtion_per_DWCW', 'CW_from')

Vicia_Copper_DESORBTION_SHOOT_VS_ROOT <- bind_rows(Vicia_Copper_DESORBTION_shoot.1, Vicia_Copper_DESORBTION_root.1)
Vicia_Copper_DESORBTION_SHOOT_VS_ROOT<- Vicia_Copper_DESORBTION_SHOOT_VS_ROOT %>% 
  mutate(treatment_order = factor(Variant, levels = c('10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM',
                                                       '100 mkM', '100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM',
                                                       '50 mkM', 'Tr 10 mkM', 'Tr His 1 mM', 'Tr Gln 5 mM')))
Vicia_Copper_DESORBTION_root %>% 
  arrange(Variant) %>% 
  filter(Variant %in% c('10 mkM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  na.omit() %>% 
  group_by(Variant) %>% 
  summarise(mean(desorbtion_per_FW_root), sd(desorbtion_per_FW_root), std.error(desorbtion_per_FW_root),
            mean(desorbtion_per_DW_root), sd(desorbtion_per_DW_root), std.error(desorbtion_per_DW_root),
            mean(desorbtion_per_DWCW_root), sd(desorbtion_per_DWCW_root), std.error(desorbtion_per_DWCW_root)) %>% 
  View()

Vicia_Copper_DESORBTION_root.T.test <- Vicia_Copper_DESORBTION_root %>% 
  filter(Variant %in% c('10 mkM', 'Gln 1 mM'))
t.test(desorbtion_per_DWCW_root ~ Variant, Vicia_Copper_DESORBTION_root.T.test, var.equal = TRUE)

