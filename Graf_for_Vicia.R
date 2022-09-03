
library(ggplot2)
library(tidyverse)
library(ggtext)
library(glue)
library(patchwork)
library(RColorBrewer)
library(Richtext)
library(gridtext)
install.packages('RColorBrewer')
Vicia_mass <- Vicia_mass %>% 
  mutate(treatment = factor(Variant, levels = c('Control','10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 
                       'Gln 5 mM', '100 mkM', '100 mkM His 0,5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', 
                       '100 mkM Gln 5 mM', '50 mkM', '50 mkM Gln 5')))

Vicia_graff_10mkM_DW.shoot <-  Vicia_mass%>% 
  filter(treatment %in% c('Control', '10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 
                       'Gln 5 mM')) %>%
  ggplot(aes(y = DW.shoot , x = `treatment`,  fill = treatment))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
    labs(y = 'DW побега, г' , x = NULL)+
  #scale_x_discrete(labels = c('Control','100 mkM','100 mkM<br> His 0,5 mM','100 mkM<br> His 1 mM',
                              #'100 mkM<br> Gln 1 mM', '100 mkM<br> Gln 5 mM'))+
  theme_classic()
  theme(axis.text.x = element_markdown())

  Vicia_graff_10mkM_DW.shoot<-  Vicia_graff_10mkM_DW.shoot+
  #geom_line(data = tibble (x = (1:6), y = rep(18,6)), aes(x = x, y = y), inherit.aes = FALSE)+
  geom_text(data = tibble (x = (1), y = (0.22)), aes(x = x, y = y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data = tibble (x = (2), y = (0.23)),aes(x = x, y = y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data = tibble (x = (3), y = (0.26)), aes(x = x, y = y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data = tibble (x = (4), y = (0.245)), aes(x = x, y = y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data = tibble (x = (5), y = (0.24)), aes(x = x, y = y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data = tibble (x = (6), y = (0.224)), aes(x = x, y = y, label = 'a'), inherit.aes = FALSE)
  
#Рисунок 1 Влияние меди на корни  
Vicia_graff_10mkM_DW.root
Vicia_graff_10mkM_hydration.root
Vicia_graff_100mkM_DW.root
Vicia_graff_100mkM_hydration.root
(Vicia_graff_10mkM_DW.root | Vicia_graff_10mkM_hydration.root) / 
  (Vicia_graff_100mkM_DW.root | Vicia_graff_100mkM_hydration.root)+
  plot_annotation(title = 'Рисунок 1', subtitle = 'Влияние ионов меди на корни интактных растений', 
                  tag_levels = 'A')

ggsave('Влияние меди на корни.png', width = 10, height = 10)

#Рисунок 2 Влияние меди на побеги
Vicia_graff_10mkM_DW.shoot
Vicia_graff_100mkM_DW.shoot
Vicia_graff_100mkM_hydration.shoot
Vicia_graff_10mkM_hydration.shoot

(Vicia_graff_10mkM_DW.shoot | Vicia_graff_10mkM_hydration.shoot)/
  (Vicia_graff_100mkM_DW.shoot |Vicia_graff_100mkM_hydration.shoot)+
  plot_annotation(title = 'Рисунок 2', subtitle = 'Влияние ионов меди на побеги интактных растений', 
                  tag_levels = 'A')
ggsave('Влияние меди на побеги.png', width = 10, height = 11)


#График для массовой доли КС
Vicia_Cell_wall_mass.share <- Vicia_Cell_wall_mass.share %>% 
  mutate(treatment = factor(Variant, levels = c('Control', '10 mkM', 'His 1 mM', 'Gln 5 mM', 'Pretrit His 0,5 mM',
                            'Pretrit His 1 mM', 'Pretrit Gln 1 mM', 'Pretrit Gln 5 mM')))


Vicia_Cell_wall_mass.share.shoot<- Vicia_Cell_wall_mass.share %>% 
  filter(treatment %in% c('Control', '10 mkM', 'His 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(y = `CW.share.shoot`, x = `treatment`, fill = `Variant`))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  stat_summary(fun.data = mean_se, geom = 'Bar',show.legend = FALSE)+
  labs(y = 'Массовая доля КС побега', x = NULL)+
  scale_x_discrete(labels = c('Control', '10 mkM', 'His 1 mM', 'Gln 5 mM', 'Pretrit<br> His 0,5 mM',
                              'Pretrit<br> His 1 mM', 'Pretrit<br> Gln 1 mM', 'Pretrit<br> Gln 5 mM'))+
  theme_classic()+
theme(axis.text.x = element_markdown())


Vicia_Cell_wall_mass.share.shoot<- Vicia_Cell_wall_mass.share.shoot+
  geom_text(data=tibble(x = c(1), y = 0.45), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(2), y = 0.52), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(3), y = 0.51), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(4), y = 0.51), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)
 # geom_text(data=tibble(x = c(5), y = 0.52), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  #geom_text(data=tibble(x = c(6), y = 0.42), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  #geom_text(data=tibble(x = c(7), y = 0.48), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)


(Vicia_Cell_wall_mass.share.root |  Vicia_Cell_wall_mass.share.shoot)+
  plot_annotation(title = "Рисунок 3", subtitle = 'Влияние ионов меди и лигандов на массовую долю клеточной стенки',
                  tag_levels = 'A')
ggsave('Массовая доля клеточной стенки.png', width = 6, height = 5)

Vicia_Cell_wall_mass.share.shoot       
  
#График для ОЗОЛЕНИЯ
Vicia_Copper_ENDOGEN_CONT_shoot<- Vicia_Copper_ENDOGEN_CONT_shoot %>% 
  mutate(treatment = factor(Variant, levels = c('Control', '10 mkM', 'His 0.5  mM','His 1  mM', 'Gln 1 mM', 'Gln 5 mM',
                                                '50 mkM', 'Gln 5 mM 50 mkM',
                                                '100 mkM', 'His 0.5 mM 100 mkM', 'His 1 mM 100 mkM',
                                                'Gln 1 mM 100 mkM', 'Gln 5 mM 100 mkM')))
 

Vicia_Copper_ENDOGEN_CONT_shoot_b<- Vicia_Copper_ENDOGEN_CONT_shoot %>% 
  filter(treatment %in% c('Control', '10 mkM', 'His 0.5  mM', 
                          'His 1  mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = treatment, y = Copper_content_per_DW_shoot, fill = treatment))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  labs(x = NULL, y = 'мкМ Cu/г DW побега')+
  #scale_x_discrete(labels = c('Control','100 mkM','100 mkM<br> His 0,5 mM','100 mkM<br> His 1 mM',
  #'100 mkM<br> Gln 1 mM', '100 mkM<br> Gln 5 mM'))+
  theme_classic()
  theme(axis.text.x = element_markdown())
  Vicia_Copper_ENDOGEN_CONT_shoot_b<- Vicia_Copper_ENDOGEN_CONT_shoot_b +
  geom_text(data=tibble(x = c(1), y = 0.42), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(2), y = 0.5), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(3), y = 0.42), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(4), y = 0.33), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(5), y = 0.38), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(6), y = 0.33), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)

Vicia_Copper_ENDOGEN_CONT_root_A
Vicia_Copper_ENDOGEN_CONT_root_B
Vicia_Copper_ENDOGEN_CONT_root_C

Vicia_Copper_ENDOGEN_CONT_root_A / Vicia_Copper_ENDOGEN_CONT_root_B / 
  (Vicia_Copper_ENDOGEN_CONT_root_C)+
  plot_annotation(title = 'Рисунок 4', subtitle = 'Эндогенная концентрация ионов меди в корне', 
                  tag_levels = 'A')

ggsave('Эндогенная концентрация ионов меди в корне.png', width = 8, height = 10)

Vicia_Copper_ENDOGEN_CONT_shoot_A
Vicia_Copper_ENDOGEN_CONT_shoot_b
Vicia_Copper_ENDOGEN_CONT_shoot_C
Vicia_Copper_ENDOGEN_CONT_shoot_A/Vicia_Copper_ENDOGEN_CONT_shoot_b/Vicia_Copper_ENDOGEN_CONT_shoot_C+
  plot_annotation(title = 'Рисунок 5', subtitle = 'Эндогенная концентрация ионов меди в побеге', 
                  tag_levels = 'A')

ggsave('Эндогенная концентрация ионов меди в побеге.png', width = 8, height = 10)

Vicia_Copper_DESORBTION_shoot <- Vicia_Copper_DESORBTION_shoot %>% 
  mutate(treatment = factor(Variant, levels = c('10 mkM','50 mkM', '100 mkM','His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM',
                            '100 mkM His 0.5 mM', '100 mkM His 1 mM','100 mkM Gln 1 mM','100 mkM Gln 5 mM',
                            'Pretrit His 0.5 mM','Pretrit His 1 mM','Pretrit Gln  1 mM', 'Pretrit Gln 5 mM',
                            'Tr 10 mkM', 'Tr His 1 mM', 'Tr Gln 5 mM')))


Vicia_Copper_DESORBTION_root.A <- Vicia_Copper_DESORBTION_root %>% 
  filter(Variant %in% c('10 mkM','50 mkM',  '100 mkM')) %>%
  ggplot(aes(x = treatment, y = desorbtion_per_DWCW_root, fill = treatment))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  labs(x = NULL, y = 'mkM Cu/г DW КС')+
  #scale_x_discrete(labels = c('10 mkM', 'His 1 mM', 'Gln 5 mM', 'Tr 10 mkM', 'Tr His 1 mM', 'Tr Gln 5 mM'))+
  theme_classic()
 # theme(axis.text.x = element_markdown())
Vicia_Copper_DESORBTION_root.C <- Vicia_Copper_DESORBTION_root.C+
  geom_text(data=tibble(x = c(1), y = 260), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(2), y = 260), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(3), y = 225), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(4), y = 225), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(5), y = 225), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)
  

(Vicia_Copper_DESORBTION_root.A / Vicia_Copper_DESORBTION_root.B / Vicia_Copper_DESORBTION_root.C)+
  plot_annotation(title = 'Рисунок 6', subtitle = 'Десорбция КС корней', tag_levels = 'A')
Vicia_Copper_DESORBTION_root.B
Vicia_Copper_DESORBTION_root.C
ggsave('Десорбция КС корней.png', width = 6, height = 9)



Vicia_Copper_DESORBTION_shoot.C <- Vicia_Copper_DESORBTION_shoot %>% 
  filter(Variant %in% c('100 mkM','100 mkM His 0.5 mM',  '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>%
  ggplot(aes(x = treatment, y = desorbtion_per_DWCW_shoot, fill = treatment))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  labs(x = NULL, y = 'mkM Cu/г DW КС')+
  scale_x_discrete(labels = c('100 mkM',
                              '100 mkM<br> His 0.5 mM',
                              '100 mkM<br> His 1 mM', 
                              '100 mkM<br> Gln 1 mM',
                              '100 mkM<br> Gln 5 mM'))+
  theme_classic()+
 theme(axis.text.x = element_markdown())
Vicia_Copper_DESORBTION_shoot.C <- Vicia_Copper_DESORBTION_shoot.C+
  geom_text(data=tibble(x = c(1), y = 120), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(2), y = 135), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(3), y = 120), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(4), y = 115), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(5), y = 110), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)

Vicia_Copper_DESORBTION_shoot.A
Vicia_Copper_DESORBTION_shoot.B
Vicia_Copper_DESORBTION_shoot.C
(Vicia_Copper_DESORBTION_shoot.A / Vicia_Copper_DESORBTION_shoot.B / Vicia_Copper_DESORBTION_shoot.C)+
  plot_annotation(title = 'Рисунок 7', subtitle = 'Десорбция КС побегов', tag_levels = 'A')
ggsave('Десорбция КС побегов.png', width = 6, height = 8.5)


Vicia_Copper_DESORBTION_root.TRCW.A<- Vicia_Copper_DESORBTION_root %>% 
  filter(treatment %in% c('10 mkM','Tr 10 mkM','His 1 mM', 'Gln 5 mM','Tr His 1 mM', 'Tr Gln 5 mM')) %>% 
  mutate(treatment.2 = factor(treatment, levels =  
                                c('10 mkM','Tr 10 mkM','His 1 mM','Tr His 1 mM', 'Gln 5 mM','Tr Gln 5 mM'))) %>% 
  ggplot(aes(x = treatment.2, y = desorbtion_per_DWCW_root, fill = treatment))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  scale_fill_manual(values = c('steelblue1', 'steelblue1', 'steelblue1', 'indianred1', 'indianred1', 'indianred1'))+
  labs(x = NULL, y = 'mkM Cu/г DW КС')+
  scale_x_discrete(labels = c('Control', 'Treated plant','Control', 'Treated plant','Control', 'Treated plant'))+
  geom_richtext(data=tibble(x = c(1.5), y = 55), aes(x=x, y=y, label = '10 mkM Cu'), inherit.aes = FALSE, size = 4,
                fill = NA, label.colour = NA)+
  geom_richtext(data=tibble(x = c(3.5), y = 55), aes(x=x, y=y, label = '10 mkM Cu<br> His 1 mM'), inherit.aes = FALSE, size = 4,
                fill = NA, label.colour = NA)+
  geom_richtext(data=tibble(x = c(5.5), y = 55), aes(x=x, y=y, label = '10 mkM Cu<br> Gln 5 mM'), inherit.aes = FALSE, size = 4,
                fill = NA, label.colour = NA)+
  theme_classic()
Vicia_Copper_DESORBTION_root.TRCW.A<- Vicia_Copper_DESORBTION_root.TRCW.A+
  geom_text(data=tibble(x = c(1), y = 38), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(2), y = 47), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(3), y = 38), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(4), y = 42), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(5), y = 43), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(6), y = 43), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)

Vicia_Copper_DESORBTION_root.TRCW.A

Vicia_Copper_DESORBTION_shoot.TRCW.A<- Vicia_Copper_DESORBTION_shoot %>% 
  filter(treatment %in% c('10 mkM','Tr 10 mkM','His 1 mM', 'Gln 5 mM','Tr His 1 mM', 'Tr Gln 5 mM')) %>% 
  mutate(treatment.2 = factor(treatment, levels =  
                                c('10 mkM','Tr 10 mkM','His 1 mM','Tr His 1 mM', 'Gln 5 mM','Tr Gln 5 mM'))) %>% 
  ggplot(aes(x = treatment.2, y = desorbtion_per_DWCW_shoot, fill = treatment))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  scale_fill_manual(values = c('steelblue1', 'steelblue1', 'steelblue1', 'indianred1', 'indianred1', 'indianred1'))+
  labs(x = NULL, y = 'mkM Cu/г DW КС')+
  scale_x_discrete(labels = c('Control', 'Treated plant','Control', 'Treated plant','Control', 'Treated plant'))+
  geom_richtext(data=tibble(x = c(1.5), y = 20), aes(x=x, y=y, label = '10 mkM Cu'), inherit.aes = FALSE, size = 4,
                fill = NA, label.colour = NA)+
  geom_richtext(data=tibble(x = c(3.5), y = 20), aes(x=x, y=y, label = '10 mkM Cu<br> His 1 mM'), inherit.aes = FALSE, size = 4,
                fill = NA, label.colour = NA)+
  geom_richtext(data=tibble(x = c(5.5), y = 20), aes(x=x, y=y, label = '10 mkM Cu<br> Gln 5 mM'), inherit.aes = FALSE, size = 4,
                fill = NA, label.colour = NA)+
  theme_classic()
Vicia_Copper_DESORBTION_shoot.TRCW.A<- Vicia_Copper_DESORBTION_shoot.TRCW.A+
  geom_text(data=tibble(x = c(1), y = 14.8), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(2), y = 11), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(3), y = 14.8), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(4), y = 9), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(5), y = 18), aes(x=x, y=y, label = 'a'), inherit.aes = FALSE, size = 4)+
  geom_text(data=tibble(x = c(6), y = 10), aes(x=x, y=y, label = 'b'), inherit.aes = FALSE, size = 4)

(Vicia_Copper_DESORBTION_root.TRCW.A | Vicia_Copper_DESORBTION_shoot.TRCW.A)+
  plot_annotation(title = 'Рисунок 8', subtitle = 'Сравнение десорбции КС контрольных и опытных растений',
                  tag_levels = 'A')
ggsave('Десорбция КС Контроль VS опыт.png', width = 10, height = 8.5)



Vicia_Copper_by_solution
Vicia_Copper_ENDOGEN_CONT_root_1 <- Vicia_Copper_ENDOGEN_CONT_root %>% 
  filter(Variant %in% c('10 mkM', 'His 0.5 mM', 'His 1 mM',
                        'Gln 1 mM', 'Gln 5 mM')) %>% 
  arrange(Variant) %>% 
  mutate(preporation = c(rep('10 mkM Oz', 18),
                         rep('Gln 1 mM Oz', 9),
                         rep('Gln 5 mM Oz', 9),
                         rep('His 0.5 mM Oz',3),
                         rep('His 1 mM Oz', 3))) %>% 
  mutate(Method.1 = factor(preporation, levels = c('10 mkM Oz',
                                                   'His 0.5 mM Oz',
                                                   'His 1 mM Oz',
                                                   'Gln 1 mM Oz',
                                                   'Gln 5 mM Oz')))
colnames(Vicia_Copper_ENDOGEN_CONT_root_1) <-  c('Variant', 'mkM_Cu_per_FW', 'mkM_Cu_per_DW','prep', 'Method.1')
Vicia_Copper_ENDOGEN_CONT_root_1 <- Vicia_Copper_ENDOGEN_CONT_root_1[,-4]
Vicia_comparison_Oz_VS_IP <- bind_rows(Vicia_Copper_ENDOGEN_CONT_root_1,Vicia_Copper_by_solution )
Vicia_comparison_Oz_VS_IP <- Vicia_comparison_Oz_VS_IP %>% 
  mutate(order.2 = factor(Method.1, levels = c('10 mkM IP', '10 mkM Oz',
                                               'His 0.5 mM IP', 'His 0.5 mM Oz',
                                               'His 1 mM IP', 'His 1 mM Oz',
                                               'Gln 1 mM IP', 'Gln 1 mM Oz',
                                               'Gln 5 mM IP', 'Gln 5 mM Oz')))
#########################################################################################
Vicia_comparison_Oz_VS_IP.ggplot <- Vicia_comparison_Oz_VS_IP %>% 
  filter(order.2 %in% c('10 mkM IP', '10 mkM Oz',
                        'His 0.5 mM IP', 'His 0.5 mM Oz',
                        'His 1 mM IP', 'His 1 mM Oz',
                        'Gln 1 mM IP', 'Gln 1 mM Oz',
                        'Gln 5 mM IP', 'Gln 5 mM Oz')) %>% 
  ggplot(aes(x = order.2, y = mkM_Cu_per_DW, fill = order.2))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', size = 4, show.legend = FALSE)+
  labs(x = NULL, y = 'mkM Cu / г DW корня')+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 8.5)+
  scale_x_discrete(labels = c('По раствору','Озоление', 
                              'По раствору','Озоление', 
                              'По раствору','Озоление', 
                              'По раствору','Озоление', 
                              'По раствору','Озоление'))+
  scale_fill_manual(values = c('steelblue1', 'indianred1', 
                              'steelblue1', 'indianred1',
                              'steelblue1','indianred1',
                              'steelblue1','indianred1',
                              'steelblue1','indianred1'))+
  geom_richtext(data = tibble(x = 1.5, y = 20), aes(x = x, y =y, label = '10 mkM'), inherit.aes = FALSE, size = 5,
                fill = NA, label.colour= NA)+
  geom_richtext(data = tibble(x = 3.5, y = 20), aes(x = x, y =y, label = '10 mkM<br> His 0.5 mM'), inherit.aes = FALSE, size = 5,
                fill = NA, label.colour= NA)+
  geom_richtext(data = tibble(x = 5.5, y = 20), aes(x = x, y =y, label = '10 mkM<br> His 1 mM'), inherit.aes = FALSE, size = 5,
                fill = NA, label.colour= NA)+
  geom_richtext(data = tibble(x = 7.5, y = 20), aes(x = x, y =y, label = '10 mkM<br> Gln 1 mM'), inherit.aes = FALSE, size = 5,
                fill = NA, label.colour= NA)+
  geom_richtext(data = tibble(x = 9.5, y = 20), aes(x = x, y =y, label = '10 mkM<br> Gln 5 mM'), inherit.aes = FALSE, size = 5,
                fill = NA, label.colour= NA)+
  geom_text(data = tibble(x= 1, y = 18), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 2, y = 8.9), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 3, y = 18), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 4, y = 5), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 5, y = 15.5), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 6, y = 4), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 7, y = 12), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 8, y = 5), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 9, y = 8), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 10, y = 2.7), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  theme_classic()
Vicia_comparison_Oz_VS_IP.ggplot+
  plot_annotation(title = 'Рисунок 9', subtitle = 'Сравнения концентрации меди в корнях с убылью металла из раствора после обработки растений.')
ggsave('Сравнение Озоления с поглощением по растворам.png', width = 8, height = 8)



Sravnenie_desorbc_s_Ozol_1<- Sravnenie_desorbc_s_Ozol %>% 
  filter(Variant %in% c('10 mkM', '100 mkM', 'His 0.5 mM', 'His 1 mM',
                        'Gln 1 mM', 'Gln 5 mM', 'His 0.5 mM 100 mkM',
                        'His 1 mM 100 mkM', 'Gln 1 mM 100 mkM', 'Gln 5 mM 100 mkM',
                        '100 mkM His 0.5 mM', '100 mkM His 1 mM',
                        '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  arrange(Variant)
Sravnenie_desorbc_s_Ozol_1<- Sravnenie_desorbc_s_Ozol_1 %>% 
  mutate(method.2 = c(rep('10 mkM Oz', 18),
                      rep('10 mkM CW', 21),
                      rep('100 mkM Oz', 12),
                      rep('100 mkM CW', 11),
                      rep('100 mkM Gln 1 mM CW', 3),
                      rep('100 mkM Gln 5 mM CW', 3),
                      rep('100 mkM His 0.5 mM CW', 3),
                      rep('100 mkM His 1 mM CW', 3),
                      rep('Gln 1 mM Oz', 9),
                      rep('Gln 1 mM CW', 12),
                      rep('100 mkM Gln 1 mM Oz', 6),
                      rep('Gln 5 mM Oz', 9),
                      rep('Gln 5 mM CW', 12),
                      rep('100 mkM Gln 5 mM Oz', 6),
                      rep('His 0.5 Oz', 3),
                      rep('His 0.5 CW', 6),
                      rep('100 mkM His 0.5 mM Oz', 6),
                      rep('His 1 mM Oz', 3),
                      rep('His 1 mM CW', 6),
                      rep('100 mkM His 1 mM Oz', 6))) %>% 
mutate(Method = factor(method.2, levels = c('10 mkM CW','10 mkM Oz',
                                            'His 0.5 CW','His 0.5 Oz', 
                                            '100 mkM CW','100 mkM Oz', 
                                            'His 1 mM CW','His 1 mM Oz',
                                            'Gln 1 mM CW','Gln 1 mM Oz',
                                            'Gln 5 mM CW','Gln 5 mM Oz',
                                            '100 mkM His 0.5 mM CW', '100 mkM His 0.5 mM Oz',
                                            '100 mkM His 1 mM CW', '100 mkM His 1 mM Oz',
                                            '100 mkM Gln 1 mM CW', '100 mkM Gln 1 mM Oz',
                                            '100 mkM Gln 5 mM CW', '100 mkM Gln 5 mM Oz')))


Sravnenie_desorbc_s_Ozol_1.B <- Sravnenie_desorbc_s_Ozol_1 %>% 
  filter(Method %in% c('100 mkM CW','100 mkM Oz',
                       '100 mkM His 0.5 mM CW','100 mkM His 0.5 mM Oz', 
                       '100 mkM His 1 mM CW','100 mkM His 1 mM Oz',
                       '100 mkM Gln 1 mM CW','100 mkM Gln 1 mM Oz',
                       '100 mkM Gln 5 mM CW','100 mkM Gln 5 mM Oz')) %>% 
  ggplot(aes(x = Method, y = Copper_per_DW, fill = Method))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  labs(x = NULL, y = 'mkM Cu / г DW корня')+
  scale_x_discrete(labels = c('КС', 'Озоление',
                              'КС', 'Озоление',
                              'КС', 'Озоление',
                              'КС', 'Озоление',
                              'КС', 'Озоление'))+
  scale_fill_manual(values = c('steelblue1', 'indianred1', 
                               'steelblue1', 'indianred1',
                               'steelblue1','indianred1',
                               'steelblue1','indianred1',
                               'steelblue1','indianred1'))+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 8.5)+
  geom_richtext(data=tibble(x = 1.5, y = 180), aes(x=x, y=y, label = '100 mkM'), inherit.aes = FALSE, fill = NA, label.colour = NA, size = 5)+
  geom_richtext(data=tibble(x = 3.5, y = 180), aes(x=x, y=y, label = '100 mkM<br> His 0.5 mM'), inherit.aes = FALSE, fill = NA, label.colour = NA, size = 5)+
  geom_richtext(data=tibble(x = 5.5, y = 180), aes(x=x, y=y, label = '100 mkM<br> His 1 mM'), inherit.aes = FALSE, fill = NA, label.colour = NA, size = 5)+
  geom_richtext(data=tibble(x = 7.5, y = 180), aes(x=x, y=y, label = '100 mkM<br> Gln 1 mM'), inherit.aes = FALSE, fill = NA, label.colour = NA, size = 5)+
  geom_richtext(data=tibble(x = 9.5, y = 180), aes(x=x, y=y, label = '100 mkM<br> Gln 5 mM'), inherit.aes = FALSE, fill = NA, label.colour = NA, size = 5)+
  geom_text(data = tibble(x= 1, y = 125), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 2, y = 58), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 3, y = 125), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 4, y = 20), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 5, y = 105), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 6, y = 20), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 7, y = 110), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 8, y = 20), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 9, y = 110), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 10, y = 18), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  theme_classic()
  
ggsave('Сравнение Десорбции с озолением.png', width = 8, height = 6)

(Sravnenie_desorbc_s_Ozol_1.A | Sravnenie_desorbc_s_Ozol_1.B)+
  plot_annotation(title = 'Рисунок 10', subtitle = 'Сравнение десорбции с озолением', tag_levels = 'A')
ggsave('Сравнение Десорбции с озолением.png', width = 12, height = 10)



Vicia_Copper_DESORBTION_SHOOT_VS_ROOT %>% 
  filter(CW_from %in% c('Root 10 mkM', 'Shoot 10 mkM', 
                        'Root His 0.5 mM', 'Shoot His 0.5 mM', 
                        'Root His 1 mM', 'Shoot His 1 mM',
                        'Root Gln 1 mM','Shoot Gln 1 mM',
                        'Root Gln 5 mM','Shoot Gln 5 mM')) %>% 
  ggplot(aes(x = CW_from, y = desorbtion_per_DW, fill = Variant))+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  labs(x = NULL, y = 'mkM Cu на г DW КС')
  scale_x_discrete(labels = c('КС корня', 'КС побега',
                              'КС корня', 'КС побега',
                              'КС корня', 'КС побега'))+
  scale_fill_manual(values = c('steelblue1', 'indianred1', 
                               'steelblue1', 'indianred1',
                               'steelblue1','indianred1',
                               'steelblue1','indianred1'))+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_richtext(data=tibble(x = 1.5, y=50), aes(x=x, y=y, label = '10 mkM CuCl2'), inherit.aes = FALSE,
                fill = NA, label.colour = NA)+
  geom_richtext(data=tibble(x = 3.5, y=50), aes(x=x, y=y, label = '10 mkM CuCl2<br> His 1 mM'), inherit.aes = FALSE,
                fill = NA, label.colour = NA)+
  geom_richtext(data=tibble(x = 5.5, y=50), aes(x=x, y=y, label = '10 mkM CuCl2<br> Gln 5 mm'), inherit.aes = FALSE,
                fill = NA, label.colour = NA)+
  geom_text(data = tibble(x= 1, y = 38), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 2, y = 32), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 3, y = 38), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 4, y = 30), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 5, y = 43), aes(x = x, y=y, label='a'),inherit.aes = FALSE, size = 4)+
  geom_text(data = tibble(x= 6, y = 32), aes(x = x, y=y, label='b'),inherit.aes = FALSE, size = 4)+
  theme_classic()+
  plot_annotation(title = 'Рисунок 11', subtitle = 'Сравнения КС корней с КС побегов')
  
ggsave('Сравнение КС корней с КС побегов.png', width = 8, height = 6)
