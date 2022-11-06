
library(ggplot2)
library(tidyverse)
library(ggtext)
library(glue)
library(patchwork)
library(RColorBrewer)
library(gridtext)
library(tidytext)
library(embed)
install.packages('Richtext')
Vicia_mass <- Vicia_mass %>% 
  mutate(Order = factor(Treatment, 
                        levels = c('Control', '10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM', '50 mkM', '50 mkM Gln 5 mM',
                                             '100 mkM', '100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')))
Vicia_mass.DW.ROOT.10_mkM <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = DW.ROOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'сухая масса корня, г воды /г DW')+
  geom_text(data=tibble(x=1, y=0.085), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=0.08), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=0.09), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=0.085), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=0.09), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=0.09), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()

(Vicia_mass.HYDRATION.ROOT/ Vicia_mass.HYDRATION.ROOT.10_mkM/ Vicia_mass.HYDRATION.ROOT.100_mkM)+
  plot_annotation(title = 'Рисунок 1', subtitle = 'Токсическое действие ионов меди', tag_level = 'A')
ggsave('Влияние ионов меди на оводненность корня.png', height = 9, width = 8.23)  

Vicia_Copper_ENDOGEN_CONT_root <- Vicia_Copper_ENDOGEN_CONT_root %>% 
  mutate(order = factor(Treatment, levels = c('Control', '10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM', '50 mkM', '50 mkM Gln 5 mM',
                                              '100 mkM', 'His 0.5 mM 100 mkM', 'His 1 mM 100 mkM', 'Gln 1 mM 100 mkM', 'Gln 5 mM 100 mkM')))

Vicia_Copper_ENDOGEN_CONT_shoot.100mkM.ROOT<- Vicia_Copper_ENDOGEN_CONT_root %>% 
  filter(Treatment %in% c('Control', '100 mkM','His 0.5 mM 100 mkM', 'His 1 mM 100 mkM', 'Gln 1 mM 100 mkM', 'Gln 5 mM 100 mkM')) %>% 
  ggplot(aes(x = order, y = OZOL_DW_ROOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar',width=0.5)+
  labs(x=NULL, y = 'мкМ меди / г сухой массы корня')+
  geom_text(data=tibble(x=1, y =3), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y = 55), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y = 18), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y = 16), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y = 18), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y = 10), aes(x=x,y=y,label = 'd'),inherit.aes = FALSE)+
  scale_x_discrete(labels = c('Control', '100 mkM','100 mkM<br>His 0.5 mM', '100 mkM<br>His 1 mM', 
                              '100 mkM<br>Gln 1 mM', '100 mkM<br>Gln 5 mM'))+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

(Vicia_Copper_ENDOGEN_CONT_root.NL|Vicia_Copper_ENDOGEN_CONT_shoot.NL)/
(Vicia_Copper_ENDOGEN_CONT_root.10mkM|Vicia_Copper_ENDOGEN_CONT_shoot.10mkM.SHOOT)/
(Vicia_Copper_ENDOGEN_CONT_root.100mkM|Vicia_Copper_ENDOGEN_CONT_shoot.100mkM.SHOOT)+
  plot_annotation(title = 'Рисунок 5', subtitle = 'Содержание ионов меди в тканях корней и побегов', tag_level = 'A')

ggsave('Содержание ионов меди в тканях корней и побегов.png', height = 9, width = 9)

Vicia_Copper_DESORBTION_root<- Vicia_Copper_DESORBTION_root %>% 
  mutate(Graf.order = factor(Treatment, levels = c('10 mkM', '50 mkM', '100 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM',
                                                   '100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM',
                                                   'Pretrit Gln  1 mM', 'Pretrit Gln 5 mM', 'Pretrit His 0.5 mM', 'Pretrit His 1 mM',
                                                   'Tr 10 mkM', 'Tr Gln 5 mM', 'Tr His 1 mM')))
Vicia_Copper_DESORBTION_shoot<- Vicia_Copper_DESORBTION_shoot %>% 
  select(1:4) %>% 
  mutate(Graf.order = factor(Treatment, levels = c('10 mkM', '50 mkM', '100 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM',
                                                   '100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM',
                                                   'Pretrit Gln  1 mM', 'Pretrit Gln 5 mM', 'Pretrit His 0.5 mM', 'Pretrit His 1 mM',
                                                   'Tr 10 mkM', 'Tr Gln 5 mM', 'Tr His 1 mM')))


Vicia_Copper_DESORBTION_root.graf.No_ligands <- Vicia_Copper_DESORBTION_root %>% 
  filter(Treatment %in% c('10 mkM', '50 mkM','100 mkM')) %>% 
  ggplot(aes(x=Graf.order, y = DESORB_DW_ROOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  labs(x=NULL, y = 'мкМ Cu / г DW корня')+
  geom_text(data=tibble(x=1,y=25), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2,y=90), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=130), aes(x=x,y=y, label='c'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()

Vicia_Copper_DESORBTION_root.graf.10_mkM <- Vicia_Copper_DESORBTION_root %>% 
  filter(Treatment %in% c('10 mkM', 'His 0.5 mM','His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x=Graf.order, y = DESORB_DW_ROOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  labs(x=NULL, y = 'мкМ Cu / г DW корня')+
  geom_text(data=tibble(x=1,y=18), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2,y=18), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=18), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4,y=20), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5,y=20), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()

Vicia_Copper_DESORBTION_root.graf.100_mkM <- Vicia_Copper_DESORBTION_root %>% 
  filter(Treatment %in% c('100 mkM', '100 mkM His 0.5 mM','100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  ggplot(aes(x=Graf.order, y = DESORB_DW_ROOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  labs(x=NULL, y = 'мкМ Cu / г DW корня')+
  geom_text(data=tibble(x=1,y=128), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2,y=128), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=100), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4,y=110), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5,y=110), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  scale_x_discrete(labels = c('100 mkM','100 mkM<br>His 0.5 mM', '100 mkM<br>His 1 mM', 
                              '100 mkM<br>Gln 1 mM', '100 mkM<br>Gln 5 mM'))+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

(Vicia_Copper_DESORBTION_root.graf.No_ligands/
Vicia_Copper_DESORBTION_root.graf.10_mkM/
Vicia_Copper_DESORBTION_root.graf.100_mkM)

Vicia_Copper_DESORBTION_shoot.graf.No_ligands <- Vicia_Copper_DESORBTION_shoot %>% 
  filter(Treatment %in% c('10 mkM', '50 mkM','100 mkM')) %>% 
  ggplot(aes(x=Graf.order, y = DESORB_DW_SHOOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  labs(x=NULL, y = 'мкМ Cu / г DW побега')+
  geom_text(data=tibble(x=1,y=10), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2,y=35), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=55), aes(x=x,y=y, label='c'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()

Vicia_Copper_DESORBTION_shoot.graf.10_mkM <- Vicia_Copper_DESORBTION_shoot %>% 
  filter(Treatment %in% c('10 mkM', 'His 0.5 mM','His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x=Graf.order, y = DESORB_DW_SHOOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  labs(x=NULL, y = 'мкМ Cu / г DW побега')+
  geom_text(data=tibble(x=1,y=7), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2,y=6.5), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=6), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4,y=7.8), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5,y=8.5), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()

Vicia_Copper_DESORBTION_shoot.graf.100_mkM <- Vicia_Copper_DESORBTION_shoot %>% 
  filter(Treatment %in% c('100 mkM', '100 mkM His 0.5 mM','100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  ggplot(aes(x=Graf.order, y = DESORB_DW_SHOOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.3)+
  labs(x=NULL, y = 'мкМ Cu / г DW побега')+
  geom_text(data=tibble(x=1,y=55), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2,y=55), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=50), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4,y=55), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5,y=50), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  scale_x_discrete(labels = c('100 mkM','100 mkM<br>His 0.5 mM', '100 mkM<br>His 1 mM', 
                              '100 mkM<br>Gln 1 mM', '100 mkM<br>Gln 5 mM'))+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())


(Vicia_Copper_DESORBTION_root.graf.No_ligands|Vicia_Copper_DESORBTION_shoot.graf.No_ligands)/
    (Vicia_Copper_DESORBTION_root.graf.10_mkM|Vicia_Copper_DESORBTION_shoot.graf.10_mkM)/
    (Vicia_Copper_DESORBTION_root.graf.100_mkM|Vicia_Copper_DESORBTION_shoot.graf.100_mkM)+
  plot_annotation(title = 'Рисунок 6', subtitle = 'Влияние лигандов на адсорбцию ионов меди изолированными клеточными стенками',tag_levels = 'A')

ggsave('Влияние лигандов на адсорбцию ионов меди изолированными клеточными стенками.png', height = 8, width = 9)


Vicia_Copper_DESORBTION_root.DES<- Vicia_Copper_DESORBTION_root %>% 
  filter(Treatment%in% c('100 mkM', '100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  mutate(method = c(rep('100 mkM Des',8),
                    rep('100 mkM Gln 1 mM 10 mkM Des',3),
                    rep('100 mkM Gln 5 mM 10 mkM Des',3),
                    rep('100 mkM His 0.5 mM 10 mkM Des', 3),
                    rep('100 mkM His 1 mM 10 mkM Des',3))) %>% 
  select(1:3,6)

Vicia_Copper_ENDOGEN_CONT_root.MIN <- Vicia_Copper_ENDOGEN_CONT_root %>% 
  filter(Treatment%in% c('100 mkM', 'His 0.5 mM 100 mkM', 'His 1 mM 100 mkM', 'Gln 1 mM 100 mkM', 'Gln 5 mM 100 mkM')) %>% 
  arrange(Treatment) %>% 
  mutate(method = c(rep('100 mkM Min',12),
                    rep('100 mkM Gln 1 mM 10 mkM Min',6),
                    rep('100 mkM Gln 5 mM 10 mkM Min',6),
                    rep('100 mkM His 0.5 mM 10 mkM Min', 6),
                    rep('100 mkM His 1 mM 10 mkM Min',6))) %>% 
  select(1:3,5)

Vicia_Copper_ENDOGEN_CONT_root.MIN
Vicia_Copper_DESORBTION_root.DES
colnames(Vicia_Copper_DESORBTION_root.DES) <- c('Treatment', 'mkM_Cu_per_FW', 'mkM_Cu_per_DW', 'method')
Vicia_Des_VS_Min.100_mkM <- bind_rows(Vicia_Copper_DESORBTION_root.DES,Vicia_Copper_ENDOGEN_CONT_root.MIN )

Vicia_Des_VS_Min.graf<- Vicia_Des_VS_Min %>% 
  mutate(graf.order = factor(method, levels=c('10 mkM Des','10 mkM Min',
                                              'His 0.5 mM 10 mkM Des', 'His 0.5 mM 10 mkM Min',
                                              'His 1 mM 10 mkM Des', 'His 1 mM 10 mkM Min',
                                              'Gln 1 mM 10 mkM Des','Gln 1 mM 10 mkM Min',
                                              'Gln 5 mM 10 mkM Des', 'Gln 5 mM 10 mkM Min'))) %>% 
  ggplot(aes(x=graf.order, y=mkM_Cu_per_DW, fill=method))+
  stat_summary(fun.data=mean_se, geom='bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar',width=0.3)+
  labs(x=NULL,y='mkM Cu / грамм сухой массы корня')+
  scale_x_discrete(labels=c('Десорбция','Озоление',
                            'Десорбция','Озоление',
                            'Десорбция','Озоление',
                            'Десорбция','Озоление',
                            'Десорбция','Озоление'))+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 8.5)+
  geom_richtext(data=tibble(x=1.5,y=22), aes(x=x,y=y,label='10 mkM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_richtext(data=tibble(x=3.5,y=22), aes(x=x,y=y,label='10 mkM<br>+His 0.5 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_richtext(data=tibble(x=5.5,y=22), aes(x=x,y=y,label='10 mkM<br>+ His 1 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_richtext(data=tibble(x=7.5,y=22), aes(x=x,y=y,label='10 mkM<br>+ Gln 1 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_richtext(data=tibble(x=9.5,y=22), aes(x=x,y=y,label='10 mkM<br>+ Gln 5 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_text(data=tibble(x=1, y=16.5), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=9), aes(x=x,y=y,label='b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=18),aes(x=x,y=y,label='a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=6), aes(x=x,y=y, label='c'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=16), aes(x=x,y=y,label='a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=6,y=4.8),aes(x=x,y=y,label='c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=7, y=19), aes(x=x,y=y, label='d'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=8, y=6), aes(x=x,y=y,label='c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=9,y=19),aes(x=x,y=y,label='d'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=10,y=2.5),aes(x=x,y=y,label='e'),inherit.aes = FALSE)+
  scale_fill_manual(values=c('#078FF1','#F1071A',
                             '#078FF1','#F1071A',
                             '#078FF1','#F1071A',
                             '#078FF1','#F1071A',
                             '#078FF1','#F1071A'))+
  theme_classic()+
  theme_bw()
  
  
Vicia_Des_VS_Min.100_mkM.graf <- Vicia_Des_VS_Min.100_mkM %>% 
  mutate(graf.order = factor(method, levels=c('100 mkM Des','100 mkM Min',
                                              '100 mkM His 0.5 mM Des', '100 mkM His 0.5 mM Min',
                                              '100 mkM His 1 mM Des', '100 mkM His 1 mM Min',
                                              '100 mkM Gln 1 mM Des','100 mkM Gln 1 mM Min',
                                              '100 mkM Gln 5 mM Des', '100 mkM Gln 5 mM Min'))) %>% 
  ggplot(aes(x=graf.order, y=mkM_Cu_per_DW, fill=graf.order))+
  stat_summary(fun.data=mean_se, geom='bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar',width=0.3)+
  labs(x=NULL,y='mkM Cu / грамм сухой массы корня')+
  scale_x_discrete(labels=c('Десорбция','Озоление',
                            'Десорбция','Озоление',
                            'Десорбция','Озоление',
                            'Десорбция','Озоление',
                            'Десорбция','Озоление'))+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 8.5)+
  geom_richtext(data=tibble(x=1.5,y=145), aes(x=x,y=y,label='100 mkM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_richtext(data=tibble(x=3.5,y=145), aes(x=x,y=y,label='100 mkM<br>+His 0.5 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_richtext(data=tibble(x=5.5,y=145), aes(x=x,y=y,label='100 mkM<br>+ His 1 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_richtext(data=tibble(x=7.5,y=145), aes(x=x,y=y,label='100 mkM<br>+ Gln 1 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_richtext(data=tibble(x=9.5,y=145), aes(x=x,y=y,label='100 mkM<br>+ Gln 5 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA)+
  geom_text(data=tibble(x=1, y=130), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=60), aes(x=x,y=y,label='b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=130),aes(x=x,y=y,label='a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=20), aes(x=x,y=y, label='c'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=100), aes(x=x,y=y,label='d'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=6,y=20),aes(x=x,y=y,label='c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=7, y=110), aes(x=x,y=y, label='d'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=8, y=20), aes(x=x,y=y,label='c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=9,y=110),aes(x=x,y=y,label='d'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=10,y=15),aes(x=x,y=y,label='e'),inherit.aes = FALSE)+
  scale_fill_manual(values=c('#078FF1','#F1071A',
                             '#078FF1','#F1071A',
                             '#078FF1','#F1071A',
                             '#078FF1','#F1071A',
                             '#078FF1','#F1071A'))+
  theme_classic()+
  theme_bw()
 

(Vicia_Des_VS_Min.graf/Vicia_Des_VS_Min.100_mkM.graf)+
  plot_annotation(title='Рисунок 10', subtitle = 'Сравнение данных десорбции КС и озоления корней', tag_levels = 'A')
ggsave('Desorb_VS_Mineralisation.png', height = 10, width=9)


