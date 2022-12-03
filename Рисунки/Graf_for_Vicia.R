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
Vicia_mass.DW.ROOT <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','50 mkM', '100 mkM')) %>% 
  ggplot(aes(x = Order,y = DW.ROOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'DW корня, г')+
  geom_text(data=tibble(x=1, y=0.086), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=0.080), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=0.070), aes(x=x,y=y, label = 'c'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=0.068), aes(x=x,y=y, label = 'd'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()
Vicia_mass.DW.ROOT.10_mkM <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = DW.ROOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'DW корня, г')+
  scale_x_discrete(labels = c('Control', '10 mkM','10 mkM<br> His 0.5 mM', '10 mkM<br> His 1 mM',
                              '10 mkM<br> Gln 1 mM', '10 mkM<br> Gln 5 mM'))+
  geom_text(data=tibble(x=1, y=0.086), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=0.080), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=0.090), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=0.086), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=0.090), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=0.090), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())
  
Vicia_mass.DW.ROOT.100_mkM <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '100 mkM','100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = DW.ROOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'DW корня, г')+
  scale_x_discrete(labels = c('Control', '100 mkM','100 mkM<br> His 0.5 mM', '100 mkM<br> His 1 mM',
                              '100 mkM<br> Gln 1 mM', '100 mkM<br> Gln 5 mM'))+
  geom_text(data=tibble(x=1, y=0.086), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=0.067), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=0.072), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=0.077), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=0.082), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=0.092), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

(Vicia_mass.DW.ROOT/ Vicia_mass.DW.ROOT.10_mkM/ Vicia_mass.DW.ROOT.100_mkM)+
  plot_annotation(subtitle = 'Токсическое действие ионов меди на накопление сухой массы корня', tag_level = 'A')
ggsave('Влияние ионов меди на сухую массу корня.png', height = 9, width = 8.23)

#####################################################
Vicia_mass.DW.SHOOT <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','50 mkM', '100 mkM')) %>% 
  ggplot(aes(x = Order,y = DW.SHOOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'DW побега, г')+
  geom_text(data=tibble(x=1, y=0.225), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=0.225), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=0.215), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=0.21), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()

Vicia_mass.DW.SHOOT.10_mkM <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = DW.SHOOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'DW побега, г')+
  scale_x_discrete(labels = c('Control', '10 mkM','10 mkM<br> His 0.5 mM', '10 mkM<br> His 1 mM',
                              '10 mkM<br> Gln 1 mM', '10 mkM<br> Gln 5 mM'))+
  geom_text(data=tibble(x=1, y=0.225), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=0.225), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=0.262), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=0.25), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=0.240), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=0.23), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

Vicia_mass.DW.SHOOT.100_mkM <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '100 mkM','100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = DW.SHOOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'DW побега, г')+
  scale_x_discrete(labels = c('Control', '100 mkM','100 mkM<br> His 0.5 mM', '100 mkM<br> His 1 mM',
                              '100 mkM<br> Gln 1 mM', '100 mkM<br> Gln 5 mM'))+
  geom_text(data=tibble(x=1, y=0.225), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=0.205), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=0.18), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=0.21), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=0.21), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=0.23), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())


(Vicia_mass.DW.SHOOT/ Vicia_mass.DW.SHOOT.10_mkM/ Vicia_mass.DW.SHOOT.100_mkM)+
  plot_annotation(subtitle = 'Токсическое действие ионов меди на накопление сухой массы побега', tag_level = 'A')
ggsave('Влияние ионов меди на сухую массу побега.png', height = 9, width = 8.23)

Vicia_mass.HYDRATION.ROOT <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','50 mkM', '100 mkM')) %>% 
  ggplot(aes(x = Order,y = HYDRATION.ROOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'г воды /г DW корня')+
  geom_text(data=tibble(x=1, y=16.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=15.5), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=14.0), aes(x=x,y=y, label = 'c'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=14.0), aes(x=x,y=y, label = 'c'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()

Vicia_mass.HYDRATION.ROOT.10_mkM <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = HYDRATION.ROOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'г воды /г DW корня')+
  scale_x_discrete(labels=c('Control', '10 mkM','10 mkM<br>His 0.5 mM', '10 mkM<br>His 1 mM', 
                              '10 mkM<br>Gln 1 mM', '10 mkM<br>Gln 5 mM'))+
  geom_text(data=tibble(x=1, y=16.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=15.5), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=17.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=16.8), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=15.5), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=15.8), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x=element_markdown())

Vicia_mass.HYDRATION.ROOT.100_mkM <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '100 mkM','100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = HYDRATION.ROOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'г воды /г DW корня')+
  scale_x_discrete(labels = c('Control', '100 mkM','100 mkM<br>His 0.5 mM', '100 mkM<br>His 1 mM', 
                              '100 mkM<br>Gln 1 mM', '100 mkM<br>Gln 5 mM'))+
  geom_text(data=tibble(x=1, y=16.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=14.2), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=14.2), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=14.2), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=14.2), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=14.2), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

(Vicia_mass.HYDRATION.ROOT/ Vicia_mass.HYDRATION.ROOT.10_mkM/ Vicia_mass.HYDRATION.ROOT.100_mkM)+
  plot_annotation(subtitle = 'Токсическое действие ионов меди на содержание воды в корнях', tag_level = 'A')
ggsave('Влияние ионов меди на оводненность корня.png', height = 9, width = 8.23)  


Vicia_mass.HYDRATION.SHOOT <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','50 mkM', '100 mkM')) %>% 
  ggplot(aes(x = Order,y = HYDRATION.SHOOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'г воды /г DW побега')+
  geom_text(data=tibble(x=1, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()

Vicia_mass.HYDRATION.SHOOT.10.mkM  <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = HYDRATION.SHOOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'г воды /г DW побега')+
  scale_x_discrete(labels=c('Control', '10 mkM', '10 mkM<br> His 0.5 mM', 
                            '10 mkM<br> His 1 mM', '10 mkM<br> Gln 1 mM', '10 mkM<br> Gln 5 mM'))+
  geom_text(data=tibble(x=1, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

Vicia_mass.HYDRATION.SHOOT.100.mkM  <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '100 mkM','100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = HYDRATION.SHOOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'г воды /г DW побега')+
  scale_x_discrete(labels=c('Control', '100 mkM', '100 mkM<br> His 0.5 mM', 
                            '100 mkM<br> His 1 mM', '100 mkM<br> Gln 1 mM', '100 mkM<br> Gln 5 mM'))+
  geom_text(data=tibble(x=1, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=9.5), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=9.5), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

(Vicia_mass.HYDRATION.SHOOT/ Vicia_mass.HYDRATION.SHOOT.10.mkM/ Vicia_mass.HYDRATION.SHOOT.100.mkM)+
  plot_annotation(subtitle = 'Токсическое действие ионов меди на содержание воды в побегах', tag_level = 'A')
ggsave('Влияние ионов меди на оводненность побега.png', height = 9, width = 8.23)

Vicia_Copper_ENDOGEN_CONT_root <- Vicia_Copper_ENDOGEN_CONT_root %>% 
  mutate(order = factor(Treatment, levels = c('Control', '10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM', '50 mkM', '50 mkM Gln 5 mM',
                                              '100 mkM', 'His 0.5 mM 100 mkM', 'His 1 mM 100 mkM', 'Gln 1 mM 100 mkM', 'Gln 5 mM 100 mkM')))


Vicia_Copper_ENDOGEN_CONT_root.NL<- Vicia_Copper_ENDOGEN_CONT_root %>% 
  filter(Treatment %in% c('Control', '10 mkM','50 mkM', '100 mkM')) %>% 
  ggplot(aes(x = order, y = OZOL_DW_ROOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar',width=0.5)+
  labs(x=NULL, y = 'мкМ меди / г DW корня')+
  geom_text(data=tibble(x=1, y =3), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y = 10), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y = 30), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y = 55), aes(x=x,y=y,label = 'd'),inherit.aes = FALSE)+
  #geom_text(data=tibble(x=5, y = 18), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
  #geom_text(data=tibble(x=6, y = 10), aes(x=x,y=y,label = 'd'),inherit.aes = FALSE)+
  #scale_x_discrete(labels = c('Control', '100 mkM','100 mkM<br>His 0.5 mM', '100 mkM<br>His 1 mM', 
                              #'100 mkM<br>Gln 1 mM', '100 mkM<br>Gln 5 mM'))+
  theme_classic()+
  theme_bw()
  theme(axis.text.x = element_markdown())

  Vicia_Copper_ENDOGEN_CONT_root.10mkM<- Vicia_Copper_ENDOGEN_CONT_root %>% 
    filter(Treatment %in% c('Control', '10 mkM','His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
    ggplot(aes(x = order, y = OZOL_DW_ROOT, fill = Treatment))+
    stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
    stat_summary(fun.data=mean_se, geom='errorbar',width=0.5)+
    labs(x=NULL, y = 'мкМ меди / г DW корня')+
    geom_text(data=tibble(x=1, y =1.8), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=2, y = 8.5), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=3, y = 5), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=4, y = 3.5), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=5, y = 5), aes(x=x,y=y,label = 'd'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=6, y = 2.0), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
    scale_x_discrete(labels = c('Control', '10 mkM','10 mkM<br>His 0.5 mM', '10 mkM<br>His 1 mM', 
                                '10 mkM<br>Gln 1 mM', '10 mkM<br>Gln 5 mM'))+
    theme_classic()+
    theme_bw()+
  theme(axis.text.x = element_markdown())
  
Vicia_Copper_ENDOGEN_CONT_root.100mkM<- Vicia_Copper_ENDOGEN_CONT_root %>% 
    filter(Treatment %in% c('Control', '100 mkM','His 0.5 mM 100 mkM', 'His 1 mM 100 mkM', 'Gln 1 mM 100 mkM', 'Gln 5 mM 100 mkM')) %>% 
    ggplot(aes(x = order, y = OZOL_DW_ROOT, fill = Treatment))+
    stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
    stat_summary(fun.data=mean_se, geom='errorbar',width=0.5)+
    labs(x=NULL, y = 'мкМ меди / г DW корня')+
    geom_text(data=tibble(x=1, y =3), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=2, y = 55), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=3, y = 18), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=4, y = 15), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=5, y = 18), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
    geom_text(data=tibble(x=6, y = 10), aes(x=x,y=y,label = 'd'),inherit.aes = FALSE)+
    scale_x_discrete(labels = c('Control', '10 mkM','10 mkM<br>His 0.5 mM', '10 mkM<br>His 1 mM', 
                                '10 mkM<br>Gln 1 mM', '10 mkM<br>Gln 5 mM'))+
    theme_classic()+
    theme_bw()+
    theme(axis.text.x = element_markdown())

Vicia_Copper_ENDOGEN_CONT_shoot.NL<- Vicia_Copper_ENDOGEN_CONT_shoot %>% 
  filter(Treatment %in% c('Control', '10 mkM','50 mkM', '100 mkM')) %>% 
  ggplot(aes(x = order, y = OZOL_DW_SHOOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar',width=0.5)+
  labs(x=NULL, y = 'мкМ меди / г DW побега')+
  geom_text(data=tibble(x=1, y =0.5), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y = 0.55), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y = 0.98), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y = 0.98), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()
  
Vicia_Copper_ENDOGEN_CONT_shoot.10mkM<- Vicia_Copper_ENDOGEN_CONT_shoot %>% 
  filter(Treatment %in% c('Control', '10 mkM','His 0.5  mM', 'His 1  mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = order, y = OZOL_DW_SHOOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar',width=0.5)+
  labs(x=NULL, y = 'мкМ меди / г DW побега')+
  geom_text(data=tibble(x=1, y =0.45), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y = 0.5), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y = 0.45), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y = 0.35), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y = 0.4), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y = 0.35), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  scale_x_discrete(labels = c('Control', '10 mkM','10 mkM<br>His 0.5 mM', '10 mkM<br>His 1 mM', 
                              '10 mkM<br>Gln 1 mM', '10 mkM<br>Gln 5 mM'))+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

Vicia_Copper_ENDOGEN_CONT_shoot.100mkM<- Vicia_Copper_ENDOGEN_CONT_shoot %>% 
  filter(Treatment %in% c('Control', '100 mkM','His 0.5 mM 100 mkM', 'His 1 mM 100 mkM', 'Gln 1 mM 100 mkM', 'Gln 5 mM 100 mkM')) %>% 
  ggplot(aes(x = order, y = OZOL_DW_SHOOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar',width=0.5)+
  labs(x=NULL, y = 'мкМ меди / г DW побега')+
  geom_text(data=tibble(x=1, y =0.45), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y = 0.95), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y = 1.05), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y = 0.8), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y = 0.60), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y = 0.55), aes(x=x,y=y,label = 'c'),inherit.aes = FALSE)+
  scale_x_discrete(labels = c('Control', '100 mkM','100 mkM<br>His 0.5 mM', '100 mkM<br>His 1 mM', 
                              '100 mkM<br>Gln 1 mM', '100 mkM<br>Gln 5 mM'))+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

(Vicia_Copper_ENDOGEN_CONT_root.NL|Vicia_Copper_ENDOGEN_CONT_shoot.NL)/
(Vicia_Copper_ENDOGEN_CONT_root.10mkM|Vicia_Copper_ENDOGEN_CONT_shoot.10mkM)/
(Vicia_Copper_ENDOGEN_CONT_root.100mkM|Vicia_Copper_ENDOGEN_CONT_shoot.100mkM)+
  plot_annotation(subtitle = 'Содержание ионов меди в тканях корней и побегов', tag_level = 'A')

ggsave('Copper_content_in_roots_and_shoots.png', height = 9, width = 9)

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
  scale_x_discrete(labels=c('10 mkM', '10 mkM<br>His 0.5 mM', '10 mkM<br> His 1 mM',
                            '10 mkM<br> Gln 1 mM', '10 mkM<br> Gln 5 mM'))+
  geom_text(data=tibble(x=1,y=18), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2,y=18), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=18), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4,y=20), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5,y=20), aes(x=x,y=y, label='b'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x=element_markdown())

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
  scale_x_discrete(labels=c('10 mkM', '10 mkM<br>His 0.5 mM', '10 mkM<br> His 1 mM',
                            '10 mkM<br> Gln 1 mM', '10 mkM<br> Gln 5 mM'))+
  geom_text(data=tibble(x=1,y=7), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2,y=6.5), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3,y=6), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4,y=7.8), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5,y=8.5), aes(x=x,y=y, label='a'), inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()+
  theme(axis.text.x = element_markdown())

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
  plot_annotation(subtitle = 'Влияние лигандов на адсорбцию ионов меди изолированными клеточными стенками',tag_levels = 'A')

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
  plot_annotation(subtitle = 'Сравнение данных десорбции КС и озоления корней', tag_levels = 'A')
ggsave('Desorb_VS_Mineralisation.png', height = 10, width=9)

Vicia_Copper_DESORBTION_root_VS_shoot <- Vicia_Copper_DESORBTION_root %>% 
  filter(Treatment%in% c('10 mkM', '100 mkM', '100 mkM Gln 1 mM',
                         '100 mkM Gln 5 mM', '100 mkM His 0.5 mM', '100 mkM His 1 mM', '50 mkM', 
                         'Gln 1 mM', 'Gln 5 mM', 'His 0.5 mM', 'His 1 mM')) %>% 
  mutate(part = factor(Treatment, labels = c('10 mkM.root', '100 mkM.root', '100 mkM Gln 1 mM.root',
                       '100 mkM Gln 5 mM.root', '100 mkM His 0.5 mM.root', '100 mkM His 1 mM.root', '50 mkM.root', 
                       'Gln 1 mM.root', 'Gln 5 mM.root', 'His 0.5 mM.root', 'His 1 mM.root'))) %>% 
  select(1:4,6)

Vicia_Copper_DESORBTION_root_VS_shoot.2 <- Vicia_Copper_DESORBTION_shoot %>% 
  filter(Treatment%in% c('10 mkM', '100 mkM', '100 mkM Gln 1 mM',
                         '100 mkM Gln 5 mM', '100 mkM His 0.5 mM', '100 mkM His 1 mM', '50 mkM', 
                         'Gln 1 mM', 'Gln 5 mM', 'His 0.5 mM', 'His 1 mM')) %>% 
  mutate(part = factor(Treatment, labels = c('10 mkM.shoot', '100 mkM.shoot', '100 mkM Gln 1 mM.shoot',
                                             '100 mkM Gln 5 mM.shoot', '100 mkM His 0.5 mM.shoot', '100 mkM His 1 mM.shoot', '50 mkM.shoot', 
                                             'Gln 1 mM.shoot', 'Gln 5 mM.shoot', 'His 0.5 mM.shoot', 'His 1 mM.shoot'))) %>% 
  select(1:4,6)
colnames(Vicia_Copper_DESORBTION_root_VS_shoot.2) <- c('Treatment', 'mkM_Cu_per_FW', 'mkM_Cu_per_DW','mkM_Cu_per_DWCW', 'part')
Vicia_Copper_DESORBTION_root_VS_shoot <- bind_rows(Vicia_Copper_DESORBTION_root_VS_shoot,Vicia_Copper_DESORBTION_root_VS_shoot.2)

Vicia_Copper_DESORBTION_root_VS_shoot.1 <- Vicia_Copper_DESORBTION_root_VS_shoot %>% 
  mutate(graphic.order = factor(part, levels = c('10 mkM.root','10 mkM.shoot',
                                                 '50 mkM.root', '50 mkM.shoot',
                                                 '100 mkM.root', '100 mkM.shoot',
                                                 '100 mkM Gln 1 mM.root','100 mkM Gln 1 mM.shoot',
                                                 '100 mkM Gln 5 mM.root', '100 mkM Gln 5 mM.shoot',
                                                 '100 mkM His 0.5 mM.root', '100 mkM His 0.5 mM.shoot',
                                                 '100 mkM His 1 mM.root','100 mkM His 1 mM.shoot',
                                                 'Gln 1 mM.root', 'Gln 1 mM.shoot',
                                                 'Gln 5 mM.root','Gln 5 mM.shoot',
                                                 'His 0.5 mM.root','His 0.5 mM.shoot',
                                                 'His 1 mM.root', 'His 1 mM.shoot'))) %>% 
  filter(Treatment%in% c('10 mkM', '50 mkM', '100 mkM')) %>% 
  ggplot(aes(x=graphic.order, y=mkM_Cu_per_DWCW, fill=graphic.order))+
  stat_summary(fun.data=mean_se, geom='bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar', width=0.3)+
  labs(x=NULL, y='mkM Cu / г сухой массы КС')+
  scale_fill_manual(values = c('#078FF1','#F1071A',
                               '#078FF1','#F1071A',
                               '#078FF1','#F1071A'))+
  geom_text(data=tibble(x=1,y=50), aes(x=x,y=y,label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=2,y=25), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=3,y=175), aes(x=x,y=y,label='c'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=4,y=75), aes(x=x,y=y,label='d'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=5,y=260), aes(x=x,y=y,label='e'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=6,y=125), aes(x=x,y=y,label='f'), inherit.aes = FALSE, size=5)+
  geom_richtext(data=tibble(x=1.5,y=280), aes(x=x,y=y,label='10 mkM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=3.5,y=280), aes(x=x,y=y,label='50 mkM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=5.5,y=280), aes(x=x,y=y,label='100 mkM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  scale_x_discrete(labels = c('КС корней', 'КС побег',
                              'КС корней', 'КС побег',
                              'КС корней', 'КС побег'))+
  theme_classic()+
  theme_bw()


Vicia_Copper_DESORBTION_root_VS_shoot.2<- Vicia_Copper_DESORBTION_root_VS_shoot %>% 
  mutate(graphic.order = factor(part, levels = c('10 mkM.root','10 mkM.shoot',
                                                 '50 mkM.root', '50 mkM.shoot',
                                                 '100 mkM.root', '100 mkM.shoot',
                                                 '100 mkM His 0.5 mM.root', '100 mkM His 0.5 mM.shoot',
                                                 '100 mkM His 1 mM.root','100 mkM His 1 mM.shoot',
                                                 '100 mkM Gln 1 mM.root','100 mkM Gln 1 mM.shoot',
                                                 '100 mkM Gln 5 mM.root', '100 mkM Gln 5 mM.shoot',
                                                 'His 0.5 mM.root','His 0.5 mM.shoot',
                                                 'His 1 mM.root', 'His 1 mM.shoot',
                                                 'Gln 1 mM.root', 'Gln 1 mM.shoot',
                                                 'Gln 5 mM.root','Gln 5 mM.shoot'))) %>% 
  filter(Treatment%in% c('10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x=graphic.order, y=mkM_Cu_per_DWCW, fill=graphic.order))+
  stat_summary(fun.data=mean_se, geom='bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar', width=0.3)+
  labs(x=NULL, y='mkM Cu / г сухой массы КС')+
  scale_fill_manual(values = c('#078FF1','#F1071A',
                               '#078FF1','#F1071A',
                               '#078FF1','#F1071A',
                               '#078FF1','#F1071A',
                               '#078FF1','#F1071A'))+
  geom_text(data=tibble(x=1,y=40), aes(x=x,y=y,label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=2,y=17), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=3,y=40), aes(x=x,y=y,label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=4,y=17), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=5,y=40), aes(x=x,y=y,label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=6,y=17), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=7,y=45), aes(x=x,y=y,label='c'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=8,y=18), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=9,y=42.5), aes(x=x,y=y,label='c'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=10,y=20), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_richtext(data=tibble(x=1.5,y=47), aes(x=x,y=y,label='10 mkM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=3.5,y=47), aes(x=x,y=y,label='10 mkM<br> +His 0.5 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=5.5,y=47), aes(x=x,y=y,label='10 mkM<br> +His 1 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=7.5,y=47), aes(x=x,y=y,label='10 mkM<br> +Gln 1 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=9.5,y=47), aes(x=x,y=y,label='10 mkM<br> +Gln 5 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  scale_x_discrete(labels = c('КС корней', 'КС побег',
                              'КС корней', 'КС побег',
                              'КС корней', 'КС побег',
                              'КС корней', 'КС побег',
                              'КС корней', 'КС побег'))+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 8.5)+
  theme_classic()+
  theme_bw()

Vicia_Copper_DESORBTION_root_VS_shoot.1
Vicia_Copper_DESORBTION_root_VS_shoot.2

Vicia_Copper_DESORBTION_root_VS_shoot.3<- Vicia_Copper_DESORBTION_root_VS_shoot %>% 
  mutate(graphic.order = factor(part, levels = c('10 mkM.root','10 mkM.shoot',
                                                 '50 mkM.root', '50 mkM.shoot',
                                                 '100 mkM.root', '100 mkM.shoot',
                                                 '100 mkM His 0.5 mM.root', '100 mkM His 0.5 mM.shoot',
                                                 '100 mkM His 1 mM.root','100 mkM His 1 mM.shoot',
                                                 '100 mkM Gln 1 mM.root','100 mkM Gln 1 mM.shoot',
                                                 '100 mkM Gln 5 mM.root', '100 mkM Gln 5 mM.shoot',
                                                 'His 0.5 mM.root','His 0.5 mM.shoot',
                                                 'His 1 mM.root', 'His 1 mM.shoot',
                                                 'Gln 1 mM.root', 'Gln 1 mM.shoot',
                                                 'Gln 5 mM.root','Gln 5 mM.shoot'))) %>% 
  filter(Treatment%in% c('100 mkM', '100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')) %>% 
  ggplot(aes(x=graphic.order, y=mkM_Cu_per_DWCW, fill=graphic.order))+
  stat_summary(fun.data=mean_se, geom='bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar', width=0.3)+
  labs(x=NULL, y='mkM Cu / г сухой массы КС')+
  scale_fill_manual(values = c('#078FF1','#F1071A',
                               '#078FF1','#F1071A',
                               '#078FF1','#F1071A',
                               '#078FF1','#F1071A',
                               '#078FF1','#F1071A'))+
  geom_text(data=tibble(x=1,y=260), aes(x=x,y=y,label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=2,y=125), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=3,y=260), aes(x=x,y=y,label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=4,y=135), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=5,y=225), aes(x=x,y=y,label='c'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=6,y=125), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=7,y=225), aes(x=x,y=y,label='c'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=8,y=125), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=9,y=225), aes(x=x,y=y,label='c'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=10,y=125), aes(x=x,y=y,label='b'), inherit.aes = FALSE, size=5)+
  geom_richtext(data=tibble(x=1.5,y=270), aes(x=x,y=y,label='100 mkM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=3.5,y=270), aes(x=x,y=y,label='100 mkM<br> +His 0.5 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=5.5,y=270), aes(x=x,y=y,label='100 mkM<br> +His 1 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=7.5,y=270), aes(x=x,y=y,label='100 mkM<br> +Gln 1 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  geom_richtext(data=tibble(x=9.5,y=270), aes(x=x,y=y,label='100 mkM<br> +Gln 5 mM'), 
                inherit.aes = FALSE, label.colour=NA,fill=NA,size=5)+
  scale_x_discrete(labels = c('КС корней', 'КС побег',
                              'КС корней', 'КС побег',
                              'КС корней', 'КС побег',
                              'КС корней', 'КС побег',
                              'КС корней', 'КС побег'))+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 8.5)+
  theme_classic()+
  theme_bw()

(Vicia_Copper_DESORBTION_root_VS_shoot.1/
Vicia_Copper_DESORBTION_root_VS_shoot.2/
Vicia_Copper_DESORBTION_root_VS_shoot.3)
Vicia_Copper_DESORBTION_root_VS_shoot.1+
  plot_annotation(subtitle = 'Сравнение адсобционных способностей клеточных стенок корней и побегов в отсутвии лигандов')
ggsave('Fig.11_Adsorbtion_CW.ROOT_CW.SHOOT_ligand_free.png', height = 8, width = 8)

Vicia_Copper_DESORBTION_root_VS_shoot.2+
  plot_annotation(subtitle = 'Сравнение адсобционных способностей клеточных стенок корней и побегов при 10 mkM меди в среде')
ggsave('Fig.12_Adsorbtion_CW.ROOT_CW.SHOOT_10_mkM.png', height = 8, width = 8)

Vicia_Copper_DESORBTION_root_VS_shoot.3+
  plot_annotation(subtitle = 'Сравнение адсобционных способностей клеточных стенок корней и побегов при 100 mkM меди в среде')
ggsave('Fig.13_Adsorbtion_CW.ROOT_CW.SHOOT_100_mkM.png', height = 8, width = 8)


Vicia_Copper_DESORBTION_root %>% 
  select(1:4) %>% 
  filter(Treatment%in%c('10 mkM', 'His 1 mM', 'Gln 5 mM', 'Tr 10 mkM', 'Tr His 1 mM', 'Tr Gln 5 mM')) %>% 
  mutate(order = factor(Treatment, levels = c('10 mkM', 'Tr 10 mkM',
                                              'His 1 mM', 'Tr His 1 mM', 
                                              'Gln 5 mM', 'Tr Gln 5 mM'))) %>% 
  ggplot(aes(x=order, y=DESORB_DWCW_ROOT, fill=order))+
  stat_summary(fun.data=mean_se, geom='bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.3)+
  scale_fill_manual(values=c('#10B2F3', '#F35110', 
                             '#10B2F3', '#F35110',
                             '#10B2F3', '#F35110'))+
  labs(x=NULL, y='mkM меди/ г DW КС корней')+
  scale_x_discrete(labels=c('Контроль', 'Опыт',
                            'Контроль', 'Опыт',
                            'Контроль', 'Опыт'))+
  geom_text(data=tibble(x=1,y=38),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=2,y=46),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=3,y=38),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=4,y=42),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=5,y=43.5),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=6,y=42),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_richtext(data=tibble(x=1.5,y=50), aes(x=x,y=y,label='10 mkM'), inherit.aes = FALSE, label.colour=NA, fill=NA, size=5)+
  geom_richtext(data=tibble(x=3.5,y=50), aes(x=x,y=y,label='10 mkM<br>+ His 1 mM'), inherit.aes = FALSE, label.colour=NA, fill=NA, size=5)+
  geom_richtext(data=tibble(x=5.5,y=50), aes(x=x,y=y,label='10 mkM<br>+ Gln 5 mM'), inherit.aes = FALSE, label.colour=NA, fill=NA, size=5)+
  theme_classic()+
  theme_bw()+
  plot_annotation(subtitle = 'Сравнение адсобционных способностей КС контрольных и опытных корней')
ggsave('Adsorbtion_CW_Control_VS_Tr.png', height = 8, width=8)


Vicia_Copper_DESORBTION_shoot %>% 
  select(1:4) %>% 
  filter(Treatment%in%c('10 mkM', 'His 1 mM', 'Gln 5 mM', 'Tr 10 mkM', 'Tr His 1 mM', 'Tr Gln 5 mM')) %>% 
  mutate(order = factor(Treatment, levels = c('10 mkM', 'Tr 10 mkM',
                                              'His 1 mM', 'Tr His 1 mM', 
                                              'Gln 5 mM', 'Tr Gln 5 mM'))) %>% 
  ggplot(aes(x=order, y=DESORB_DWCW_SHOOT, fill=order))+
  stat_summary(fun.data=mean_se, geom='bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.3)+
  scale_fill_manual(values=c('#10B2F3', '#F35110', 
                             '#10B2F3', '#F35110',
                             '#10B2F3', '#F35110'))+
  labs(x=NULL, y='mkM меди/ г DW КС побегов')+
  scale_x_discrete(labels=c('Контроль', 'Опыт',
                            'Контроль', 'Опыт',
                            'Контроль', 'Опыт'))+
  geom_text(data=tibble(x=1,y=16),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=2,y=11),aes(x=x,y=y, label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=3,y=16),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=4,y=9.5),aes(x=x,y=y, label='b'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=5,y=17.5),aes(x=x,y=y, label='a'), inherit.aes = FALSE, size=5)+
  geom_text(data=tibble(x=6,y=10),aes(x=x,y=y, label='b'), inherit.aes = FALSE, size=5)+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_richtext(data=tibble(x=1.5,y=19), aes(x=x,y=y,label='10 mkM'), inherit.aes = FALSE, label.colour=NA, fill=NA, size=5)+
  geom_richtext(data=tibble(x=3.5,y=19), aes(x=x,y=y,label='10 mkM<br>+ His 1 mM'), inherit.aes = FALSE, label.colour=NA, fill=NA, size=5)+
  geom_richtext(data=tibble(x=5.5,y=19), aes(x=x,y=y,label='10 mkM<br>+ Gln 5 mM'), inherit.aes = FALSE, label.colour=NA, fill=NA, size=5)+
  theme_classic()+
  theme_bw()+
  plot_annotation(subtitle = 'Сравнение адсобционных способностей КС контрольных и опытных побегов')
ggsave('Adsorbtion_CW_Control_VS_Tr.shoot.png', height = 7.5, width=6)

Vicia_Copper_by_solution
Vicia_Copper_ENDOGEN_CONT_root

colnames(Vicia_Copper_ENDOGEN_CONT_root_VS_solution) <- c('Treatment', 'mkM_Cu_per_FW', 'mkM_Cu_per_DW', 'Method')
Vicia_Copper_ENDOGEN_CONT_root_VS_solution <- Vicia_Copper_ENDOGEN_CONT_root %>% 
  select(1:3) %>% 
  arrange(Treatment) %>% 
  mutate(Method = c(rep('10 mkM Ozol', 18),
                    rep('100 mkM Ozol', 12),
                    rep('50 mkM Ozol', 3),
                    rep('Control Ozol', 21),
                    rep('Gln 1 mM Ozol', 9),
                    rep('Gln 1 mM 100 mkM Ozol', 6),
                    rep('Gln 5 mM Ozol', 9),
                    rep('Gln 5 mM 100 mkM Ozol', 6),
                    rep('Gln 5 mM 50 mkM Ozol', 3),
                    rep('His 0.5 mM Ozol', 3),
                    rep('His 0.5 mM 100 mkM Ozol', 6),
                    rep('His 1 mM Ozol', 3),
                    rep('His 1 mM 100 mkM Ozol', 6)))

Vicia_Copper_ENDOGEN_CONT_root_VS_solution <- bind_rows(Vicia_Copper_by_solution, Vicia_Copper_ENDOGEN_CONT_root_VS_solution)

Vicia_Copper_ENDOGEN_CONT_root_VS_solution %>% 
  filter(Treatment%in%c('10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  mutate(order = factor(Method, levels = c('10 mkM IP', '10 mkM Ozol',
                                  'His 0.5 mM IP', 'His 0.5 mM Ozol',
                                  'His 1 mM IP', 'His 1 mM Ozol',
                                  'Gln 1 mM IP', 'Gln 1 mM Ozol',
                                  'Gln 5 mM IP', 'Gln 5 mM Ozol'))) %>% 
  ggplot(aes(x=order, y=mkM_Cu_per_DW, fill = order))+
  stat_summary(fun.data=mean_se, geom ='bar', show.legend=FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar', width=0.3)+
  labs(x=NULL, y='mkM меди / г DW корня')+
  scale_x_discrete(labels=c('По раствору', 'Озоление',
                            'По раствору', 'Озоление',
                            'По раствору', 'Озоление',
                            'По раствору', 'Озоление',
                            'По раствору', 'Озоление'))+
  scale_fill_manual(values=c('#10B2F3', '#F35110', 
                            '#10B2F3', '#F35110',
                            '#10B2F3', '#F35110',
                            '#10B2F3', '#F35110',
                            '#10B2F3', '#F35110'))+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 8.5)+
  geom_text(data=tibble(x=1, y=17.5), aes(x=x, y=y, label= 'a'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=2, y=9), aes(x=x, y=y, label= 'b'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=3, y=17.7), aes(x=x, y=y, label= 'a'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=4, y=5), aes(x=x, y=y, label= 'c'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=5, y=15), aes(x=x, y=y, label= 'd'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=6, y=4), aes(x=x, y=y, label= 'c'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=7, y=11.5), aes(x=x, y=y, label= 'e'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=8, y=5), aes(x=x, y=y, label= 'f'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=9, y=7.8), aes(x=x, y=y, label= 'g'), inherit.aes = FALSE,size=5)+
  geom_text(data=tibble(x=10, y=2.5), aes(x=x, y=y, label= 'h'), inherit.aes = FALSE,size=5)+
  geom_richtext(data=tibble(x=1.5, y=19), aes(x=x,y=y,label='10 mkM'),inherit.aes = FALSE, label.colour=NA, fill=NA, size= 5)+
  geom_richtext(data=tibble(x=3.5, y=19), aes(x=x,y=y,label='10 mkM<br> + His 0.5 mM'),inherit.aes = FALSE, label.colour=NA, fill=NA, size= 5)+
  geom_richtext(data=tibble(x=5.5, y=19), aes(x=x,y=y,label='10 mkM<br> + His 1 mM'),inherit.aes = FALSE, label.colour=NA, fill=NA, size= 5)+
  geom_richtext(data=tibble(x=7.5, y=19), aes(x=x,y=y,label='10 mkM<br> + Gln 1 mM'),inherit.aes = FALSE, label.colour=NA, fill=NA, size= 5)+
  geom_richtext(data=tibble(x=9.5, y=19), aes(x=x,y=y,label='10 mkM<br> + Gln 5 mM'),inherit.aes = FALSE, label.colour=NA, fill=NA, size= 5)+
  theme_classic()+
  theme_bw()+
  plot_annotation(subtitle = 'Сравнение концентрации меди в корнях с убылью металла из расторов после обработки растений')
ggsave('Mineralisation_VS_Solutions.png', height = 6, width = 8)
