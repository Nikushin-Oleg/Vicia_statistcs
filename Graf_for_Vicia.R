
library(ggplot2)
library(tidyverse)
library(ggtext)
library(glue)
library(patchwork)
library(RColorBrewer)
library(Richtext)
library(gridtext)
library(janitor)
library(tidymodels)
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

Vicia_Copper_ENDOGEN_CONT_shoot <- Vicia_Copper_ENDOGEN_CONT_shoot %>% 
  mutate(order = factor(Treatment, levels = c('Control', '10 mkM', 'His 0.5  mM', 'His 1  mM', 'Gln 1 mM', 'Gln 5 mM', '50 mkM', '50 mkM Gln 5 mM',
                                              '100 mkM', '100 mkM His 0.5 mM', '100 mkM His 1 mM', '100 mkM Gln 1 mM', '100 mkM Gln 5 mM')))

Vicia_Copper_ENDOGEN_CONT_root.10mkM.SHOOT<- Vicia_Copper_ENDOGEN_CONT_shoot %>% 
  filter(Treatment %in% c('Control', '10 mkM','His 0.5  mM', 'His 1  mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = order, y = OZOL_DW_SHOOT, fill = Treatment))+
  stat_summary(fun.data = mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data=mean_se, geom='errorbar',width=0.5)+
  labs(x=NULL, y = 'мкМ меди / г сухой массы побега')+
  geom_text(data=tibble(x=1, y = 0.43), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y = 0.5), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y = 0.43), aes(x=x,y=y,label = 'a'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y = 0.35), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y = 0.4), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y = 0.35), aes(x=x,y=y,label = 'b'),inherit.aes = FALSE)+
  theme_classic()+
  theme_bw()


(Vicia_Copper_ENDOGEN_CONT_root.10mkM.ROOT | Vicia_Copper_ENDOGEN_CONT_root.10mkM.SHOOT)/
 (Vicia_Copper_DESORBTION_root.10mkM.root | Vicia_Copper_DESORBTION_shoot.10mkM.SHOOT)+
  plot_annotation(tag_level = 'A')

ggsave('Озоление и десорбция ионов меди 10 мкМ (корень - слева, побег - справа).JPG', height = 9, width = 9)  
