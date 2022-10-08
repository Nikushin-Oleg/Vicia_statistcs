
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
Vicia_mass_10_mkM <- Vicia_mass %>% 
  filter(Treatment %in% c('Control', '10 mkM','His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  ggplot(aes(x = Order,y = DW.ROOT, fill = Order))+
  stat_summary(fun.data=mean_se, geom = 'bar', show.legend = FALSE)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.5)+
  labs(x = NULL, y = 'Сухая масса корня, г')+
  geom_text(data=tibble(x=1, y=0.086), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=2, y=0.078), aes(x=x,y=y, label = 'b'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=3, y=0.088), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=4, y=0.086), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=5, y=0.088), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  geom_text(data=tibble(x=6, y=0.092), aes(x=x,y=y, label = 'a'), inherit.aes = FALSE)+
  theme_classic()

(Vicia_mass_Ligand_free/ Vicia_mass_10_mkM / Vicia_mass_100_mkM)+
  plot_annotation(title = 'Рисунок 1', subtitle = 'Влияние ионов меди на накопление сухой массы корня', tag_level = 'A')
ggsave('Влияние ионов меди на накопление сухой массы корня.png', height = 8.5, width = 8)  


umap_rec <- Vicia_mass %>% 
  filter(Treatment %in% c('Control','10 mkM', 'His 0.5 mM', 'His 1 mM', 'Gln 1 mM', 'Gln 5 mM')) %>% 
  select(Treatment, FW.ROOT, DW.ROOT, FW.SHOOT,DW.SHOOT) %>% 
  recipe(~.) %>%
  update_role(Treatment, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec)

juice(umap_prep) %>%
  ggplot(aes(UMAP1, UMAP2, label = Treatment)) +
  geom_point(aes(color = Treatment), alpha = 0.7, size = 4) +
  labs(color = NULL)+
  theme_classic()
