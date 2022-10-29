library(tidyverse)
library(readxl)


Vicia <- read_excel('Vicia_CW_share.xlsx', skip = 1)
Vicia <- Vicia %>% 
  select(Treament,CW.SHARE.ROOT, CW.SHARE.SHOOT) %>% 
  slice(1:20)
colnames(Vicia) <- c('Treatment','CW_share_ROOT', 'CW_share_SHOOT')
#Vicia <- Vicia[-1,]

Vicia$Variant<- c(rep('Control', 9))
Vicia$Variant <- c(rep('Control', 3),rep('100 mkM',3), rep('Gln 1 mM 100 mkM', 3),rep('Gln 5 mM 100 mkM', 3))

Vicia_Cell_wall_mass.share <- Vicia_Cell_wall_mass.share %>% 
  arrange(Treatment)

Vicia<- type_convert(Vicia)
#Vicia <- Vicia %>% 
  #mutate('CW.share.root' = `DWCW.root`/`DW.root`,
  #'CW.share.shoot' = `DWCW.shoot`/`DW.shoot`)

Vicia_Cell_wall_mass.share <- bind_rows(Vicia_Cell_wall_mass.share, Vicia)
Vicia_Copper_ENDOGEN_CONT_root <- bind_rows(Vicia_Copper_ENDOGEN_CONT_root,Vicia)
#Vicia_pH.root$Variant[19:27] <- c(rep('Tr 10 mkM',3), rep('Tr His 1 mM', 3), rep('Tr Gln 5 mM', 3))

