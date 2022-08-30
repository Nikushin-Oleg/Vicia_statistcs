library(tidyverse)
library(readxl)


Vicia <- read_excel('Vicia_CW_root_HIS.xlsx', skip = 1)[-c(11:15), -c(1:17,19:21,23)]
Vicia <- Vicia[-c(1),]
colnames(Vicia) <- c('Variant','CW.share.root', 'CW.share.shoot')
#Vicia <- Vicia[-1,]

Vicia$Variant<- c(rep('Control', 9))
Vicia$Variant <- c(rep('Control', 3),rep('100 mkM',3), rep('Gln 1 mM 100 mkM', 3),rep('Gln 5 mM 100 mkM', 3))

                   

Vicia<- type_convert(Vicia)
#Vicia <- Vicia %>% 
  #mutate('CW.share.root' = `DWCW.root`/`DW.root`,
  #'CW.share.shoot' = `DWCW.shoot`/`DW.shoot`)

Vicia_Cell_wall_mass.share <- Vicia
Vicia_Copper_ENDOGEN_CONT_root <- bind_rows(Vicia_Copper_ENDOGEN_CONT_root,Vicia)
#Vicia_pH.root$Variant[19:27] <- c(rep('Tr 10 mkM',3), rep('Tr His 1 mM', 3), rep('Tr Gln 5 mM', 3))

