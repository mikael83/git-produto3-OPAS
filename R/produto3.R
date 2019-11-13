############ Produto 3 ###########################
##################################################
###### script desenvolvido por Mikael Lemos ######
###### vers?o 1.0 - 24.10.2019 ###################
##################################################

######
### Carregando / instalando pacotes
######

#install.packages("RColorBrewer")

library('RColorBrewer')

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

# install.packages("tidyverse")
library("tidyverse")

# install.packages("lubridate")
library("lubridate")

#install.packages("Amelia")
library("Amelia")

#install.packages('Rtools')
#library('Rtools')

#install.packages('microbenchmark')
library("microbenchmark")

#install.packages('ggplot2movies')
library("ggplot2movies")

#install.packages('profvis')
library("profvis")

#install.packages('Rcpp')
library("Rcpp")

#install.packages('compiler')
library("compiler")

#install.packages('memoise')
library("memoise")

#install.packages('DiagrammeR')
library("DiagrammeR")

#install.packages('rio')
library("rio")

#install.packages('readr')
library("readr")

#install.packages('data.table')
library("data.table")

#install.packages('feather')
library("feather")

#install.packages('WDI')
library("WDI")

#install.packages('eeptools')
library("eeptools")

#install.packages('compareDF')

library("compareDF")

library("read.dbc")

library("ggplot2")

######
###  carregando tabelas
######

#### MS  ####

## APAC ##

### PR

APAC_PR <- read.csv("/Users/mikael.lemos/Downloads/APAC_PR_BDcompleto.csv")

### PO

APAC_PO <- read.csv("/Users/mikael.lemos/Downloads/APAC_PO_BDcompleto.csv")


## AIH ##

### PR

AIH_PR <- read.csv("/Users/mikael.lemos/Downloads/AIH_PR_BDcompleto.csv")

### PO

AIH_PO <- read.csv("/Users/mikael.lemos/Downloads/AIH_PO_BDcompleto.csv")


## BPAI ##

### PR

BPAI_PR <- read.csv("/Users/mikael.lemos/Downloads/BPAI_PR_BDcompleto.csv")

### PO

BPAI_PO <- read.csv("/Users/mikael.lemos/Downloads/BPAI_PO_BDcompleto.csv")


## SIM ##

### PR

SIM_PR <- read.csv("/Users/mikael.lemos/Downloads/SIM_PR_BDcompleto.csv")

### PO

SIM_PO <- read.csv("/Users/mikael.lemos/Downloads/SIM_PO_BDcompleto.csv")


#### Separar colunas de procedimentos e CIDs

## APAC ##

APAC_PO$CID <- as.character(APAC_PO$CID)

APAC_PO$PROCEDIMENTO <- as.character(APAC_PO$PROCEDIMENTO)

APAC_PO <- separate(APAC_PO, PROCEDIMENTO, into = c( "PROCEDIMENTO1" , "PROCEDIMENTO2"))

APAC_PO <- separate(APAC_PO, CID, into = c( "CID1" , "CID2","CID3"))

APAC_PR$CID <- as.character(APAC_PR$CID)

APAC_PR$PROCEDIMENTO <- as.character(APAC_PR$PROCEDIMENTO)

APAC_PR <- separate(APAC_PR, PROCEDIMENTO, into = c( "PROCEDIMENTO1" , "PROCEDIMENTO2"))

APAC_PR <- separate(APAC_PR, CID, into = c( "CID1" , "CID2", "CID3"))

## AIH ##

AIH_PO$CID <- as.character(AIH_PO$CID)

AIH_PO$PROCEDIMENTO <- as.character(AIH_PO$PROCEDIMENTO)

AIH_PO <- separate(AIH_PO, PROCEDIMENTO, into = c( "PROCEDIMENTO1" , "PROCEDIMENTO2", "PROCEDIMENTO3"))

AIH_PO <- separate(AIH_PO, CID, into = c( "CID1" , "CID2", "CID3", "CID4", "CID5", "CID6", "CID7", "CID8", "CID9", "CID10", "CID11","CID12"))

AIH_PR$CID <- as.character(AIH_PR$CID)

AIH_PR$PROCEDIMENTO <- as.character(AIH_PR$PROCEDIMENTO)

AIH_PR <- separate(AIH_PR, PROCEDIMENTO, into = c( "PROCEDIMENTO1" , "PROCEDIMENTO2", "PROCEDIMENTO3"))

AIH_PR <- separate(AIH_PR, CID, into = c( "CID1" , "CID2", "CID3", "CID4", "CID5", "CID6", "CID7", "CID8", "CID9", "CID10", "CID11", "CID12"))

## BPAI ##

BPAI_PO$CID <- as.character(BPAI_PO$CID)

BPAI_PO$PROCEDIMENTO <- as.character(BPAI_PO$PROCEDIMENTO)

BPAI_PR$CID <- as.character(BPAI_PR$CID)

BPAI_PR$PROCEDIMENTO <- as.character(BPAI_PR$PROCEDIMENTO)

## SIM ##

SIM_PO$CID <- as.character(SIM_PO$CID)

SIM_PO$PROCEDIMENTO <- as.character(SIM_PO$PROCEDIMENTO)

SIM_PO <- separate(SIM_PO, CID, into = c( "CID1" , "CID2", "CID3", "CID4", "CID5", "CID6", "CID7", "CID8", "CID9", "CID10", "CID11", "CID12", "CID13", "CID14", "CID15", "CID16", "CID17", "CID18", "CID19", "CID20", "CID21", "CID22", "CID23", "CID24", "CID25", "CID26"))

SIM_PR$CID <- as.character(SIM_PR$CID)

SIM_PR$PROCEDIMENTO <- as.character(SIM_PR$PROCEDIMENTO)

SIM_PR <- separate(SIM_PR, CID, into = c( "CID1" , "CID2", "CID3", "CID4", "CID5", "CID6", "CID7", "CID8", "CID9", "CID10", "CID11", "CID12", "CID13", "CID14", "CID15", "CID16", "CID17", "CID18", "CID19", "CID20", "CID21", "CID22", "CID23", "CID24", "CID25", "CID26"))


#### Filtragens de agravos ####

#### hep c ####

## APAC ##

APAC_PO_hepc <- filter(APAC_PO, CID1== "B171" | CID2== "B171" | CID3=="B171" | CID1== "B182" | CID2== "B182" | CID3=="B182" | PROCEDIMENTO1== "0604760019"| PROCEDIMENTO1== "0604760027" | PROCEDIMENTO1== "0604760035" | PROCEDIMENTO1== "0604640030" | PROCEDIMENTO1== "0604760043"  | PROCEDIMENTO1== "0604450010"| PROCEDIMENTO2== "0604760019"| PROCEDIMENTO2== "0604760027" | PROCEDIMENTO2== "0604760035" | PROCEDIMENTO2== "0604640030" | PROCEDIMENTO2== "0604760043"  | PROCEDIMENTO2== "0604450010" )

APAC_PR_hepc <- filter(APAC_PR, CID1== "B171" | CID2== "B171" | CID3=="B171" | CID1== "B182" | CID2== "B182" | CID3=="B182" | PROCEDIMENTO1== "0604760019"| PROCEDIMENTO1== "0604760027" | PROCEDIMENTO1== "0604760035" | PROCEDIMENTO1== "0604640030" | PROCEDIMENTO1== "0604760043"  | PROCEDIMENTO1== "0604450010"| PROCEDIMENTO2== "0604760019"| PROCEDIMENTO2== "0604760027" | PROCEDIMENTO2== "0604760035" | PROCEDIMENTO2== "0604640030" | PROCEDIMENTO2== "0604760043"  | PROCEDIMENTO2== "0604450010" )

APAC_hepc <- do.call("rbind", list(APAC_PO_hepc, APAC_PR_hepc))

APAC_hepc <- distinct(APAC_hepc)

## AIH ##

AIH_PO_hepc <- filter(AIH_PO, CID1== "B171" | CID2== "B171" | CID3=="B171" | CID4=="B171" | CID5=="B171" | CID6=="B171" | CID7=="B171" | CID8=="B171" | CID9=="B171"| CID10=="B171" | CID11=="B171" | CID12=="B171" | CID1== "B182" | CID2== "B182" | CID3=="B182"  | CID4=="B182"  | CID5=="B182"  | CID6=="B182"  | CID7=="B182"  | CID8=="B182"  | CID9=="B182"  | CID10=="B182"  | CID11=="B182"  | CID12=="B182"  |  PROCEDIMENTO1== "0604760019"| PROCEDIMENTO1== "0604760027" | PROCEDIMENTO1== "0604760035" | PROCEDIMENTO1== "0604640030" | PROCEDIMENTO1== "0604760043"  | PROCEDIMENTO1== "0604450010"| PROCEDIMENTO2== "0604760019"| PROCEDIMENTO2== "0604760027" | PROCEDIMENTO2== "0604760035" | PROCEDIMENTO2== "0604640030" | PROCEDIMENTO2== "0604760043"  | PROCEDIMENTO2== "0604450010" |  PROCEDIMENTO3== "0604760019" | PROCEDIMENTO3== "0604760027" | PROCEDIMENTO3== "0604760035" | PROCEDIMENTO3== "0604640030" | PROCEDIMENTO3== "0604760043"  | PROCEDIMENTO3== "0604450010" )

AIH_PR_hepc <-  filter(AIH_PR, CID1== "B171" | CID2== "B171" | CID3=="B171" | CID4=="B171" | CID5=="B171" | CID6=="B171" | CID7=="B171" | CID8=="B171" | CID9=="B171"| CID10=="B171" | CID11=="B171" | CID12=="B171" | CID1== "B182" | CID2== "B182" | CID3=="B182"  | CID4=="B182"  | CID5=="B182"  | CID6=="B182"  | CID7=="B182"  | CID8=="B182"  | CID9=="B182"  | CID10=="B182"  | CID11=="B182"  | CID12=="B182"  |  PROCEDIMENTO1== "0604760019"| PROCEDIMENTO1== "0604760027" | PROCEDIMENTO1== "0604760035" | PROCEDIMENTO1== "0604640030" | PROCEDIMENTO1== "0604760043"  | PROCEDIMENTO1== "0604450010"| PROCEDIMENTO2== "0604760019"| PROCEDIMENTO2== "0604760027" | PROCEDIMENTO2== "0604760035" | PROCEDIMENTO2== "0604640030" | PROCEDIMENTO2== "0604760043"  | PROCEDIMENTO2== "0604450010" |  PROCEDIMENTO3== "0604760019" | PROCEDIMENTO3== "0604760027" | PROCEDIMENTO3== "0604760035" | PROCEDIMENTO3== "0604640030" | PROCEDIMENTO3== "0604760043"  | PROCEDIMENTO3== "0604450010" )

AIH_hepc <- do.call("rbind", list(AIH_PO_hepc, AIH_PR_hepc))

AIH_hepc <- distinct(AIH_hepc)

## BPAI ##

BPAI_PO_hepc <- filter(BPAI_PO, CID== "B171" | CID=="B182" | PROCEDIMENTO== "0604760019"| PROCEDIMENTO== "0604760027" | PROCEDIMENTO== "0604760035" | PROCEDIMENTO== "0604640030" | PROCEDIMENTO== "0604760043"  | PROCEDIMENTO== "0604450010" )

BPAI_PR_hepc <- filter(BPAI_PR, CID== "B171" | CID=="B182" | PROCEDIMENTO== "0604760019"| PROCEDIMENTO== "0604760027" | PROCEDIMENTO== "0604760035" | PROCEDIMENTO== "0604640030" | PROCEDIMENTO== "0604760043"  | PROCEDIMENTO== "0604450010" )

## SIM ##

SIM_PO_hepc <- filter(SIM_PO, CID1== "B171" | CID2== "B171" | CID3=="B171" | CID4=="B171" | CID5=="B171" | CID6=="B171" | CID7=="B171" | CID8=="B171" | CID9=="B171"| CID10=="B171" | CID11=="B171" | CID12=="B171" | CID13=="B171" | CID14=="B171" | CID15=="B171" | CID16=="B171" | CID17=="B171" | CID18=="B171" | CID19=="B171" | CID20=="B171" | CID21=="B171" | CID22=="B171" | CID23=="B171" | CID24=="B171" | CID25=="B171" | CID26=="B171" | CID1== "B182" | CID2== "B182" | CID3=="B182"  | CID4=="B182"  | CID5=="B182"  | CID6=="B182"  | CID7=="B182"  | CID8=="B182"  | CID9=="B182"  | CID10=="B182"  | CID11=="B182"  | CID12=="B182"  | CID13=="B182"  | CID14=="B182"  | CID15=="B182"  | CID16=="B182"  | CID17=="B182"  | CID18=="B182"  | CID19=="B182"  | CID20=="B182"  | CID21=="B182"  | CID22=="B182"  | CID23=="B182"  | CID24=="B182"  | CID25=="B182"  | CID26=="B182" )

SIM_PR_hepc <-   filter(SIM_PO, CID1== "B171" | CID2== "B171" | CID3=="B171" | CID4=="B171" | CID5=="B171" | CID6=="B171" | CID7=="B171" | CID8=="B171" | CID9=="B171"| CID10=="B171" | CID11=="B171" | CID12=="B171" | CID13=="B171" | CID14=="B171" | CID15=="B171" | CID16=="B171" | CID17=="B171" | CID18=="B171" | CID19=="B171" | CID20=="B171" | CID21=="B171" | CID22=="B171" | CID23=="B171" | CID24=="B171" | CID25=="B171" | CID26=="B171" | CID1== "B182" | CID2== "B182" | CID3=="B182"  | CID4=="B182"  | CID5=="B182"  | CID6=="B182"  | CID7=="B182"  | CID8=="B182"  | CID9=="B182"  | CID10=="B182"  | CID11=="B182"  | CID12=="B182"  | CID13=="B182"  | CID14=="B182"  | CID15=="B182"  | CID16=="B182"  | CID17=="B182"  | CID18=="B182"  | CID19=="B182"  | CID20=="B182"  | CID21=="B182"  | CID22=="B182"  | CID23=="B182"  | CID24=="B182"  | CID25=="B182"  | CID26=="B182" )

SIM_hepc <- do.call("rbind", list(SIM_PO_hepc, SIM_PR_hepc))

SIM_hepc <- distinct(SIM_hepc)

#################### 
####### Comparando tabelas por diferen?cas em colunas ##############################################################################
####################

AIH_PO_hepc$ID_PACIENTE <- as.character(AIH_PO_hepc$ID_PACIENTE) 

AIH_PR_hepc$ID_PACIENTE <- as.character(AIH_PR_hepc$ID_PACIENTE) 

APAC_PO_hepc$ID_PACIENTE <- as.character(APAC_PO_hepc$ID_PACIENTE) 

APAC_PR_hepc$ID_PACIENTE <- as.character(APAC_PR_hepc$ID_PACIENTE) 

SIM_PO_hepc$ID_PACIENTE <- as.character(SIM_PO_hepc$ID_PACIENTE) 

SIM_PR_hepc$ID_PACIENTE <- as.character(SIM_PR_hepc$ID_PACIENTE) 

### AIH

ctable_DF_AIH_ID = compare_df(AIH_PO_hepc, AIH_PR_hepc, c("ID_PACIENTE"))

print(ctable_DF_AIH_ID$html_output)

ctable_DF_AIH_ID$change_summary

### APAC
ctable_DF_APAC_ID = compare_df(APAC_PO_hepc, APAC_PR_hepc, c("ID_PACIENTE"))

print(ctable_DF_APAC_ID$html_output)

ctable_DF_APAC_ID$change_summary

### SIM 

ctable_DF_SIM_ID = compare_df(SIM_PO_hepc, SIM_PR_hepc, c("ID_PACIENTE"))

print(ctable_DF_SIM_ID$html_output)

####################################################################################################################################


#### cirrose ####

## 'K74', 'K743', 'K744', 'K745', 'K746'

## APAC ##

APAC_hepc_cir <- filter(APAC_hepc, CID1== "K74" | CID1== "K743"  | CID1== "K744"  | CID1== "K745"  | CID1== "K746" | CID2== "K74"  | CID2== "K743" | CID2== "K744" | CID2== "K745" | CID2== "K746" | CID3=="K74" | CID3=="K743"| CID3=="K744"| CID3=="K745"| CID3=="K746" )

## AIH ##

AIH_hepc_cir <- filter(AIH_hepc, CID1== "K74" | CID1== "K743"  | CID1== "K744"  | CID1== "K745"  | CID1== "K746" | CID2== "K74"  | CID2== "K743" | CID2== "K744" | CID2== "K745" | CID2== "K746" | CID3=="K74" | CID3=="K743"| CID3=="K744"| CID3=="K745"| CID3=="K746" | CID4=="K74" | CID4=="K743"| CID4=="K744"| CID4=="K745"| CID4=="K746"  | CID5=="K74" | CID5=="K743"| CID5=="K744"| CID5=="K745"| CID5=="K746"  | CID6=="K74" | CID6=="K743"| CID6=="K744"| CID6=="K745"| CID6=="K746"   | CID7=="K74" | CID7=="K743"| CID7=="K744"| CID7=="K745"| CID7=="K746"   | CID8=="K74" | CID8=="K743"| CID8=="K744"| CID8=="K745"| CID8=="K746"   | CID9=="K74" | CID9=="K743"| CID9=="K744"| CID9=="K745"| CID9=="K746"  | CID10=="K74" | CID10=="K743"| CID10=="K744"| CID10=="K745"| CID10=="K746"   | CID11=="K74" | CID11=="K743"| CID11=="K744"| CID11=="K745"| CID11=="K746"   | CID12=="K74" | CID12=="K743"| CID12=="K744"| CID12=="K745"| CID12=="K746" )

## BPAI ##

BPAI_hepc_cir <- filter(BPAI_PR_hepc, CID== "K74" | CID== "K743"  | CID== "K744"  | CID== "K745"  | CID== "K746" )

## SIM ##

SIM_hepc_cir <- filter(SIM_hepc, CID1== "K74" | CID1== "K743"  | CID1== "K744"  | CID1== "K745"  | CID1== "K746" | CID2== "K74"  | CID2== "K743" | CID2== "K744" | CID2== "K745" | CID2== "K746" | CID3=="K74" | CID3=="K743"| CID3=="K744"| CID3=="K745"| CID3=="K746" | CID4=="K74" | CID4=="K743"| CID4=="K744"| CID4=="K745"| CID4=="K746"  | CID5=="K74" | CID5=="K743"| CID5=="K744"| CID5=="K745"| CID5=="K746"  | CID6=="K74" | CID6=="K743"| CID6=="K744"| CID6=="K745"| CID6=="K746"   | CID7=="K74" | CID7=="K743"| CID7=="K744"| CID7=="K745"| CID7=="K746"   | CID8=="K74" | CID8=="K743"| CID8=="K744"| CID8=="K745"| CID8=="K746"   | CID9=="K74" | CID9=="K743"| CID9=="K744"| CID9=="K745"| CID9=="K746"  | CID10=="K74" | CID10=="K743"| CID10=="K744"| CID10=="K745"| CID10=="K746"   | CID11=="K74" | CID11=="K743"| CID11=="K744"| CID11=="K745"| CID11=="K746"   | CID12=="K74" | CID12=="K743"| CID12=="K744"| CID12=="K745"| CID12=="K746"  | CID13=="K74" | CID13=="K743"| CID13=="K744"| CID13=="K745"| CID13=="K746"   | CID14=="K74" | CID14=="K743"| CID14=="K744"| CID14=="K745"| CID14=="K746"   | CID15=="K74" | CID15=="K743"| CID15=="K744"| CID15=="K745"| CID15=="K746"  | CID16=="K74" | CID16=="K743"| CID16=="K744"| CID16=="K745"| CID16=="K746"  | CID17=="K74" | CID17=="K743"| CID17=="K744"| CID17=="K745"| CID17=="K746"   | CID18=="K74" | CID18=="K743"| CID18=="K744"| CID18=="K745"| CID18=="K746"  | CID19=="K74" | CID19=="K743"| CID19=="K744"| CID19=="K745"| CID19=="K746"   | CID20=="K74" | CID20=="K743"| CID20=="K744"| CID20=="K745"| CID20=="K746"   | CID21=="K74" | CID21=="K743"| CID21=="K744"| CID21=="K745"| CID21=="K746" )

SIM_hepc_cir_un <- distinct(SIM_hepc_cir, ID_PACIENTE, .keep_all = TRUE)

### Cirrose

hepc_cirrose <- do.call("rbind", list(APAC_hepc_cir, AIH_hepc_cir, BPAI_hepc_cir))

hepc_cirrose <- distinct(hepc_cirrose)

hepc_cirrose_un <- distinct(hepc_cirrose, ID_PACIENTE, .keep_all = TRUE)

#### anti join quem n?o est? morto por cirrose (at? 2016)

#### Pessoas com cirrose de 2008 - 2018 (novembro) 

hepc_cirrose_anti_join <- anti_join(hepc_cirrose_un, SIM_hepc_cir,by="ID_PACIENTE")


### numero de mortos por cirrose 2006 - 2016

hepc_cirrose_right_join <- right_join(hepc_cirrose_un, SIM_hepc_cir,by="ID_PACIENTE")

### carcinoma hepatocelular ####

#### 'C220', 'C221', 'C222', 'C223', 'C224', 'C225', 'C226', 'C227', 'C228', 'C229'

## APAC ##

# APAC_hepc_hepatocel <- filter(APAC_hepc, CID1== "C220" | CID1== "C221" | CID1== "C222"  | CID1== "C223"  | CID1== "C224"  | CID1== "C225" | CID1== "C226"  | CID1== "C227" | CID1== "C228"  | CID1== "C229" | CID2== "C220"  | CID2== "C221" | CID2== "C222" | CID2== "C223" | CID2== "C224" | CID2== "C225" | CID2== "C226" | CID2== "C227" | CID2== "C228" | CID2== "C229" | CID3=="C220" | CID3=="C221"| CID3=="C222"| CID3=="C223"| CID3=="C224" | CID3=="C225" | CID3=="C226" | CID3=="C227" | CID3=="C228" | CID3=="C229" )

APAC_hepc_hepatocel0 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C220" | CID2 %like% "C220"  | CID3 %like% "C220"  )

APAC_hepc_hepatocel1 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C221" | CID2 %like% "C221"  | CID3 %like% "C221"  )

APAC_hepc_hepatocel2 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C222" | CID2 %like% "C222"  | CID3 %like% "C222"  )

APAC_hepc_hepatocel3 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C223" | CID2 %like% "C223"  | CID3 %like% "C223"  )

APAC_hepc_hepatocel4 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C224" | CID2 %like% "C224"  | CID3 %like% "C224"  )

APAC_hepc_hepatocel5 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C225" | CID2 %like% "C225"  | CID3 %like% "C225"  )

APAC_hepc_hepatocel6 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C226" | CID2 %like% "C226"  | CID3 %like% "C226"  )

APAC_hepc_hepatocel7 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C227" | CID2 %like% "C227"  | CID3 %like% "C227"  )

APAC_hepc_hepatocel8 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C228" | CID2 %like% "C228"  | CID3 %like% "C228"  )

APAC_hepc_hepatocel9 <- APAC_hepc %>% dplyr::filter(CID1 %like% "C229" | CID2 %like% "C229"  | CID3 %like% "C229"  )

## AIH ##

AIH_hepc_hepatocel0 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C220" | CID2 %like% "C220"  | CID3 %like% "C220" | CID4 %like% "C220" | CID5 %like% "C220" | CID6 %like% "C220" | CID7 %like% "C220" | CID8 %like% "C220" | CID9 %like% "C220" | CID10 %like% "C220" | CID11 %like% "C220" | CID12 %like% "C220" )

AIH_hepc_hepatocel1 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C221" | CID2 %like% "C221"  | CID3 %like% "C221" | CID4 %like% "C221" | CID5 %like% "C221" | CID6 %like% "C221" | CID7 %like% "C221" | CID8 %like% "C221" | CID9 %like% "C221" | CID10 %like% "C221" | CID11 %like% "C221" | CID12 %like% "C221" )

AIH_hepc_hepatocel2 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C222" | CID2 %like% "C222"  | CID3 %like% "C222" | CID4 %like% "C222" | CID5 %like% "C222" | CID6 %like% "C222" | CID7 %like% "C222" | CID8 %like% "C222" | CID9 %like% "C222" | CID10 %like% "C222" | CID11 %like% "C222" | CID12 %like% "C222" )

AIH_hepc_hepatocel3 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C223" | CID2 %like% "C223"  | CID3 %like% "C223" | CID4 %like% "C223" | CID5 %like% "C223" | CID6 %like% "C223" | CID7 %like% "C223" | CID8 %like% "C223" | CID9 %like% "C223" | CID10 %like% "C223" | CID11 %like% "C223" | CID12 %like% "C223" )

AIH_hepc_hepatocel4 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C224" | CID2 %like% "C224"  | CID3 %like% "C224" | CID4 %like% "C224" | CID5 %like% "C224" | CID6 %like% "C224" | CID7 %like% "C224" | CID8 %like% "C224" | CID9 %like% "C224" | CID10 %like% "C224" | CID11 %like% "C224" | CID12 %like% "C224" )

AIH_hepc_hepatocel5 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C225" | CID2 %like% "C225"  | CID3 %like% "C225" | CID4 %like% "C225" | CID5 %like% "C225" | CID6 %like% "C225" | CID7 %like% "C225" | CID8 %like% "C225" | CID9 %like% "C225" | CID10 %like% "C225" | CID11 %like% "C225" | CID12 %like% "C225" )

AIH_hepc_hepatocel6 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C226" | CID2 %like% "C226"  | CID3 %like% "C226" | CID4 %like% "C226" | CID5 %like% "C226" | CID6 %like% "C226" | CID7 %like% "C226" | CID8 %like% "C226" | CID9 %like% "C226" | CID10 %like% "C226" | CID11 %like% "C226" | CID12 %like% "C226" )

AIH_hepc_hepatocel7 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C227" | CID2 %like% "C227"  | CID3 %like% "C227" | CID4 %like% "C227" | CID5 %like% "C227" | CID6 %like% "C227" | CID7 %like% "C227" | CID8 %like% "C227" | CID9 %like% "C227" | CID10 %like% "C227" | CID11 %like% "C227" | CID12 %like% "C227" )

AIH_hepc_hepatocel8 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C228" | CID2 %like% "C228"  | CID3 %like% "C228" | CID4 %like% "C228" | CID5 %like% "C228" | CID6 %like% "C228" | CID7 %like% "C228" | CID8 %like% "C228" | CID9 %like% "C228" | CID10 %like% "C228" | CID11 %like% "C228" | CID12 %like% "C228" )

AIH_hepc_hepatocel9 <- AIH_hepc %>% dplyr::filter(CID1 %like% "C229" | CID2 %like% "C229"  | CID3 %like% "C229" | CID4 %like% "C229" | CID5 %like% "C229" | CID6 %like% "C229" | CID7 %like% "C229" | CID8 %like% "C229" | CID9 %like% "C229" | CID10 %like% "C229" | CID11 %like% "C229" | CID12 %like% "C229" )


AIH_hepc_hepatocel <- do.call("rbind", list(AIH_hepc_hepatocel0, AIH_hepc_hepatocel1, AIH_hepc_hepatocel2, AIH_hepc_hepatocel3, AIH_hepc_hepatocel4, AIH_hepc_hepatocel5,AIH_hepc_hepatocel6, AIH_hepc_hepatocel7, AIH_hepc_hepatocel8, AIH_hepc_hepatocel9))


## BPAI ##

BPAI_hepc_hepatocel <- filter(BPAI_PR_hepc, CID== "C220" | CID== "C221"  | CID== "C222"  | CID== "C223"  | CID== "C224" | CID== "C225" | CID== "C226" | CID== "C227" | CID== "C228" | CID== "C229" )


## SIM ##

SIM_hepc_hepatocel0 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C220" | CID2 %like% "C220"  | CID3 %like% "C220" | CID4 %like% "C220" | CID5 %like% "C220" | CID6 %like% "C220" | CID7 %like% "C220" | CID8 %like% "C220" | CID9 %like% "C220" | CID10 %like% "C220" | CID11 %like% "C220" | CID12 %like% "C220" | CID13 %like% "C220" | CID14 %like% "C220"  | CID15 %like% "C220" | CID16 %like% "C220" | CID17 %like% "C220" | CID18 %like% "C220" | CID19 %like% "C220" | CID20 %like% "C220" | CID21 %like% "C220" | CID22 %like% "C220" | CID23 %like% "C220" | CID24 %like% "C220" | CID25 %like% "C220"| CID26 %like% "C220" )

SIM_hepc_hepatocel1 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C221" | CID2 %like% "C221"  | CID3 %like% "C221" | CID4 %like% "C221" | CID5 %like% "C221" | CID6 %like% "C221" | CID7 %like% "C221" | CID8 %like% "C221" | CID9 %like% "C221" | CID10 %like% "C221" | CID11 %like% "C221" | CID12 %like% "C221"  |  CID13 %like% "C221" | CID14 %like% "C221"  | CID15 %like% "C221" | CID16 %like% "C221" | CID17 %like% "C221" | CID18 %like% "C221" | CID19 %like% "C221" | CID20 %like% "C221" | CID21 %like% "C221" | CID22 %like% "C221" | CID23 %like% "C221" | CID24 %like% "C221"  | CID25 %like% "C221" | CID26 %like% "C221")

SIM_hepc_hepatocel2 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C222" | CID2 %like% "C222"  | CID3 %like% "C222" | CID4 %like% "C222" | CID5 %like% "C222" | CID6 %like% "C222" | CID7 %like% "C222" | CID8 %like% "C222" | CID9 %like% "C222" | CID10 %like% "C222" | CID11 %like% "C222" | CID12 %like% "C222" | CID13 %like% "C222" | CID14 %like% "C222"  | CID15 %like% "C222" | CID16 %like% "C222" | CID17 %like% "C222" | CID18 %like% "C222" | CID19 %like% "C222" | CID20 %like% "C222" | CID21 %like% "C222" | CID22 %like% "C222" | CID23 %like% "C222" | CID24 %like% "C222"  | CID25 %like% "C222"  | CID26 %like% "C222" )

SIM_hepc_hepatocel3 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C223" | CID2 %like% "C223"  | CID3 %like% "C223" | CID4 %like% "C223" | CID5 %like% "C223" | CID6 %like% "C223" | CID7 %like% "C223" | CID8 %like% "C223" | CID9 %like% "C223" | CID10 %like% "C223" | CID11 %like% "C223" | CID12 %like% "C223" |  CID13 %like% "C223" | CID14 %like% "C223"  | CID15 %like% "C223" | CID16 %like% "C223" | CID17 %like% "C223" | CID18 %like% "C223" | CID19 %like% "C223" | CID20 %like% "C223" | CID21 %like% "C223" | CID22 %like% "C223" | CID23 %like% "C223" | CID24 %like% "C223" | CID25 %like% "C223"| CID26 %like% "C223" )

SIM_hepc_hepatocel4 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C224" | CID2 %like% "C224"  | CID3 %like% "C224" | CID4 %like% "C224" | CID5 %like% "C224" | CID6 %like% "C224" | CID7 %like% "C224" | CID8 %like% "C224" | CID9 %like% "C224" | CID10 %like% "C224" | CID11 %like% "C224" | CID12 %like% "C224"  |  CID13 %like% "C224" | CID14 %like% "C224"  | CID15 %like% "C224" | CID16 %like% "C224" | CID17 %like% "C224" | CID18 %like% "C224" | CID19 %like% "C224" | CID20 %like% "C224" | CID21 %like% "C224" | CID22 %like% "C224" | CID23 %like% "C224" | CID24 %like% "C224"  | CID24 %like% "C225" | CID24 %like% "C226" )

SIM_hepc_hepatocel5 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C225" | CID2 %like% "C225"  | CID3 %like% "C225" | CID4 %like% "C225" | CID5 %like% "C225" | CID6 %like% "C225" | CID7 %like% "C225" | CID8 %like% "C225" | CID9 %like% "C225" | CID10 %like% "C225" | CID11 %like% "C225" | CID12 %like% "C225" |  CID13 %like% "C225" | CID14 %like% "C225"  | CID15 %like% "C225" | CID16 %like% "C225" | CID17 %like% "C225" | CID18 %like% "C225" | CID19 %like% "C225" | CID20 %like% "C225" | CID21 %like% "C225" | CID22 %like% "C225" | CID23 %like% "C225" | CID24 %like% "C225" | CID25 %like% "C225"| CID26 %like% "C225" )

SIM_hepc_hepatocel6 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C226" | CID2 %like% "C226"  | CID3 %like% "C226" | CID4 %like% "C226" | CID5 %like% "C226" | CID6 %like% "C226" | CID7 %like% "C226" | CID8 %like% "C226" | CID9 %like% "C226" | CID10 %like% "C226" | CID11 %like% "C226" | CID12 %like% "C226" |  CID13 %like% "C226" | CID14 %like% "C226"  | CID15 %like% "C226" | CID16 %like% "C226" | CID17 %like% "C226" | CID18 %like% "C226" | CID19 %like% "C226" | CID20 %like% "C226" | CID21 %like% "C226" | CID22 %like% "C226" | CID23 %like% "C226" | CID24 %like% "C226"  | CID25 %like% "C226"  | CID26 %like% "C226" )

SIM_hepc_hepatocel7 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C227" | CID2 %like% "C227"  | CID3 %like% "C227" | CID4 %like% "C227" | CID5 %like% "C227" | CID6 %like% "C227" | CID7 %like% "C227" | CID8 %like% "C227" | CID9 %like% "C227" | CID10 %like% "C227" | CID11 %like% "C227" | CID12 %like% "C227" |  CID13 %like% "C227" | CID14 %like% "C227"  | CID15 %like% "C227" | CID16 %like% "C227" | CID17 %like% "C227" | CID18 %like% "C227" | CID19 %like% "C227" | CID20 %like% "C227" | CID21 %like% "C227" | CID22 %like% "C227" | CID23 %like% "C227" | CID24 %like% "C227"  | CID25 %like% "C227" | CID26 %like% "C227"  )

SIM_hepc_hepatocel8 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C228" | CID2 %like% "C228"  | CID3 %like% "C228" | CID4 %like% "C228" | CID5 %like% "C228" | CID6 %like% "C228" | CID7 %like% "C228" | CID8 %like% "C228" | CID9 %like% "C228" | CID10 %like% "C228" | CID11 %like% "C228" | CID12 %like% "C228" |  CID13 %like% "C228" | CID14 %like% "C228"  | CID15 %like% "C228" | CID16 %like% "C228" | CID17 %like% "C228" | CID18 %like% "C228" | CID19 %like% "C228" | CID20 %like% "C228" | CID21 %like% "C228" | CID22 %like% "C228" | CID23 %like% "C228" | CID24 %like% "C228"  | CID25 %like% "C228"  | CID26 %like% "C228" )

SIM_hepc_hepatocel9 <- SIM_hepc %>% dplyr::filter(CID1 %like% "C229" | CID2 %like% "C229"  | CID3 %like% "C229" | CID4 %like% "C229" | CID5 %like% "C229" | CID6 %like% "C229" | CID7 %like% "C229" | CID8 %like% "C229" | CID9 %like% "C229" | CID10 %like% "C229" | CID11 %like% "C229" | CID12 %like% "C229" |  CID13 %like% "C229" | CID14 %like% "C229"  | CID15 %like% "C229" | CID16 %like% "C229" | CID17 %like% "C229" | CID18 %like% "C229" | CID19 %like% "C229" | CID20 %like% "C229" | CID21 %like% "C229" | CID22 %like% "C229" | CID23 %like% "C229" | CID24 %like% "C229"  | CID25 %like% "C229"  | CID26 %like% "C229" )


SIM_hepc_hepatocel <- do.call("rbind", list(SIM_hepc_hepatocel0, SIM_hepc_hepatocel1, SIM_hepc_hepatocel2, SIM_hepc_hepatocel3, SIM_hepc_hepatocel4, SIM_hepc_hepatocel5,SIM_hepc_hepatocel6, SIM_hepc_hepatocel7, SIM_hepc_hepatocel8, SIM_hepc_hepatocel9))

###  Carcinoma hepatocelular

hepc_carc_hepat <- do.call("rbind", list(APAC_hepc_hepatocel, AIH_hepc_hepatocel, BPAI_hepc_hepatocel))

hepc_carc_hepat  <- distinct(hepc_carc_hepat)

hepc_carc_hepat_un <- distinct(hepc_carc_hepat, ID_PACIENTE, .keep_all = TRUE)


#### anti join quem n?o est? morto por carcinoma hepatocelular (at? 2016)

#### Pessoas com ccarcinoma hepatocelular de 2008 - 2018 (novembro) 

hepc_carc_hepat_anti_join <- anti_join(hepc_carc_hepat_un, SIM_hepc_hepatocel,by="ID_PACIENTE")


### numero de mortos por carcinoma hepatocelular 2006 - 2016

hepc_carc_hepat_rigth_join <- right_join(hepc_carc_hepat_un, SIM_hepc_hepatocel,by="ID_PACIENTE")


### transplante de f?gado ###

### 'Z944'

## APAC ##

APAC_hepc_trans <- filter(APAC_hepc, CID1== "Z944"  | CID2== "Z944" | CID3=="Z944" )

APAC_hepc_trans_unitecol <- unite(APAC_hepc_trans, "CID", CID1, CID2, CID3, sep = " ")

APAC_hepc_trans_unitecol <- unite(APAC_hepc_trans_unitecol, "PROCEDIMENTO", PROCEDIMENTO1, PROCEDIMENTO2, sep = " ")

## AIH ##

AIH_hepc_trans <- filter(AIH_hepc, CID1== "Z944" | CID2== "Z944" | CID3=="Z944" | CID4=="Z944"  | CID5=="Z944"  | CID6=="Z944"  | CID7=="Z944"  | CID8=="Z944"  | CID9=="Z944"  | CID10=="Z944" | CID11=="Z944" | CID12=="Z944" )

AIH_hepc_trans_unitecol <- unite(AIH_hepc_trans, "CID", CID1, CID2, CID3, CID4, CID5, CID6, CID7, CID8, CID9, CID10, CID11, CID12, sep = " ")

AIH_hepc_trans_unitecol <- unite(AIH_hepc_trans_unitecol, "PROCEDIMENTO", PROCEDIMENTO1, PROCEDIMENTO2,PROCEDIMENTO3, sep = " ")

## BPAI ##

BPAI_hepc_trans <- filter(BPAI_PR_hepc, CID== "Z944" )

## SIM ##

SIM_hepc_trans <- filter(SIM_hepc, CID1== "Z944"  | CID2== "Z944" | CID3=="Z944" | CID4=="Z944"  | CID5=="Z944" | CID6=="Z944"  | CID7=="Z944" | CID8=="Z944" | CID9=="Z944" | CID10=="Z944"  | CID11=="Z944" | CID12=="Z944" | CID13=="Z944" | CID14=="Z944" | CID15=="Z944"  | CID16=="Z944"  | CID17=="Z944" | CID18=="Z944"  | CID19=="Z944"  | CID20=="Z944" | CID21=="Z944" )

###  Transplante de f?gado

hepc_trans <- do.call("rbind", list(APAC_hepc_trans_unitecol, AIH_hepc_trans_unitecol))

hepc_trans  <- distinct(hepc_trans)

hepc_trans_un <- distinct(hepc_trans, ID_PACIENTE, .keep_all = TRUE)


### anti join quem n?o est? morto por cirrose (at? 2016)

#### Pessoas com cirrose de 2008 - 2018 (novembro) 

hepc_carc_hepat_anti_join <- anti_join(hepc_carc_hepat_un, SIM_hepc_hepatocel,by="ID_PACIENTE")


### numero de mortos por cirrose 2006 - 2016

hepc_carc_hepat_rigth_join <- right_join(hepc_carc_hepat_un, SIM_hepc_hepatocel,by="ID_PACIENTE")


### anti join quem n?o est? morto por transplante de f?gado (at? 2016)

#### Pessoas com transplante de f?gado de 2008 - 2018 (novembro) 

hepc_trans_anti_join <- anti_join(hepc_trans_un, SIM_hepc_trans,by="ID_PACIENTE")


### numero de mortos por transplante de f?gado 2006 - 2016

hepc_trans_right_join <- right_join(hepc_trans_un, SIM_hepc_trans,by="ID_PACIENTE")


############
######
###### OBS : as mortes foram checadas entre 2016 e 2018 (novembro) pelo CID_OBITO do AIH
###########

############################################################################
#############################  PLOTS #######################################
############################################################################

######## ?bitos ########

SIM_hepc_cir$agravo_morte <- "cirrose"  

SIM_hepc_hepatocel$agravo_morte <- "carcinoma hepatocelular"

SIM_hepc_trans$agravo_morte <- "transplante de f?gado"

morte_agravo_plot <- do.call("rbind", list(SIM_hepc_cir, SIM_hepc_hepatocel))

morte_agravo_plot <- table(morte_agravo_plot$agravo_morte)

morte_agravo_plot <- as.data.frame(morte_agravo_plot)

morte_agravo_plot$ano <- "HCV - 2006-2016"

################################ Plot mortes por agravos ##################################

ggplot(data=morte_agravo_plot, aes(x=ano, y=Freq, fill=Var1)) +
        geom_MGr(stat="identity", width = 0.4) +
        geom_text(aes(label=Freq), position = position_stack(vjust = 0.5))+
        theme_minimal()  + labs(x="Hepatite viral", y = "Frequ?ncia de ?bitos") + labs(fill = "CID") + ggtitle("a)") +  theme(plot.title = element_text(size = 12, face = "plain"))


######## agravos vivos - 2008 - 2018 ########

hepc_carc_hepat_anti_join <- unite(hepc_carc_hepat_anti_join, "CID", CID1, CID2, CID3,CID4, CID5, CID6, CID7, CID8, CID9, CID10, CID11, CID12, sep = " ")

hepc_cirrose_anti_join <- unite(hepc_cirrose_anti_join, "CID", CID1, CID2, CID3,CID4, CID5, CID6, CID7, CID8, CID9, CID10, CID11, CID12, sep = " ")

hepc_cirrose_anti_join <- unite(hepc_cirrose_anti_join, "PROCEDIMENTO", PROCEDIMENTO1, PROCEDIMENTO2,PROCEDIMENTO3, sep = " ")


hepc_cirrose_anti_join$agravo <- "cirrose"  

hepc_carc_hepat_anti_join$agravo <- "carcinoma hepatocelular"

hepc_trans_anti_join$agravo <- "transplante de f?gado"

agravo_plot <- do.call("rbind", list(hepc_cirrose_anti_join, hepc_carc_hepat_anti_join, hepc_trans_anti_join))

agravo_plot <- table(agravo_plot$agravo)

agravo_plot <- as.data.frame(agravo_plot)

agravo_plot$ano <- "HCV - 2008-2018"

################################ Plot  por agravos ##################################

ggplot(data=agravo_plot, aes(x=ano, y=Freq, fill=Var1)) +
        geom_bar(stat="identity", width = 0.4) +
        geom_text(aes(label=Freq), position = position_stack(vjust = 0.5))+
        theme_minimal()  + labs(x="Hepatite viral", y = "Frequ?ncia") + scale_fill_manual("CID", values = c("carcinoma hepatocelular" = "#f8766d", "cirrose" = "#00bfc4", "transplante de f?gado" = "#a667d0")) + ggtitle("a)") + 
        theme(plot.title = element_text(size = 12, face = "plain"))


##########################################################################################
############################  Scatter plot agravos por ano ###############################
##########################################################################################

hepc_carc_hepat_anti_join <- hepc_carc_hepat_anti_join %>% separate(DT_OCOR, sep="-", into = c("ano", "mes", "dia"))

hepc_cirrose_anti_join <- hepc_cirrose_anti_join %>% separate(DT_OCOR, sep="-", into = c("ano", "mes", "dia"))

hepc_trans_anti_join <- hepc_trans_anti_join %>% separate(DT_OCOR, sep="-", into = c("ano", "mes", "dia"))

carc_hepat_plot <- table(hepc_carc_hepat_anti_join$ano)
carc_hepat_plot <- as.data.frame(carc_hepat_plot)
carc_hepat_plot$Agravos <- "carcinoma hepatocelular"

cirrose_plot <- table(hepc_cirrose_anti_join$ano)
cirrose_plot <- as.data.frame(cirrose_plot)
cirrose_plot$Agravos <- "cirrose"

trans_plot <- table(hepc_trans_anti_join$ano)
trans_plot <- as.data.frame(trans_plot)
trans_plot$Agravos <- " transplante de f?gado"

agravos_plot <- do.call("rbind", list(carc_hepat_plot, cirrose_plot, trans_plot))

agravos_plot <- rename(agravos_plot, CID = Agravos)

agravos_plot$Var1 <- as.character.Date(agravos_plot$Var1)

agravos_plot$Var1 <- as.numeric(agravos_plot$Var1)


################# scatter plot linhas ########################
ggplot(data = agravos_plot, aes(x =Var1, 
                                y = Freq, 
                                group=CID, 
                                color=CID )) +
        geom_line() +
        geom_point() + 
        labs( 
                y="Frequ?ncia", 
                x="Ano"
        ) + geom_text(aes(label=Freq),hjust=0, vjust=0, check_overlap = TRUE, size = 3) + scale_x_continuous(breaks = seq(2008, 2018, by = 1)) + scale_color_manual(values=c("#a667d0", "#f8766d", "#00bfc4")) + theme_minimal() 
#############################################################


##########################################################################################
############################  Scatter plot obitos por ano ###############################
##########################################################################################

hepc_carc_hepat_rigth_join <- hepc_carc_hepat_rigth_join %>% separate(DT_OCOR.y, sep="-", into = c("ano", "mes", "dia"))

hepc_cirrose_right_join <- hepc_cirrose_right_join %>% separate(DT_OCOR.y, sep="-", into = c("ano", "mes", "dia"))


carc_hepat_plot_obt <- table(hepc_carc_hepat_rigth_join$ano)
carc_hepat_plot_obt <- as.data.frame(carc_hepat_plot_obt)
carc_hepat_plot_obt$Agravos <- "carcinoma hepatocelular"

cirrose_plot_obt <- table(hepc_cirrose_right_join$ano)
cirrose_plot_obt <- as.data.frame(cirrose_plot_obt)
cirrose_plot_obt$Agravos <- "cirrose"

obt_plot <- do.call("rbind", list(carc_hepat_plot_obt, cirrose_plot_obt))

obt_plot <- rename(obt_plot, CID = Agravos)

obt_plot$Var1 <- as.character.Date(obt_plot$Var1)
obt_plot$Var1 <- as.numeric(obt_plot$Var1)


################# scatter plot linhas ########################
ggplot(data = obt_plot, aes(x =Var1, 
                            y = Freq, 
                            group=CID, 
                            color=CID )) +
        geom_line() +
        geom_point() + 
        labs( 
                y="Frequ?ncia de ?bitos", 
                x="Ano"
        ) + geom_text(aes(label=Freq),hjust=0, vjust=0, check_overlap = TRUE, size = 3) + scale_x_continuous(breaks = seq(2006, 2016, by = 1)) + scale_color_manual(values=c("#f8766d", "#00bfc4")) + theme_minimal() 
#############################################################

################################################## compara??o tabela completa #############################################
SIM_hepc_cir_un$ID_PACIENTE <- as.character(SIM_hepc_cir_un$ID_PACIENTE)
SIM_hepc_hepatocel$ID_PACIENTE <- as.character(SIM_hepc_hepatocel$ID_PACIENTE)

SIM_hepc_cir_un <- SIM_hepc_cir_un %>% unite( "CID", CID1, CID2, CID3,CID4, CID5, CID6, CID7, CID8, CID9, CID10, CID11, CID12, CID13, CID14, CID15, CID16, CID17, CID18, CID19, CID20, CID21, CID22, CID23, CID24, CID25, CID26, sep = " ")

SIM_hepc_cir_un$agravo_morte <- "cirrose"

SIM_hepc_hepatocel <- SIM_hepc_hepatocel %>% unite( "CID", CID1, CID2, CID3,CID4, CID5, CID6, CID7, CID8, CID9, CID10, CID11, CID12, CID13, CID14, CID15, CID16, CID17, CID18, CID19, CID20, CID21, CID22, CID23, CID24, CID25, CID26, sep = " ")

ctable_DF = compare_df(SIM_hepc_cir_un, SIM_hepc_hepatocel ,  c("ID_PACIENTE"))

print(ctable_DF$html_output)
############################################################################################################################

################################################## compara??o ID ##########################################################
SIM_hepc_cir_un$ID_PACIENTE <- as.character(SIM_hepc_cir_un$ID_PACIENTE)
SIM_hepc_hepatocel$ID_PACIENTE <- as.character(SIM_hepc_hepatocel$ID_PACIENTE)

cirrose_ID_paciente <- SIM_hepc_hepatocel$ID_PACIENTE

hepatocel_ID_paciente <- SIM_hepc_hepatocel$ID_PACIENTE

cirrose_ID_paciente <- as.data.frame(cirrose_ID_paciente)

cirrose_ID_paciente <- rename(cirrose_ID_paciente,  ID_PACIENTE = cirrose_ID_paciente )

hepatocel_ID_paciente <- as.data.frame(hepatocel_ID_paciente)

hepatocel_ID_paciente <- rename(hepatocel_ID_paciente,  ID_PACIENTE = hepatocel_ID_paciente )

ID_paciente <- do.call("rbind", list(cirrose_ID_paciente, hepatocel_ID_paciente))

distinct_ID_paciente <- distinct(ID_paciente)
############################################################################################################################

############# Pie chart agravos ##############
colors = c("#f8766d", "#00bfc4" , "#a667d0" )

# Pie Chart with Percentages
slices <- c(277, 657, 146)
lbls <- c("carcinoma hepatocelular", "cirrose", "transplante de f?gado" )
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=colors,
    main="b)                                                                                            ", cex.main = 1.0, font.main = 1) 

#############################################


############# Pie chart  ?bitos ##############

# Pie Chart with Percentages
slices <- c(4912, 2246)
lbls <- c("carcinoma hepatocelular", "cirrose" )
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=colors,
    main="b)                                                                                                           " , cex.main = 1.0, font.main = 1)

#############################################

################## Barplot agravos por estado 

carc_hepat_plot_uf <- table(hepc_carc_hepat_anti_join$UF_OCOR)
carc_hepat_plot_uf <- as.data.frame(carc_hepat_plot_uf)
carc_hepat_plot_uf$Agravos <- "carcinoma hepatocelular"

cirrose_plot_uf <- table(hepc_cirrose_anti_join$UF_OCOR)
cirrose_plot_uf <- as.data.frame(cirrose_plot_uf)
cirrose_plot_uf$Agravos <- "cirrose"

trans_plot_uf <- table(hepc_trans_anti_join$UF_OCOR)
trans_plot_uf <- as.data.frame(trans_plot_uf)
trans_plot_uf$Agravos <- " transplante de f?gado"

agravos_plot_uf <- do.call("rbind", list(carc_hepat_plot_uf, cirrose_plot_uf, trans_plot_uf))

agravos_plot_uf <- rename(agravos_plot_uf, CID=Agravos)

agravos_plot_uf <- filter(agravos_plot_uf, Freq!=0)

ggplot(data=agravos_plot_uf , aes(x=reorder(Var1, -Freq), y=Freq , fill=CID)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=Freq), position = position_stack(vjust = 0.8), size=2.5)+
        theme_minimal()  + labs(x="UF", y = "Frequ?ncia") + scale_fill_manual(values=c('#a667d0','#f8766d', "#00bfc4" ))


###########################################


###########################   Barplot agravos por estado  - dodge ##########################
ggplot(data=agravos_plot_uf , aes(x=reorder(Var1, -Freq), y=Freq , fill=CID)) +
        geom_bar(stat="identity", position=position_dodge()) +
        geom_text(aes(label=Freq),vjust=-0.3, position = position_dodge(0.9), size=2.5)+
        theme_minimal()  + labs(x="UF", y = "Frequ?ncia") + scale_fill_manual(values=c('#a667d0','#f8766d', "#00bfc4" ))
########################################################


################# Barplot ?bitos por estado 

carc_hepat_plot_obt_uf <- table(hepc_carc_hepat_rigth_join$UF_OCOR.y)
carc_hepat_plot_obt_uf <- as.data.frame(carc_hepat_plot_obt_uf)
carc_hepat_plot_obt_uf$CID <- "carcinoma hepatocelular"

cirrose_plot_obt_uf <- table(hepc_cirrose_right_join$UF_OCOR.y)
cirrose_plot_obt_uf <- as.data.frame(cirrose_plot_obt_uf)
cirrose_plot_obt_uf$CID <- "cirrose"

obt_plot_uf <- do.call("rbind", list(carc_hepat_plot_obt_uf, cirrose_plot_obt_uf))

obt_plot_uf <- filter(obt_plot_uf, Freq!=0)

ggplot(data=obt_plot_uf , aes(x=reorder(Var1, -Freq), y=Freq , fill=CID)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=Freq), position = position_stack(vjust = 0.8), size=2.5)+
        theme_minimal()  + labs(x="UF", y = "Frequ?ncia de ?bitos") + scale_fill_manual(values=c('#f8766d', "#00bfc4" ))

###########################################

###########################   Barplot obitos por estado  - dodge ##########################
ggplot(data=obt_plot_uf , aes(x=reorder(Var1, -Freq), y=Freq , fill=CID)) +
        geom_bar(stat="identity", position=position_dodge()) +
        geom_text(aes(label=Freq),vjust=-0.3, position = position_dodge(0.9), size=2.5)+
        theme_minimal()  + labs(x="UF", y = "Frequ?ncia de ?bitos") + scale_fill_manual(values=c('#f8766d', "#00bfc4" ))
########################################################

##################################################################################
######################  DADOS PROVENIENTES DO TABWIN #############################
##################################################################################

setwd("c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/")

setwd("/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/")

files <- list.files(path = "/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/", pattern = "*", full.names = T)

tbl <- sapply(files, read.dbc) %>% bind_rows()

AC_AL_AM_AP <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/AC_AL_AM_AP/", pattern = "AMAC*", full.names = T)

BA <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/BA/", pattern = "AMBA*", full.names = T)

CE <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/CE/", pattern = "AMCE*", full.names = T)

DF <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/DF/", pattern = "AMDF*", full.names = T)

ES <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/ES/", pattern = "AMES*", full.names = T)

GO <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/GO/", pattern = "AMGO*", full.names = T)

MA <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/MA/", pattern = "AMMA*", full.names = T)

MG <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/MG/", pattern = "AMMG*", full.names = T)

MS <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/MS/", pattern = "AMMS*", full.names = T)

MT <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/MT/", pattern = "AMMT*", full.names = T)

PA <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/PA/", pattern = "AMPA*", full.names = T)

PB <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/PB/", pattern = "AMPB*", full.names = T)

PE <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/PE/", pattern = "AMPE*", full.names = T)

PI <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/PI/", pattern = "AMPI*", full.names = T)

PR <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/PR/", pattern = "AMPR*", full.names = T)

RJ <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/RJ/", pattern = "AMRJ*", full.names = T)

RN <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/RN/", pattern = "AMRN*", full.names = T)

RO <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/RO/", pattern = "AMRO*", full.names = T)

RR <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/RR/", pattern = "AMRR*", full.names = T)

RS <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/RS/", pattern = "AMRS*", full.names = T)

SC <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SC/", pattern = "AMSC*", full.names = T)

SP <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/", pattern = "AMSP*", full.names = T)

SP2008 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2008", pattern = "AMSP*", full.names = T)

SP2009 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2009", pattern = "AMSP*", full.names = T)

SP2010 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2010", pattern = "AMSP*", full.names = T)

SP2011 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2011", pattern = "AMSP*", full.names = T)

SP2012 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2012", pattern = "AMSP*", full.names = T)

SP2013 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2013", pattern = "AMSP*", full.names = T)

SP2014 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2014", pattern = "AMSP*", full.names = T)

SP2015 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2015", pattern = "AMSP*", full.names = T)

SP2016 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2016", pattern = "AMSP*", full.names = T)

SP2017 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2017", pattern = "AMSP*", full.names = T)

SP2018 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2018", pattern = "AMSP*", full.names = T)

SP2019 <- list.files(path = "c:/Users/mikael.lemos/Desktop/TABWIN/APAC/script_sh/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SP/2019", pattern = "AMSP*", full.names = T)

SE <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/SE/", pattern = "AMSE*", full.names = T)

TO <- list.files(path = "/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/TO/", pattern = "AMTO*", full.names = T)

AC_AL_AM_APtbl <- sapply(AC_AL_AM_AP, read.dbc) %>% bind_rows()

BAtbl <- sapply(BA, read.dbc) %>% bind_rows()

CEtbl <- sapply(CE, read.dbc) %>% bind_rows()

DFtbl <- sapply(DF, read.dbc) %>% bind_rows()

EStbl <- sapply(ES, read.dbc) %>% bind_rows()

GOtbl <- sapply(GO, read.dbc) %>% bind_rows()

MAtbl <- sapply(MA, read.dbc) %>% bind_rows()

MGtbl <- sapply(MG, read.dbc) %>% bind_rows()

MStbl <- sapply(MS, read.dbc) %>% bind_rows()

MTtbl <- sapply(MT, read.dbc) %>% bind_rows()

PAtbl <- sapply(PA, read.dbc) %>% bind_rows()

PBtbl <- sapply(PB, read.dbc) %>% bind_rows()

PEtbl <- sapply(PE, read.dbc) %>% bind_rows()

PItbl <- sapply(PI, read.dbc) %>% bind_rows()

PRtbl <- sapply(PR, read.dbc) %>% bind_rows()

RJtbl <- sapply(RJ, read.dbc) %>% bind_rows()

RNtbl <- sapply(RN, read.dbc) %>% bind_rows()

ROtbl <- sapply(RO, read.dbc) %>% bind_rows()

RRtbl <- sapply(RR, read.dbc) %>% bind_rows()

RStbl <- sapply(RS, read.dbc) %>% bind_rows()

SCtbl <- sapply(SC, read.dbc) %>% bind_rows()

SPtbl2008 <- lapply(SP2008, read.dbc) %>% bind_rows()
SPtbl2009 <- lapply(SP2009, read.dbc) %>% bind_rows()
SPtbl2010 <- lapply(SP2010, read.dbc) %>% bind_rows()
SPtbl2011 <- lapply(SP2011, read.dbc) %>% bind_rows()
SPtbl2012 <- lapply(SP2012, read.dbc) %>% bind_rows()
SPtbl2013 <- lapply(SP2013, read.dbc) %>% bind_rows()
SPtbl2014 <- lapply(SP2014, read.dbc) %>% bind_rows()
SPtbl2015 <- lapply(SP2015, read.dbc) %>% bind_rows()
SPtbl2016 <- lapply(SP2016, read.dbc) %>% bind_rows()
SPtbl2017 <- lapply(SP2017, read.dbc) %>% bind_rows()
SPtbl2018 <- lapply(SP2018, read.dbc) %>% bind_rows()
SPtbl2019 <- lapply(SP2019, read.dbc) %>% bind_rows()


SEtbl <- sapply(SE, read.dbc) %>% bind_rows()

TOtbl <- sapply(TO, read.dbc) %>% bind_rows()




## alfainterferona 
AC_AL_AM_APtbl_alfainterf <- filter(AC_AL_AM_APtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

BAtbl_alfainterf <- filter(BAtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

CEtbl_alfainterf <- filter(CEtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

DFtbl_alfainterf <- filter(DFtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

EStbl_alfainterf <- filter(EStbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

GOtbl_alfainterf <- filter(GOtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

MAtbl_alfainterf <- filter(MAtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

MGtbl_alfainterf <- filter(MGtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

MStbl_alfainterf <- filter(MStbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

MTtbl_alfainterf <- filter(MTtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

PAtbl_alfainterf <- filter(PAtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

PBtbl_alfainterf <- filter(PBtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

PEtbl_alfainterf <- filter(PEtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

PItbl_alfainterf <- filter(PItbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

PRtbl_alfainterf <- filter(PRtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

RJtbl_alfainterf <- filter(RJtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

RNtbl_alfainterf <- filter(RNtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

ROtbl_alfainterf <- filter(ROtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

RRtbl_alfainterf <- filter(RRtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

RStbl_alfainterf <- filter(RStbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SCtbl_alfainterf <- filter(SCtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SEtbl_alfainterf <- filter(SEtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

TOtbl_alfainterf <- filter(TOtbl, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2008 <- filter(SPtbl2008, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2009 <- filter(SPtbl2009, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2010 <- filter(SPtbl2010, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2011 <- filter(SPtbl2011, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2012 <- filter(SPtbl2012, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2013 <- filter(SPtbl2013, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2014 <- filter(SPtbl2014, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2015 <- filter(SPtbl2015, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2016 <- filter(SPtbl2016, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2017 <- filter(SPtbl2017, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2018 <- filter(SPtbl2018, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

SPtbl_alfainterf2019 <- filter(SPtbl2019, AP_PRIPAL== "0601190017" | AP_PRIPAL== "0604390033" | AP_PRIPAL== "0601190025" | AP_PRIPAL== "0604390017" | AP_PRIPAL== "0601190033" | AP_PRIPAL== "0604390025" )

write.csv(SPtbl_alfainterf2008, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2008.csv' )

write.csv(SPtbl_alfainterf2009, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2009.csv' )

write.csv(SPtbl_alfainterf2010, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2010.csv' )

write.csv(SPtbl_alfainterf2011, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2011.csv' )

write.csv(SPtbl_alfainterf2012, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2012.csv' )

write.csv(SPtbl_alfainterf2013, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2013.csv' )

write.csv(SPtbl_alfainterf2014, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2014.csv' )

write.csv(SPtbl_alfainterf2015, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2015.csv' )

write.csv(SPtbl_alfainterf2016, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2016.csv' )

write.csv(SPtbl_alfainterf2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2017.csv' )

write.csv(SPtbl_alfainterf2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2018.csv' )

write.csv(SPtbl_alfainterf2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfainterf2019.csv' )

write.csv(AC_AL_AM_APtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_alfainterf.csv' )

write.csv(BAtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_alfainterf.csv' )

write.csv(CEtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_alfainterf.csv' )

write.csv(DFtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_alfainterf.csv' )

write.csv(EStbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_alfainterf.csv' )

write.csv(GOtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_alfainterf.csv' )

write.csv(MAtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_alfainterf.csv' )

write.csv(MGtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_alfainterf.csv' )

write.csv(MStbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_alfainterf.csv' )

write.csv(MTtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_alfainterf.csv' )

write.csv(PAtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_alfainterf.csv' )

write.csv(PBtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_alfainterf.csv' )

write.csv(PEtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_alfainterf.csv' )

write.csv(PItbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_alfainterf.csv' )

write.csv(PRtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_alfainterf.csv' )

write.csv(RJtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_alfainterf.csv' )

write.csv(RNtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_alfainterf.csv' )

write.csv(ROtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_alfainterf.csv' )

write.csv(RRtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_alfainterf.csv' )

write.csv(RStbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_alfainterf.csv' )

write.csv(SCtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_alfainterf.csv' )

write.csv(SEtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_alfainterf.csv' )

write.csv(TOtbl_alfainterf, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_alfainterf.csv' )

## alfapeginterferona
AC_AL_AM_APtbl_alfapeg <- filter(AC_AL_AM_APtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

BAtbl_alfapeg <- filter(BAtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

CEtbl_alfapeg <- filter(CEtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

DFtbl_alfapeg <- filter(DFtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

EStbl_alfapeg <- filter(EStbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

GOtbl_alfapeg <- filter(GOtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

MAtbl_alfapeg <- filter(MAtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

MGtbl_alfapeg <- filter(MGtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

MStbl_alfapeg <- filter(MStbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

MTtbl_alfapeg <- filter(MTtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

PAtbl_alfapeg <- filter(PAtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

PBtbl_alfapeg <- filter(PBtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

PEtbl_alfapeg <- filter(PEtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

PItbl_alfapeg <- filter(PItbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

PRtbl_alfapeg <- filter(PRtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

RJtbl_alfapeg <- filter(RJtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

RNtbl_alfapeg <- filter(RNtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

ROtbl_alfapeg <- filter(ROtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

RRtbl_alfapeg <- filter(RRtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

RStbl_alfapeg <- filter(RStbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SCtbl_alfapeg <- filter(SCtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SEtbl_alfapeg <- filter(SEtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

TOtbl_alfapeg <- filter(TOtbl,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2010 <- filter(SPtbl2010,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2011 <- filter(SPtbl2011,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2012 <- filter(SPtbl2012,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2013 <- filter(SPtbl2013,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2014 <- filter(SPtbl2014,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2015 <- filter(SPtbl2015,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2016 <- filter(SPtbl2016,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2017 <- filter(SPtbl2017,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2018 <- filter(SPtbl2018,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

SPtbl_alfapeg2019 <- filter(SPtbl2019,  AP_PRIPAL== "0604390041" |  AP_PRIPAL== "0604390050" |  AP_PRIPAL== "0604390068" |  AP_PRIPAL== "0604390076" )

write.csv(SPtbl_alfapeg2010, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2010.csv' )

write.csv(SPtbl_alfapeg2011, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2011.csv' )

write.csv(SPtbl_alfapeg2012, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2012.csv' )

write.csv(SPtbl_alfapeg2013, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2013.csv' )

write.csv(SPtbl_alfapeg2014, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2014.csv' )

write.csv(SPtbl_alfapeg2015, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2015.csv' )

write.csv(SPtbl_alfapeg2016, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2016.csv' )

write.csv(SPtbl_alfapeg2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2017.csv' )

write.csv(SPtbl_alfapeg2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2018.csv' )

write.csv(SPtbl_alfapeg2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_alfapeg2019.csv' )

write.csv(AC_AL_AM_APtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_alfapeg.csv' )

write.csv(BAtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_alfapeg.csv' )

write.csv(CEtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_alfapeg.csv' )

write.csv(DFtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_alfapeg.csv' )

write.csv(EStbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_alfapeg.csv' )

write.csv(GOtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_alfapeg.csv' )

write.csv(MAtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_alfapeg.csv' )

write.csv(MGtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_alfapeg.csv' )

write.csv(MStbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_alfapeg.csv' )

write.csv(MTtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_alfapeg.csv' )

write.csv(PAtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_alfapeg.csv' )

write.csv(PBtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_alfapeg.csv' )

write.csv(PEtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_alfapeg.csv' )

write.csv(PItbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_alfapeg.csv' )

write.csv(PRtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_alfapeg.csv' )

write.csv(RJtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_alfapeg.csv' )

write.csv(RNtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_alfapeg.csv' )

write.csv(ROtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_alfapeg.csv' )

write.csv(RRtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_alfapeg.csv' )

write.csv(RStbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_alfapeg.csv' )

write.csv(SCtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_alfapeg.csv' )

write.csv(SEtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_alfapeg.csv' )

write.csv(TOtbl_alfapeg, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_alfapeg.csv' )

## Ribavirina 
AC_AL_AM_APtbl_riba <- filter(AC_AL_AM_APtbl,  AP_PRIPAL== "0604450010")

BAtbl_riba <- filter(BAtbl,  AP_PRIPAL== "0604450010")

CEtbl_riba <- filter(CEtbl,  AP_PRIPAL== "0604450010")

DFtbl_riba <- filter(DFtbl,  AP_PRIPAL== "0604450010")

EStbl_riba <- filter(EStbl,  AP_PRIPAL== "0604450010")

GOtbl_riba <- filter(GOtbl,  AP_PRIPAL== "0604450010")

MAtbl_riba <- filter(MAtbl,  AP_PRIPAL== "0604450010")

MGtbl_riba <- filter(MGtbl,  AP_PRIPAL== "0604450010")

MStbl_riba <- filter(MStbl,  AP_PRIPAL== "0604450010")

MTtbl_riba <- filter(MTtbl,  AP_PRIPAL== "0604450010")

PAtbl_riba <- filter(PAtbl,  AP_PRIPAL== "0604450010")

PBtbl_riba <- filter(PBtbl,  AP_PRIPAL== "0604450010")

PEtbl_riba <- filter(PEtbl,  AP_PRIPAL== "0604450010")

PItbl_riba <- filter(PItbl,  AP_PRIPAL== "0604450010")

PRtbl_riba <- filter(PRtbl,  AP_PRIPAL== "0604450010")

RJtbl_riba <- filter(RJtbl,  AP_PRIPAL== "0604450010")

RNtbl_riba <- filter(RNtbl,  AP_PRIPAL== "0604450010")

ROtbl_riba <- filter(ROtbl,  AP_PRIPAL== "0604450010")

RRtbl_riba <- filter(RRtbl,  AP_PRIPAL== "0604450010")

RStbl_riba <- filter(RStbl,  AP_PRIPAL== "0604450010")

SCtbl_riba <- filter(SCtbl,  AP_PRIPAL== "0604450010")

SEtbl_riba <- filter(SEtbl,  AP_PRIPAL== "0604450010")

TOtbl_riba <- filter(TOtbl,  AP_PRIPAL== "0604450010")

SPtbl_riba2010 <- filter(SPtbl2010,  AP_PRIPAL== "0604450010")

SPtbl_riba2011 <- filter(SPtbl2011,  AP_PRIPAL== "0604450010")

SPtbl_riba2012 <- filter(SPtbl2012,  AP_PRIPAL== "0604450010")

SPtbl_riba2013 <- filter(SPtbl2013,  AP_PRIPAL== "0604450010")

SPtbl_riba2014 <- filter(SPtbl2014,  AP_PRIPAL== "0604450010")

SPtbl_riba2015 <- filter(SPtbl2015,  AP_PRIPAL== "0604450010")

SPtbl_riba2016 <- filter(SPtbl2016,  AP_PRIPAL== "0604450010")

SPtbl_riba2017 <- filter(SPtbl2017,  AP_PRIPAL== "0604450010")

SPtbl_riba2018 <- filter(SPtbl2018,  AP_PRIPAL== "0604450010")

SPtbl_riba2019 <- filter(SPtbl2019,  AP_PRIPAL== "0604450010")


write.csv(SPtbl_riba2010, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2010.csv' )

write.csv(SPtbl_riba2011, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2011.csv' )

write.csv(SPtbl_riba2012, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2012.csv' )

write.csv(SPtbl_riba2013, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2013.csv' )

write.csv(SPtbl_riba2014, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2014.csv' )

write.csv(SPtbl_riba2015, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2015.csv' )

write.csv(SPtbl_riba2016, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2016.csv' )

write.csv(SPtbl_riba2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2017.csv' )

write.csv(SPtbl_riba2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2018.csv' )

write.csv(SPtbl_riba2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_riba2019.csv' )

write.csv(AC_AL_AM_APtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_riba.csv' )

write.csv(BAtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_riba.csv' )

write.csv(CEtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_riba.csv' )

write.csv(DFtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_riba.csv' )

write.csv(EStbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_riba.csv' )

write.csv(GOtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_riba.csv' )

write.csv(MAtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_riba.csv' )

write.csv(MGtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_riba.csv' )

write.csv(MStbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_riba.csv' )

write.csv(MTtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_riba.csv' )

write.csv(PAtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_riba.csv' )

write.csv(PBtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_riba.csv' )

write.csv(PEtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_riba.csv' )

write.csv(PItbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_riba.csv' )

write.csv(PRtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_riba.csv' )

write.csv(RJtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_riba.csv' )

write.csv(RNtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_riba.csv' )

write.csv(ROtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_riba.csv' )

write.csv(RRtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_riba.csv' )

write.csv(RStbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_riba.csv' )

write.csv(SCtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_riba.csv' )

write.csv(SEtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_riba.csv' )

write.csv(TOtbl_riba, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_riba.csv' )

## boceprevi
AC_AL_AM_APtbl_boce <- filter(AC_AL_AM_APtbl,  AP_PRIPAL== "0604640013")

BAtbl_boce <- filter(BAtbl,  AP_PRIPAL== "0604640013")

CEtbl_boce <- filter(CEtbl,  AP_PRIPAL== "0604640013")

DFtbl_boce <- filter(DFtbl,  AP_PRIPAL== "0604640013")

EStbl_boce <- filter(EStbl,  AP_PRIPAL== "0604640013")

GOtbl_boce <- filter(GOtbl,  AP_PRIPAL== "0604640013")

MAtbl_boce <- filter(MAtbl,  AP_PRIPAL== "0604640013")

MGtbl_boce <- filter(MGtbl,  AP_PRIPAL== "0604640013")

MStbl_boce <- filter(MStbl,  AP_PRIPAL== "0604640013")

MTtbl_boce <- filter(MTtbl,  AP_PRIPAL== "0604640013")

PAtbl_boce <- filter(PAtbl,  AP_PRIPAL== "0604640013")

PBtbl_boce <- filter(PBtbl,  AP_PRIPAL== "0604640013")

PEtbl_boce <- filter(PEtbl,  AP_PRIPAL== "0604640013")

PItbl_boce <- filter(PItbl,  AP_PRIPAL== "0604640013")

PRtbl_boce <- filter(PRtbl,  AP_PRIPAL== "0604640013")

RJtbl_boce <- filter(RJtbl,  AP_PRIPAL== "0604640013")

RNtbl_boce <- filter(RNtbl,  AP_PRIPAL== "0604640013")

ROtbl_boce <- filter(ROtbl,  AP_PRIPAL== "0604640013")

RRtbl_boce <- filter(RRtbl,  AP_PRIPAL== "0604640013")

RStbl_boce <- filter(RStbl,  AP_PRIPAL== "0604640013")

SCtbl_boce <- filter(SCtbl,  AP_PRIPAL== "0604640013")

SEtbl_boce <- filter(SEtbl,  AP_PRIPAL== "0604640013")

TOtbl_boce <- filter(TOtbl,  AP_PRIPAL== "0604640013")

SPtbl_boce2013 <- filter(SPtbl2013,  AP_PRIPAL== "0604640013")

SPtbl_boce2014 <- filter(SPtbl2014,  AP_PRIPAL== "0604640013")

SPtbl_boce2015 <- filter(SPtbl2015,  AP_PRIPAL== "0604640013")

SPtbl_boce2016 <- filter(SPtbl2016,  AP_PRIPAL== "0604640013")

SPtbl_boce2017 <- filter(SPtbl2017,  AP_PRIPAL== "0604640013")

SPtbl_boce2018 <- filter(SPtbl2018,  AP_PRIPAL== "0604640013")

SPtbl_boce2019 <- filter(SPtbl2019,  AP_PRIPAL== "0604640013")


write.csv(SPtbl_boce2013, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_boce2013.csv' )

write.csv(SPtbl_boce2014, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_boce2014.csv' )

write.csv(SPtbl_boce2015, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_boce2015.csv' )

write.csv(SPtbl_boce2016, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_boce2016.csv' )

write.csv(SPtbl_boce2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_boce2017.csv' )

write.csv(SPtbl_boce2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_boce2018.csv' )

write.csv(SPtbl_boce2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_boce2019.csv' )

write.csv(AC_AL_AM_APtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_boce.csv' )

write.csv(BAtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_boce.csv' )

write.csv(CEtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_boce.csv' )

write.csv(DFtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_boce.csv' )

write.csv(EStbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_boce.csv' )

write.csv(GOtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_boce.csv' )

write.csv(MAtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_boce.csv' )

write.csv(MGtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_boce.csv' )

write.csv(MStbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_boce.csv' )

write.csv(MTtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_boce.csv' )

write.csv(PAtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_boce.csv' )

write.csv(PBtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_boce.csv' )

write.csv(PEtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_boce.csv' )

write.csv(PItbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_boce.csv' )

write.csv(PRtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_boce.csv' )

write.csv(RJtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_boce.csv' )

write.csv(RNtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_boce.csv' )

write.csv(ROtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_boce.csv' )

write.csv(RRtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_boce.csv' )

write.csv(RStbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_boce.csv' )

write.csv(SCtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_boce.csv' )

write.csv(SEtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_boce.csv' )

write.csv(TOtbl_boce, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_boce.csv' )

## teloprevir
AC_AL_AM_APtbl_telo <- filter(AC_AL_AM_APtbl,  AP_PRIPAL== "0604640021")

BAtbl_telo <- filter(BAtbl,  AP_PRIPAL== "0604640021")

CEtbl_telo <- filter(CEtbl,  AP_PRIPAL== "0604640021")

DFtbl_telo <- filter(DFtbl,  AP_PRIPAL== "0604640021")

EStbl_telo <- filter(EStbl,  AP_PRIPAL== "0604640021")

GOtbl_telo <- filter(GOtbl,  AP_PRIPAL== "0604640021")

MAtbl_telo <- filter(MAtbl,  AP_PRIPAL== "0604640021")

MGtbl_telo <- filter(MGtbl,  AP_PRIPAL== "0604640021")

MStbl_telo <- filter(MStbl,  AP_PRIPAL== "0604640021")

MTtbl_telo <- filter(MTtbl,  AP_PRIPAL== "0604640021")

PAtbl_telo <- filter(PAtbl,  AP_PRIPAL== "0604640021")

PBtbl_telo <- filter(PBtbl,  AP_PRIPAL== "0604640021")

PEtbl_telo <- filter(PEtbl,  AP_PRIPAL== "0604640021")

PItbl_telo <- filter(PItbl,  AP_PRIPAL== "0604640021")

PRtbl_telo <- filter(PRtbl,  AP_PRIPAL== "0604640021")

RJtbl_telo <- filter(RJtbl,  AP_PRIPAL== "0604640021")

RNtbl_telo <- filter(RNtbl,  AP_PRIPAL== "0604640021")

ROtbl_telo <- filter(ROtbl,  AP_PRIPAL== "0604640021")

RRtbl_telo <- filter(RRtbl,  AP_PRIPAL== "0604640021")

RStbl_telo <- filter(RStbl,  AP_PRIPAL== "0604640021")

SCtbl_telo <- filter(SCtbl,  AP_PRIPAL== "0604640021")

SEtbl_telo <- filter(SEtbl,  AP_PRIPAL== "0604640021")

TOtbl_telo <- filter(TOtbl,  AP_PRIPAL== "0604640021")

SPtbl_telo2013 <- filter(SPtbl2013,  AP_PRIPAL== "0604640021")

SPtbl_telo2014 <- filter(SPtbl2014,  AP_PRIPAL== "0604640021")

SPtbl_telo2015 <- filter(SPtbl2015,  AP_PRIPAL== "0604640021")

SPtbl_telo2016 <- filter(SPtbl2016,  AP_PRIPAL== "0604640021")

SPtbl_telo2017 <- filter(SPtbl2017,  AP_PRIPAL== "0604640021")

SPtbl_telo2018 <- filter(SPtbl2018,  AP_PRIPAL== "0604640021")

SPtbl_telo2019 <- filter(SPtbl2019,  AP_PRIPAL== "0604640021")


write.csv(SPtbl_telo2013, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_telo2013.csv' )

write.csv(SPtbl_telo2014, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_telo2014.csv' )

write.csv(SPtbl_telo2015, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_telo2015.csv' )

write.csv(SPtbl_telo2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_telo2017.csv' )

write.csv(SPtbl_telo2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_telo2018.csv' )

write.csv(SPtbl_telo2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SPtbl_telo2019.csv' )

write.csv(AC_AL_AM_APtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_telo.csv' )

write.csv(BAtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_telo.csv' )

write.csv(CEtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_telo.csv' )

write.csv(DFtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_telo.csv' )

write.csv(EStbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_telo.csv' )

write.csv(GOtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_telo.csv' )

write.csv(MAtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_telo.csv' )

write.csv(MGtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_telo.csv' )

write.csv(MStbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_telo.csv' )

write.csv(MTtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_telo.csv' )

write.csv(PAtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_telo.csv' )

write.csv(PBtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_telo.csv' )

write.csv(PEtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_telo.csv' )

write.csv(PItbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_telo.csv' )

write.csv(PRtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_telo.csv' )

write.csv(RJtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_telo.csv' )

write.csv(RNtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_telo.csv' )

write.csv(ROtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_telo.csv' )

write.csv(RRtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_telo.csv' )

write.csv(RStbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_telo.csv' )

write.csv(SCtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_telo.csv' )

write.csv(SEtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_telo.csv' )

write.csv(TOtbl_telo, file = '/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_telo.csv' )

## SOFOSBUVIR 

AC_AL_AM_APtbl_sofo <-  filter(AC_AL_AM_APtbl, AP_PRIPAL== "0604760019")

BAtbl_sofo <- filter(BAtbl, AP_PRIPAL== "0604760019")

CEtbl_sofo <- filter(CEtbl, AP_PRIPAL== "0604760019")

DFtbl_sofo <- filter(DFtbl, AP_PRIPAL== "0604760019")

EStbl_sofo <- filter(EStbl, AP_PRIPAL== "0604760019")

GOtbl_sofo <- filter(GOtbl, AP_PRIPAL== "0604760019")

MAtbl_sofo <- filter(MAtbl, AP_PRIPAL== "0604760019")

MGtbl_sofo <- filter(MGtbl, AP_PRIPAL== "0604760019")

MStbl_sofo <- filter(MStbl, AP_PRIPAL== "0604760019")

MTtbl_sofo <- filter(MTtbl, AP_PRIPAL== "0604760019")

PAtbl_sofo <- filter(PAtbl, AP_PRIPAL== "0604760019")

PBtbl_sofo <- filter(PBtbl, AP_PRIPAL== "0604760019")

PEtbl_sofo <- filter(PEtbl, AP_PRIPAL== "0604760019")

PItbl_sofo <- filter(PItbl, AP_PRIPAL== "0604760019")

PRtbl_sofo <- filter(PRtbl, AP_PRIPAL== "0604760019")

RJtbl_sofo <- filter(RJtbl, AP_PRIPAL== "0604760019")

RNtbl_sofo <- filter(RNtbl, AP_PRIPAL== "0604760019")

ROtbl_sofo <- filter(ROtbl, AP_PRIPAL== "0604760019")

RRtbl_sofo <- filter(RRtbl, AP_PRIPAL== "0604760019")

RStbl_sofo <- filter(RStbl, AP_PRIPAL== "0604760019")

SCtbl_sofo <- filter(SCtbl, AP_PRIPAL== "0604760019")

SPtbl_sofo2015 <- filter(SPtbl2015, AP_PRIPAL== "0604760019")
SPtbl_sofo2016 <- filter(SPtbl2016, AP_PRIPAL== "0604760019")
SPtbl_sofo2017 <- filter(SPtbl2017, AP_PRIPAL== "0604760019")
SPtbl_sofo2018 <- filter(SPtbl2018, AP_PRIPAL== "0604760019")
SPtbl_sofo2019 <- filter(SPtbl2019, AP_PRIPAL== "0604760019")



SEtbl_sofo <- filter(SEtbl, AP_PRIPAL== "0604760019")

TOtbl_sofo <- filter(TOtbl, AP_PRIPAL== "0604760019")

write.csv(AC_AL_AM_APtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/AC_AL_AM_APtbl_sofo.csv' )

write.csv(BAtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/BA_sofo.csv' )

write.csv(CEtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/CE_sofo.csv' )

write.csv(DFtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/DF_sofo.csv' )

write.csv(EStbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/ES_sofo.csv' )

write.csv(GOtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/GO_sofo.csv' )

write.csv(MAtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MA_sofo.csv' )

write.csv(MGtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MG_sofo.csv' )

write.csv(MStbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MS_sofo.csv' )

write.csv(MTtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MT_sofo.csv' )

write.csv(PAtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PA_sofo.csv' )

write.csv(PBtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PB_sofo.csv' )

write.csv(PEtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PE_sofo.csv' )

write.csv(PItbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PI_sofo.csv' )

write.csv(PRtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PR_sofo.csv' )

write.csv(RJtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RJ_sofo.csv' )

write.csv(RNtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RN_sofo.csv' )

write.csv(ROtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RO_sofo.csv' )

write.csv(RRtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RR_sofo.csv' )

write.csv(RStbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RS_sofo.csv' )

write.csv(SCtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SC_sofo.csv' )

write.csv(SPtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SP_sofo.csv' )

write.csv(SPtbl_sofo2015, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sofo2015.csv' )

write.csv(SPtbl_sofo2016, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sofo2016.csv' )

write.csv(SPtbl_sofo2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sofo2017.csv' )

write.csv(SPtbl_sofo2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sofo2018.csv' )

write.csv(SPtbl_sofo2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sofo2019.csv' )

write.csv(SEtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SE_sofo.csv' )

write.csv(TOtbl_sofo, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/TO_sofo.csv' )

## DACLATASVIR 

AC_AL_AM_APtbl_dacla <-  filter(AC_AL_AM_APtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

BAtbl_dacla <- filter(BAtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

CEtbl_dacla <- filter(CEtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

DFtbl_dacla <- filter(DFtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

EStbl_dacla <- filter(EStbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

GOtbl_dacla <- filter(GOtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

MAtbl_dacla <- filter(MAtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

MGtbl_dacla <- filter(MGtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

MStbl_dacla <- filter(MStbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

MTtbl_dacla <- filter(MTtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

PAtbl_dacla <- filter(PAtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

PBtbl_dacla <- filter(PBtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

PEtbl_dacla <- filter(PEtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

PItbl_dacla <- filter(PItbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

PRtbl_dacla <- filter(PRtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

RJtbl_dacla <- filter(RJtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

RNtbl_dacla <- filter(RNtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

ROtbl_dacla <- filter(ROtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

RRtbl_dacla <- filter(RRtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

RStbl_dacla <- filter(RStbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

SCtbl_dacla <- filter(SCtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

SEtbl_dacla <- filter(SEtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

SPtbl_dacla <- filter(SPtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )


SPtbl_dacla2015 <- filter(SPtbl2015, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )
SPtbl_dacla2016 <- filter(SPtbl2016, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )
SPtbl_dacla2017 <- filter(SPtbl2017, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )
SPtbl_dacla2018 <- filter(SPtbl2018, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )
SPtbl_dacla2019 <- filter(SPtbl2019, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )


TOtbl_dacla <- filter(TOtbl, AP_PRIPAL== "0604760027" | AP_PRIPAL== "0604760035" )

write.csv(AC_AL_AM_APtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/AC_AL_AM_APtbl_dacla.csv' )

write.csv(BAtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/BA_dacla.csv' )

write.csv(CEtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/CE_dacla.csv' )

write.csv(DFtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/DF_dacla.csv' )

write.csv(EStbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/ES_dacla.csv' )

write.csv(GOtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/GO_dacla.csv' )

write.csv(MAtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MA_dacla.csv' )

write.csv(MGtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MG_dacla.csv' )

write.csv(MStbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MS_dacla.csv' )

write.csv(MTtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MT_dacla.csv' )

write.csv(PAtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PA_dacla.csv' )

write.csv(PBtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PB_dacla.csv' )

write.csv(PEtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PE_dacla.csv' )

write.csv(PItbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PI_dacla.csv' )

write.csv(PRtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PR_dacla.csv' )

write.csv(RJtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RJ_dacla.csv' )

write.csv(RNtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RN_dacla.csv' )

write.csv(ROtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RO_dacla.csv' )

write.csv(RRtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RR_dacla.csv' )

write.csv(RStbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RS_dacla.csv' )

write.csv(SCtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SC_dacla.csv' )

write.csv(SPtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SP_dacla.csv' )

write.csv(SPtbl_dacla2015, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_dacla2015.csv' )

write.csv(SPtbl_dacla2016, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_dacla2016.csv' )

write.csv(SPtbl_dacla2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_dacla2017.csv' )

write.csv(SPtbl_dacla2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_dacla2018.csv' )

write.csv(SPtbl_dacla2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_dacla2019.csv' )

write.csv(SEtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SE_dacla.csv' )

write.csv(TOtbl_dacla, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/TO_dacla.csv' )

## SIMEPREVIR  
AC_AL_AM_APtbl_sime <-  filter(AC_AL_AM_APtbl, AP_PRIPAL== "0604640030")

BAtbl_sime <- filter(BAtbl, AP_PRIPAL== "0604640030")

CEtbl_sime <- filter(CEtbl, AP_PRIPAL== "0604640030")

DFtbl_sime <- filter(DFtbl, AP_PRIPAL== "0604640030")

EStbl_sime <- filter(EStbl, AP_PRIPAL== "0604640030")

GOtbl_sime <- filter(GOtbl, AP_PRIPAL== "0604640030")

MAtbl_sime <- filter(MAtbl, AP_PRIPAL== "0604640030")

MGtbl_sime <- filter(MGtbl, AP_PRIPAL== "0604640030")

MStbl_sime <- filter(MStbl, AP_PRIPAL== "0604640030")

MTtbl_sime <- filter(MTtbl, AP_PRIPAL== "0604640030")

PAtbl_sime <- filter(PAtbl, AP_PRIPAL== "0604640030")

PBtbl_sime <- filter(PBtbl, AP_PRIPAL== "0604640030")

PEtbl_sime <- filter(PEtbl, AP_PRIPAL== "0604640030")

PItbl_sime <- filter(PItbl, AP_PRIPAL== "0604640030")

PRtbl_sime <- filter(PRtbl, AP_PRIPAL== "0604640030")

RJtbl_sime <- filter(RJtbl, AP_PRIPAL== "0604640030")

RNtbl_sime <- filter(RNtbl, AP_PRIPAL== "0604640030")

ROtbl_sime <- filter(ROtbl, AP_PRIPAL== "0604640030")

RRtbl_sime <- filter(RRtbl, AP_PRIPAL== "0604640030")

RStbl_sime <- filter(RStbl, AP_PRIPAL== "0604640030")

SCtbl_sime <- filter(SCtbl, AP_PRIPAL== "0604640030")

SPtbl_sime <- filter(SPtbl, AP_PRIPAL== "0604640030")

SPtbl_sime2015 <- filter(SPtbl2015, AP_PRIPAL== "0604640030")

SPtbl_sime2016 <- filter(SPtbl2016, AP_PRIPAL== "0604640030")

SPtbl_sime2017 <- filter(SPtbl2017, AP_PRIPAL== "0604640030")

SPtbl_sime2018 <- filter(SPtbl2018, AP_PRIPAL== "0604640030")

SPtbl_sime2019 <- filter(SPtbl2019, AP_PRIPAL== "0604640030")

SEtbl_sime <- filter(SEtbl, AP_PRIPAL== "0604640030")

TOtbl_sime <- filter(TOtbl, AP_PRIPAL== "0604640030")

write.csv(AC_AL_AM_APtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/AC_AL_AM_APtbl_sime.csv' )

write.csv(BAtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/BA_sime.csv' )

write.csv(CEtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/CE_sime.csv' )

write.csv(DFtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/DF_sime.csv' )

write.csv(EStbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/ES_sime.csv' )

write.csv(GOtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/GO_sime.csv' )

write.csv(MAtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MA_sime.csv' )

write.csv(MGtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MG_sime.csv' )

write.csv(MStbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MS_sime.csv' )

write.csv(MTtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MT_sime.csv' )

write.csv(PAtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PA_sime.csv' )

write.csv(PBtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PB_sime.csv' )

write.csv(PEtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PE_sime.csv' )

write.csv(PItbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PI_sime.csv' )

write.csv(PRtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PR_sime.csv' )

write.csv(RJtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RJ_sime.csv' )

write.csv(RNtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RN_sime.csv' )

write.csv(ROtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RO_sime.csv' )

write.csv(RRtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RR_sime.csv' )

write.csv(RStbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RS_sime.csv' )

write.csv(SCtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SC_sime.csv' )

write.csv(SPtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SP_sime.csv' )

write.csv(SPtbl_sime2015, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sime2015.csv' )

write.csv(SPtbl_sime2016, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sime2016.csv' )

write.csv(SPtbl_sime2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sime2017.csv' )

write.csv(SPtbl_sime2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sime2018.csv' )

write.csv(SPtbl_sime2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_sime2019.csv' )

write.csv(SEtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SE_sime.csv' )

write.csv(TOtbl_sime, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/TO_sime.csv' )

## OMBITASVIR / VERUPREVIR / RITONAVIR  + DASABUVIR   

AC_AL_AM_APtbl_ovrd <-  filter(AC_AL_AM_APtbl, AP_PRIPAL== "0604760043")

BAtbl_ovrd <- filter(BAtbl, AP_PRIPAL== "0604760043")

CEtbl_ovrd <- filter(CEtbl, AP_PRIPAL== "0604760043")

DFtbl_ovrd <- filter(DFtbl, AP_PRIPAL== "0604760043")

EStbl_ovrd <- filter(EStbl, AP_PRIPAL== "0604760043")

GOtbl_ovrd <- filter(GOtbl, AP_PRIPAL== "0604760043")

MAtbl_ovrd <- filter(MAtbl, AP_PRIPAL== "0604760043")

MGtbl_ovrd <- filter(MGtbl, AP_PRIPAL== "0604760043")

MStbl_ovrd <- filter(MStbl, AP_PRIPAL== "0604760043")

MTtbl_ovrd <- filter(MTtbl, AP_PRIPAL== "0604760043")

PAtbl_ovrd <- filter(PAtbl, AP_PRIPAL== "0604760043")

PBtbl_ovrd <- filter(PBtbl, AP_PRIPAL== "0604760043")

PEtbl_ovrd <- filter(PEtbl, AP_PRIPAL== "0604760043")

PItbl_ovrd <- filter(PItbl, AP_PRIPAL== "0604760043")

PRtbl_ovrd <- filter(PRtbl, AP_PRIPAL== "0604760043")

RJtbl_ovrd <- filter(RJtbl, AP_PRIPAL== "0604760043")

RNtbl_ovrd <- filter(RNtbl, AP_PRIPAL== "0604760043")

ROtbl_ovrd <- filter(ROtbl, AP_PRIPAL== "0604760043")

RRtbl_ovrd <- filter(RRtbl, AP_PRIPAL== "0604760043")

RStbl_ovrd <- filter(RStbl, AP_PRIPAL== "0604760043")

SCtbl_ovrd <- filter(SCtbl, AP_PRIPAL== "0604760043")

SEtbl_ovrd <- filter(SEtbl, AP_PRIPAL== "0604760043")

SPtbl_ovrd <- filter(SPtbl, AP_PRIPAL== "0604760043")

SPtbl_ovrd2015 <- filter(SPtbl2015, AP_PRIPAL== "0604760043")

SPtbl_ovrd2016 <- filter(SPtbl2016, AP_PRIPAL== "0604760043")

SPtbl_ovrd2017 <- filter(SPtbl2017, AP_PRIPAL== "0604760043")

SPtbl_ovrd2018 <- filter(SPtbl2018, AP_PRIPAL== "0604760043")

SPtbl_ovrd2019 <- filter(SPtbl2019, AP_PRIPAL== "0604760043")

TOtbl_ovrd <- filter(TOtbl, AP_PRIPAL== "0604760043")

write.csv(AC_AL_AM_APtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/AC_AL_AM_APtbl_ovrd.csv' )

write.csv(BAtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/BA_ovrd.csv' )

write.csv(CEtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/CE_ovrd.csv' )

write.csv(DFtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/DF_ovrd.csv' )

write.csv(EStbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/ES_ovrd.csv' )

write.csv(GOtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/GO_ovrd.csv' )

write.csv(MAtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MA_ovrd.csv' )

write.csv(MGtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MG_ovrd.csv' )

write.csv(MStbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MS_ovrd.csv' )

write.csv(MTtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/MT_ovrd.csv' )

write.csv(PAtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PA_ovrd.csv' )

write.csv(PBtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PB_ovrd.csv' )

write.csv(PEtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PE_ovrd.csv' )

write.csv(PItbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PI_ovrd.csv' )

write.csv(PRtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/PR_ovrd.csv' )

write.csv(RJtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RJ_ovrd.csv' )

write.csv(RNtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RN_ovrd.csv' )

write.csv(ROtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RO_ovrd.csv' )

write.csv(RRtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RR_ovrd.csv' )

write.csv(RStbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/RS_ovrd.csv' )

write.csv(SCtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SC_ovrd.csv' )

write.csv(SPtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SP_ovrd.csv' )

write.csv(SPtbl_ovrd2017, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_ovrd2017.csv' )

write.csv(SPtbl_ovrd2018, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_ovrd2018.csv' )

write.csv(SPtbl_ovrd2019, file = '/Users/mikael.lemos/Desktop/TABWIN/tabelas_tratamentos_csv_novas/SP_ovrd2019.csv' )

write.csv(SEtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/SE_ovrd.csv' )

write.csv(TOtbl_ovrd, file = '/Volumes/Mikael_backup3/produto3/dados_tabwin/APAC/tratamentos_csv/TO_ovrd.csv' )

##################################################################
######### CARREGANDO ARQUIVOS CSV DE TRATAMENTOS POR UF ##########
##################################################################

## AC_AL_AM_AP

AC_AL_AM_AP_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_alfainterf.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_alfainterf$UF   = substr(AC_AL_AM_AP_alfainterf$AP_UFMUN  ,1,2)

AC_AL_AM_AP_alfainterf$UF <- as.character(AC_AL_AM_AP_alfainterf$UF)

AC_AL_AM_AP_alfainterf$UF[AC_AL_AM_AP_alfainterf$UF == "12"] <- "AC"
AC_AL_AM_AP_alfainterf$UF[AC_AL_AM_AP_alfainterf$UF == "13"] <- "AM" 
AC_AL_AM_AP_alfainterf$UF[AC_AL_AM_AP_alfainterf$UF == "27"] <- "AL"
AC_AL_AM_AP_alfainterf$UF[AC_AL_AM_AP_alfainterf$UF == "16"] <- "AP"

AC_alfainterf <- AC_AL_AM_AP_alfainterf %>% filter( UF == "AC" )
AM_alfainterf <- AC_AL_AM_AP_alfainterf %>% filter( UF == "AM" )
AL_alfainterf <- AC_AL_AM_AP_alfainterf %>% filter( UF == "AL" )
AP_alfainterf <- AC_AL_AM_AP_alfainterf %>% filter( UF == "AP" )

AC_alfainterf$ano <- substr(AC_alfainterf$AP_CMP  ,1,4)
AL_alfainterf$ano <- substr(AL_alfainterf$AP_CMP  ,1,4)
AM_alfainterf$ano <- substr(AM_alfainterf$AP_CMP  ,1,4)
AP_alfainterf$ano <- substr(AP_alfainterf$AP_CMP  ,1,4)

AC_alfainterf$UF <- "AC"
AL_alfainterf$UF <- "AL"
AM_alfainterf$UF <- "AM"
AP_alfainterf$UF <- "AP"

AC_alfainterf$tratamentoCID <- "Alfainterferona"
AL_alfainterf$tratamentoCID <- "Alfainterferona"
AM_alfainterf$tratamentoCID <- "Alfainterferona"
AP_alfainterf$tratamentoCID <- "Alfainterferona"

AC_AL_AM_AP_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_alfapeg$UF   = substr(AC_AL_AM_AP_alfapeg$AP_UFMUN  ,1,2)

AC_AL_AM_AP_alfapeg$UF <- as.character(AC_AL_AM_AP_alfapeg$UF)

AC_AL_AM_AP_alfapeg$UF[AC_AL_AM_AP_alfapeg$UF == "12"] <- "AC"
AC_AL_AM_AP_alfapeg$UF[AC_AL_AM_AP_alfapeg$UF == "13"] <- "AM" 
AC_AL_AM_AP_alfapeg$UF[AC_AL_AM_AP_alfapeg$UF == "27"] <- "AL"
AC_AL_AM_AP_alfapeg$UF[AC_AL_AM_AP_alfapeg$UF == "16"] <- "AP"

AC_alfapeg <- AC_AL_AM_AP_alfapeg %>% filter( UF == "AC" )
AM_alfapeg <- AC_AL_AM_AP_alfapeg %>% filter( UF == "AM" )
AL_alfapeg <- AC_AL_AM_AP_alfapeg %>% filter( UF == "AL" )
AP_alfapeg <- AC_AL_AM_AP_alfapeg %>% filter( UF == "AP" )

AC_alfapeg$ano <- substr(AC_alfapeg$AP_CMP  ,1,4)
AL_alfapeg$ano <- substr(AL_alfapeg$AP_CMP  ,1,4)
AM_alfapeg$ano <- substr(AM_alfapeg$AP_CMP  ,1,4)
AP_alfapeg$ano <- substr(AP_alfapeg$AP_CMP  ,1,4)

AC_alfapeg$tratamentoCID <- "Alfapeginterferona"
AL_alfapeg$tratamentoCID <- "Alfapeginterferona"
AM_alfapeg$tratamentoCID <- "Alfapeginterferona"
AP_alfapeg$tratamentoCID <- "Alfapeginterferona"

AC_AL_AM_AP_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_riba$UF   = substr(AC_AL_AM_AP_riba$AP_UFMUN  ,1,2)

AC_AL_AM_AP_riba$UF <- as.character(AC_AL_AM_AP_riba$UF)

AC_AL_AM_AP_riba$UF[AC_AL_AM_AP_riba$UF == "12"] <- "AC"
AC_AL_AM_AP_riba$UF[AC_AL_AM_AP_riba$UF == "13"] <- "AM" 
AC_AL_AM_AP_riba$UF[AC_AL_AM_AP_riba$UF == "27"] <- "AL"
AC_AL_AM_AP_riba$UF[AC_AL_AM_AP_riba$UF == "16"] <- "AP"

AC_riba <- AC_AL_AM_AP_riba %>% filter( UF == "AC" )
AM_riba <- AC_AL_AM_AP_riba %>% filter( UF == "AM" )
AL_riba <- AC_AL_AM_AP_riba %>% filter( UF == "AL" )
AP_riba <- AC_AL_AM_AP_riba %>% filter( UF == "AP" )

AC_riba$ano <- substr(AC_riba$AP_CMP  ,1,4)
AL_riba$ano <- substr(AL_riba$AP_CMP  ,1,4)
AM_riba$ano <- substr(AM_riba$AP_CMP  ,1,4)
AP_riba$ano <- substr(AP_riba$AP_CMP  ,1,4)
  
AC_riba$tratamentoCID <- "Ribavirina"
AL_riba$tratamentoCID <- "Ribavirina"
AM_riba$tratamentoCID <- "Ribavirina"
AP_riba$tratamentoCID <- "Ribavirina"

AC_AL_AM_AP_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_boce$UF   = substr(AC_AL_AM_AP_boce$AP_UFMUN  ,1,2)

AC_AL_AM_AP_boce$UF <- as.character(AC_AL_AM_AP_boce$UF)

AC_AL_AM_AP_boce$UF[AC_AL_AM_AP_boce$UF == "12"] <- "AC"
AC_AL_AM_AP_boce$UF[AC_AL_AM_AP_boce$UF == "13"] <- "AM" 
AC_AL_AM_AP_boce$UF[AC_AL_AM_AP_boce$UF == "27"] <- "AL"
AC_AL_AM_AP_boce$UF[AC_AL_AM_AP_boce$UF == "16"] <- "AP"

AC_boce <- AC_AL_AM_AP_boce %>% filter( UF == "AC" )
AM_boce <- AC_AL_AM_AP_boce %>% filter( UF == "AM" )
AL_boce <- AC_AL_AM_AP_boce %>% filter( UF == "AL" )
AP_boce <- AC_AL_AM_AP_boce %>% filter( UF == "AP" )

AC_boce$ano <- substr(AC_boce$AP_CMP  ,1,4)
AL_boce$ano <- substr(AL_boce$AP_CMP  ,1,4)
AM_boce$ano <- substr(AM_boce$AP_CMP  ,1,4)
AP_boce$ano <- substr(AP_boce$AP_CMP  ,1,4)

AC_boce$tratamentoCID <- "Boceprevir"
AL_boce$tratamentoCID <- "Boceprevir"
AM_boce$tratamentoCID <- "Boceprevir"
AP_boce$tratamentoCID <- "Boceprevir"

AC_AL_AM_AP_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/AC_AL_AM_APtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_telo$UF   = substr(AC_AL_AM_AP_telo$AP_UFMUN  ,1,2)

AC_AL_AM_AP_telo$UF <- as.character(AC_AL_AM_AP_telo$UF)

AC_AL_AM_AP_telo$UF[AC_AL_AM_AP_telo$UF == "12"] <- "AC"
AC_AL_AM_AP_telo$UF[AC_AL_AM_AP_telo$UF == "13"] <- "AM" 
AC_AL_AM_AP_telo$UF[AC_AL_AM_AP_telo$UF == "27"] <- "AL"
AC_AL_AM_AP_telo$UF[AC_AL_AM_AP_telo$UF == "16"] <- "AP"

AC_telo <- AC_AL_AM_AP_telo %>% filter( UF == "AC" )
AM_telo <- AC_AL_AM_AP_telo %>% filter( UF == "AM" )
AL_telo <- AC_AL_AM_AP_telo %>% filter( UF == "AL" )
AP_telo <- AC_AL_AM_AP_telo %>% filter( UF == "AP" )

AC_telo$ano <- substr(AC_telo$AP_CMP  ,1,4)
AL_telo$ano <- substr(AL_telo$AP_CMP  ,1,4)
AM_telo$ano <- substr(AM_telo$AP_CMP  ,1,4)
AP_telo$ano <- substr(AP_telo$AP_CMP  ,1,4)

AC_telo$tratamentoCID <- "Telaprevir"
AL_telo$tratamentoCID <- "Telaprevir"
AM_telo$tratamentoCID <- "Telaprevir"
AP_telo$tratamentoCID <- "Telaprevir"

AC_AL_AM_AP_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/AC_AL_AM_APtbl_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_sofo$UF   = substr(AC_AL_AM_AP_sofo$AP_UFMUN  ,1,2)

AC_AL_AM_AP_sofo$UF <- as.character(AC_AL_AM_AP_sofo$UF)

AC_AL_AM_AP_sofo$UF[AC_AL_AM_AP_sofo$UF == "12"] <- "AC"
AC_AL_AM_AP_sofo$UF[AC_AL_AM_AP_sofo$UF == "13"] <- "AM" 
AC_AL_AM_AP_sofo$UF[AC_AL_AM_AP_sofo$UF == "27"] <- "AL"
AC_AL_AM_AP_sofo$UF[AC_AL_AM_AP_sofo$UF == "16"] <- "AP"

AC_sofo <- AC_AL_AM_AP_sofo %>% filter( UF == "AC" )
AM_sofo <- AC_AL_AM_AP_sofo %>% filter( UF == "AM" )
AL_sofo <- AC_AL_AM_AP_sofo %>% filter( UF == "AL" )
AP_sofo <- AC_AL_AM_AP_sofo %>% filter( UF == "AP" )

AC_sofo$ano <- substr(AC_sofo$AP_CMP  ,1,4)
AL_sofo$ano <- substr(AL_sofo$AP_CMP  ,1,4)
AM_sofo$ano <- substr(AM_sofo$AP_CMP  ,1,4)
AP_sofo$ano <- substr(AP_sofo$AP_CMP  ,1,4)

AC_sofo$tratamentoCID <- "Sofosbuvir"
AL_sofo$tratamentoCID <- "Sofosbuvir"
AM_sofo$tratamentoCID <- "Sofosbuvir"
AP_sofo$tratamentoCID <- "Sofosbuvir"

AC_AL_AM_AP_sofo$tratamento <- "sofosbuvir"

AC_AL_AM_AP_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/AC_AL_AM_APtbl_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_dacla$UF   = substr(AC_AL_AM_AP_dacla$AP_UFMUN  ,1,2)

AC_AL_AM_AP_dacla$UF <- as.character(AC_AL_AM_AP_dacla$UF)

AC_AL_AM_AP_dacla$UF[AC_AL_AM_AP_dacla$UF == "12"] <- "AC"
AC_AL_AM_AP_dacla$UF[AC_AL_AM_AP_dacla$UF == "13"] <- "AM" 
AC_AL_AM_AP_dacla$UF[AC_AL_AM_AP_dacla$UF == "27"] <- "AL"
AC_AL_AM_AP_dacla$UF[AC_AL_AM_AP_dacla$UF == "16"] <- "AP"

AC_dacla <- AC_AL_AM_AP_dacla %>% filter( UF == "AC" )
AM_dacla <- AC_AL_AM_AP_dacla %>% filter( UF == "AM" )
AL_dacla <- AC_AL_AM_AP_dacla %>% filter( UF == "AL" )
AP_dacla <- AC_AL_AM_AP_dacla %>% filter( UF == "AP" )

AC_dacla$ano <- substr(AC_dacla$AP_CMP  ,1,4)
AL_dacla$ano <- substr(AL_dacla$AP_CMP  ,1,4)
AM_dacla$ano <- substr(AM_dacla$AP_CMP  ,1,4)
AP_dacla$ano <- substr(AP_dacla$AP_CMP  ,1,4)

AC_dacla$tratamentoCID <- "Daclatasvir"
AL_dacla$tratamentoCID <- "Daclatasvir"
AM_dacla$tratamentoCID <- "Daclatasvir"
AP_dacla$tratamentoCID <- "Daclatasvir"

AC_AL_AM_AP_dacla$tratamento <- "daclatasvir"

AC_AL_AM_AP_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/AC_AL_AM_APtbl_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_sime$UF   = substr(AC_AL_AM_AP_sime$AP_UFMUN  ,1,2)

AC_AL_AM_AP_sime$UF <- as.character(AC_AL_AM_AP_sime$UF)

AC_AL_AM_AP_sime$UF[AC_AL_AM_AP_sime$UF == "12"] <- "AC"
AC_AL_AM_AP_sime$UF[AC_AL_AM_AP_sime$UF == "13"] <- "AM" 
AC_AL_AM_AP_sime$UF[AC_AL_AM_AP_sime$UF == "27"] <- "AL"
AC_AL_AM_AP_sime$UF[AC_AL_AM_AP_sime$UF == "16"] <- "AP"

AC_sime <- AC_AL_AM_AP_sime %>% filter( UF == "AC" )
AM_sime <- AC_AL_AM_AP_sime %>% filter( UF == "AM" )
AL_sime <- AC_AL_AM_AP_sime %>% filter( UF == "AL" )
AP_sime <- AC_AL_AM_AP_sime %>% filter( UF == "AP" )

AC_sime$ano <- substr(AC_sime$AP_CMP  ,1,4)
AL_sime$ano <- substr(AL_sime$AP_CMP  ,1,4)
AM_sime$ano <- substr(AM_sime$AP_CMP  ,1,4)
AP_sime$ano <- substr(AP_sime$AP_CMP  ,1,4)

AC_sime$tratamentoCID <- "Simeprevir"
AL_sime$tratamentoCID <- "Simeprevir"
AM_sime$tratamentoCID <- "Simeprevir"
AP_sime$tratamentoCID <- "Simeprevir"

AC_AL_AM_AP_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/AC_AL_AM_APtbl_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

AC_AL_AM_AP_ovrd$UF   = substr(AC_AL_AM_AP_ovrd$AP_UFMUN  ,1,2)

AC_AL_AM_AP_ovrd$UF <- as.character(AC_AL_AM_AP_ovrd$UF)

AC_AL_AM_AP_ovrd$UF[AC_AL_AM_AP_ovrd$UF == "12"] <- "AC"
AC_AL_AM_AP_ovrd$UF[AC_AL_AM_AP_ovrd$UF == "13"] <- "AM" 
AC_AL_AM_AP_ovrd$UF[AC_AL_AM_AP_ovrd$UF == "27"] <- "AL"
AC_AL_AM_AP_ovrd$UF[AC_AL_AM_AP_ovrd$UF == "16"] <- "AP"

AC_ovrd <- AC_AL_AM_AP_ovrd %>% filter( UF == "AC" )
AM_ovrd <- AC_AL_AM_AP_ovrd %>% filter( UF == "AM" )
AL_ovrd <- AC_AL_AM_AP_ovrd %>% filter( UF == "AL" )
AP_ovrd <- AC_AL_AM_AP_ovrd %>% filter( UF == "AP" )

AC_ovrd$ano <- substr(AC_ovrd$AP_CMP  ,1,4)
AL_ovrd$ano <- substr(AL_ovrd$AP_CMP  ,1,4)
AM_ovrd$ano <- substr(AM_ovrd$AP_CMP  ,1,4)
AP_ovrd$ano <- substr(AP_ovrd$AP_CMP  ,1,4)

AC_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"
AL_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"
AM_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"
AP_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## BA

BA_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

BA_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

BA_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

BA_boce <-  read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

BA_telo <-  read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/BAtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

BA_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/BA_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

BA_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/BA_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

BA_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/CE_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

BA_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/BA_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")


## CE

CE_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
CE_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
CE_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
CE_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
CE_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/CEtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

CE_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/CE_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

CE_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/CE_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

CE_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/CE_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

CE_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/CE_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## DF 

DF_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
DF_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
DF_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
DF_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
DF_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/DFtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
DF_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/DF_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

DF_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/ES_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

DF_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/CE_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

DF_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/DF_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## ES

ES_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
ES_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

ES_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

ES_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
ES_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/EStbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
ES_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/ES_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

ES_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/ES_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

ES_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/DF_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

ES_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/ES_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## GO

GO_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
GO_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
GO_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
GO_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
GO_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/GOtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
GO_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/GO_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

GO_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/GO_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

GO_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/GO_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

GO_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/GO_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## MA

MA_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MA_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MA_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MA_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MA_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MAtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MA_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MA_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MA_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MA_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MA_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MA_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MA_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MG_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## MG

MG_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MG_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MG_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MG_boce <-  read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MG_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MGtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MG_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MG_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MG_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MG_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MG_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MG_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MG_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MG_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## MS

MS_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MS_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MS_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MS_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MS_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MStbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MS_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MS_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MS_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MS_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MS_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MS_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MS_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MS_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## MT

MT_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MT_alfapeg <-read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MT_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MT_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
MT_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/MTtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MT_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MT_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MT_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MT_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MT_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MT_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

MT_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/MT_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## PA

PA_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PA_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PA_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PA_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PA_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PAtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PA_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PA_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PA_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PA_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PA_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PA_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PA_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PA_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## PB

PB_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PB_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PB_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PB_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PB_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PBtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PB_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PB_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PB_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PB_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PB_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PB_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PB_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PB_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## PE

PE_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PE_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PE_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PE_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PE_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PEtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PE_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PE_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PE_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PE_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PE_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PE_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PE_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PE_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## PI

PI_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PI_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PI_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PI_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PI_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PItbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PI_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PI_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PI_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PI_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PI_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PI_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PI_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PI_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## PR

PR_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PR_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PR_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PR_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
PR_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/PRtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PR_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PR_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PR_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PR_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PR_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PR_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

PR_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/PR_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## RJ

RJ_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
RJ_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
RJ_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
RJ_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  
RJ_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RJtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RJ_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RJ_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RJ_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RJ_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RJ_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RJ_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RJ_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RJ_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## RN

RN_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RN_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RN_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RN_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RN_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RNtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RN_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RN_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RN_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RN_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RN_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RN_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RN_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RN_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## RO

RO_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RO_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RO_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RO_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RO_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/ROtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RO_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RO_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RO_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RO_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RO_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RO_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RO_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RO_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## RR

RR_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RR_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RR_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RR_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RR_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RRtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RR_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RR_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RR_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RR_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RR_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RR_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RR_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RR_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## RS 

RS_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RS_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RS_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RS_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RS_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/RStbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RS_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RS_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RS_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RS_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RS_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RS_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

RS_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/RS_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## SC 

SC_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SC_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SC_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SC_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SC_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SCtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SC_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/SC_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SC_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/SC_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SC_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/SC_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SC_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/SC_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")


## SE

SE_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SE_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SE_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SE_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SE_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SEtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SE_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/SE_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SE_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/SE_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SE_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/SE_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SE_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/SE_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

## SP

SP_alfainterf2008 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2008.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2009 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2009.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2010 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2010.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2011 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2011.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2012 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2012.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2013 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2013.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2014 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2014.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2015 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2015.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2016 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2016.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2017 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2017.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfainterf2018 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfainterf2018.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SP_alfainterf2011 <- select(SP_alfainterf2011, -AP_NATJUR)
SP_alfainterf2014 <- select(SP_alfainterf2014, -AP_NATJUR)
SP_alfainterf2016 <- select(SP_alfainterf2016, -AP_NATJUR)
SP_alfainterf2017 <- select(SP_alfainterf2017, -AP_NATJUR)
SP_alfainterf2018 <- select(SP_alfainterf2018, -AP_NATJUR)

SP_alfainterf <- do.call("rbind", list(SP_alfainterf2008, SP_alfainterf2009, SP_alfainterf2010, SP_alfainterf2011, SP_alfainterf2012, SP_alfainterf2013, SP_alfainterf2014, SP_alfainterf2015, SP_alfainterf2016, SP_alfainterf2017, SP_alfainterf2018))

SP_alfapeg2010 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2010.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2011 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2011.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2012 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2012.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2013 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2013.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2014 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2014.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2015 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2015.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2016 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2016.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2017 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2017.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2018 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2018.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_alfapeg2019 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_alfapeg2019.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SP_alfapeg2011 <- select(SP_alfapeg2011, -AP_NATJUR)
SP_alfapeg2014 <- select(SP_alfapeg2014, -AP_NATJUR)
SP_alfapeg2016 <- select(SP_alfapeg2016, -AP_NATJUR)
SP_alfapeg2017 <- select(SP_alfapeg2017, -AP_NATJUR)
SP_alfapeg2018 <- select(SP_alfapeg2018, -AP_NATJUR)
SP_alfapeg2019 <- select(SP_alfapeg2019, -AP_NATJUR)

SP_alfapeg <- do.call("rbind", list( SP_alfapeg2010, SP_alfapeg2011, SP_alfapeg2012, SP_alfapeg2013, SP_alfapeg2014, SP_alfapeg2015, SP_alfapeg2016, SP_alfapeg2017, SP_alfapeg2018, SP_alfapeg2019))

SP_riba2010 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2010.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2011 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2011.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2012 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2012.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2013 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2013.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2014 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2014.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2015 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2015.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2016 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2016.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2017 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2017.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2018 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2018.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_riba2019 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_riba2019.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SP_riba2011 <- select(SP_riba2011, -AP_NATJUR)
SP_riba2014 <- select(SP_riba2014, -AP_NATJUR)
SP_riba2016 <- select(SP_riba2016, -AP_NATJUR)
SP_riba2017 <- select(SP_riba2017, -AP_NATJUR)
SP_riba2018 <- select(SP_riba2018, -AP_NATJUR)
SP_riba2019 <- select(SP_riba2019, -AP_NATJUR)

SP_riba <- do.call("rbind", list( SP_riba2010, SP_riba2011, SP_riba2012, SP_riba2013, SP_riba2014, SP_riba2015, SP_riba2016, SP_riba2017, SP_riba2018, SP_riba2019))

SP_boce2013 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_boce2013.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_boce2014 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_boce2014.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_boce2015 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_boce2015.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_boce2016 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_boce2016.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SP_boce2014 <- select(SP_boce2014, -AP_NATJUR)
SP_boce2016 <- select(SP_boce2016, -AP_NATJUR)

SP_boce <- do.call("rbind", list( SP_boce2013, SP_boce2014, SP_boce2015, SP_boce2016))

SP_telo2013 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_telo2013.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_telo2014 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_telo2014.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_telo2015 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SPtbl_telo2015.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

SP_telo2014 <- select(SP_telo2014, -AP_NATJUR)

SP_telo <- do.call("rbind", list( SP_telo2013, SP_telo2014, SP_telo2015))

SP_sofo2015 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sofo2015.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_sofo2016 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sofo2016.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_sofo2017 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sofo2017.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_sofo2018 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sofo2018.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_sofo2019 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sofo2019.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")


SP_sofo2016 <- select(SP_sofo2016, -AP_NATJUR)
SP_sofo2017 <- select(SP_sofo2017, -AP_NATJUR)
SP_sofo2018 <- select(SP_sofo2018, -AP_NATJUR)
SP_sofo2019 <- select(SP_sofo2019, -AP_NATJUR)

SP_sofo <- do.call("rbind", list( SP_sofo2015, SP_sofo2016, SP_sofo2017, SP_sofo2018, SP_sofo2019))

SP_dacla2015 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_dacla2015.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_dacla2016 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_dacla2016.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_dacla2017 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_dacla2017.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_dacla2018 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_dacla2018.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_dacla2019 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_dacla2019.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")


SP_dacla2016 <- select(SP_dacla2016, -AP_NATJUR)
SP_dacla2017 <- select(SP_dacla2017, -AP_NATJUR)
SP_dacla2018 <- select(SP_dacla2018, -AP_NATJUR)
SP_dacla2019 <- select(SP_dacla2019, -AP_NATJUR)

SP_dacla <- do.call("rbind", list( SP_dacla2015, SP_dacla2016, SP_dacla2017, SP_dacla2018, SP_dacla2019))

SP_sime2015 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sime2015.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_sime2016 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sime2016.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_sime2017 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sime2017.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_sime2018 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sime2018.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_sime2019 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_sime2019.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")


SP_sime2016 <- select(SP_sime2016, -AP_NATJUR)
SP_sime2017 <- select(SP_sime2017, -AP_NATJUR)
SP_sime2018 <- select(SP_sime2018, -AP_NATJUR)
SP_sime2019 <- select(SP_sime2019, -AP_NATJUR)

SP_sime <- do.call("rbind", list( SP_sime2015, SP_sime2016, SP_sime2017, SP_sime2018, SP_sime2019))

SP_ovrd2017 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_ovrd2017.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_ovrd2018 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_ovrd2018.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
SP_ovrd2019 <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/SP_ovrd2019.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")


SP_ovrd2017 <- select(SP_ovrd2017, -AP_NATJUR)
SP_ovrd2018 <- select(SP_ovrd2018, -AP_NATJUR)
SP_ovrd2019 <- select(SP_ovrd2019, -AP_NATJUR)

SP_ovrd <- do.call("rbind", list( SP_ovrd2017, SP_ovrd2018, SP_ovrd2019))

## TO

TO_alfainterf <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_alfainterf.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

TO_alfapeg <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_alfapeg.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

TO_riba <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_riba.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

TO_boce <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_boce.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

TO_telo <- read.csv("/Volumes/MIKAEL/produto3/tabelas_tratamentos_csv_novas/TOtbl_telo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

TO_sofo <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/TO_sofo.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

TO_dacla <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/TO_dacla.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

TO_sime <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/TO_sime.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

TO_ovrd <- read.csv("/Volumes/MIKAEL/produto3/tratamentos_csv/TO_ovrd.csv" , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

####################################################################
######## Acrescentando colunas - tratamento/CID, ano e UF ##########
####################################################################

## AC_AL_AM_AP

AC_alfainterf$UF <- "AC"
AL_alfainterf$UF <- "AL"
AM_alfainterf$UF <- "AM"
AP_alfainterf$UF <- "AP"

AC_alfapeg$UF <- "AC"
AL_alfapeg$UF <- "AL"
AM_alfapeg$UF <- "AM"
AP_alfapeg$UF <- "AP"

AC_riba$UF <- "AC"
AL_riba$UF <- "AL"
AM_riba$UF <- "AM"
AP_riba$UF <- "AP"

AC_boce$UF <- "AC"
AL_boce$UF <- "AL"
AM_boce$UF <- "AM"
AP_boce$UF <- "AP"

AC_telo$UF <- "AC"
AL_telo$UF <- "AL"
AM_telo$UF <- "AM"
AP_telo$UF <- "AP"

AC_sime$UF <- "AC"
AL_sime$UF <- "AL"
AM_sime$UF <- "AM"
AP_sime$UF <- "AP"

AC_sofo$UF <- "AC"
AL_sofo$UF <- "AL"
AM_sofo$UF <- "AM"
AP_sofo$UF <- "AP"

AC_dacla$UF <- "AC"
AL_dacla$UF <- "AL"
AM_dacla$UF <- "AM"
AP_dacla$UF <- "AP"

AC_ovrd$UF <- "AC"
AL_ovrd$UF <- "AL"
AM_ovrd$UF <- "AM"
AP_ovrd$UF <- "AP"

AC_alfainterf$tratamentoCID <- "Alfainterferona"
AL_alfainterf$tratamentoCID <- "Alfainterferona"
AM_alfainterf$tratamentoCID <- "Alfainterferona"
AP_alfainterf$tratamentoCID <- "Alfainterferona"

AC_alfapeg$tratamentoCID <- "Alfapeginterferona"
AL_alfapeg$tratamentoCID <- "Alfapeginterferona"
AM_alfapeg$tratamentoCID <- "Alfapeginterferona"
AP_alfapeg$tratamentoCID <- "Alfapeginterferona"

AC_boce$tratamentoCID <- "Boceprevir"
AL_boce$tratamentoCID <- "Boceprevir"
AM_boce$tratamentoCID <- "Boceprevir"
AP_boce$tratamentoCID <- "Boceprevir"

AC_sime$tratamentoCID <- "Simeprevir"
AL_sime$tratamentoCID <- "Simeprevir"
AM_sime$tratamentoCID <- "Simeprevir"
AP_sime$tratamentoCID <- "Simeprevir"

AC_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"
AL_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"
AM_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"
AP_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

AC_dacla$tratamentoCID <- "Daclatasvir"
AL_dacla$tratamentoCID <- "Daclatasvir"
AM_dacla$tratamentoCID <- "Daclatasvir"
AP_dacla$tratamentoCID <- "Daclatasvir"

AC_sofo$tratamentoCID <- "Sofosbuvir"
AL_sofo$tratamentoCID <- "Sofosbuvir"
AM_sofo$tratamentoCID <- "Sofosbuvir"
AP_sofo$tratamentoCID <- "Sofosbuvir"

AC_telo$tratamentoCID <- "Telaprevir"
AL_telo$tratamentoCID <- "Telaprevir"
AM_telo$tratamentoCID <- "Telaprevir"
AP_telo$tratamentoCID <- "Telaprevir"

AC_riba$tratamentoCID <- "Ribavirina"
AL_riba$tratamentoCID <- "Ribavirina"
AM_riba$tratamentoCID <- "Ribavirina"
AP_riba$tratamentoCID <- "Ribavirina"

## BA

BA_alfainterf$UF <- "BA"
BA_alfainterf$ano <- substr(BA_alfainterf$AP_CMP  ,1,4)
BA_alfainterf$tratamentoCID <- "Alfainterferona"

BA_alfapeg$UF <- "BA"
BA_alfapeg$ano <- substr(BA_alfapeg$AP_CMP  ,1,4)
BA_alfapeg$tratamentoCID <- "Alfapeginterferona"

BA_riba$UF <- "BA"
BA_riba$ano <- substr(BA_riba$AP_CMP  ,1,4)
BA_riba$tratamentoCID <- "Ribavirina"

BA_boce$UF <- "BA"
BA_boce$ano <- substr(BA_boce$AP_CMP  ,1,4)
BA_boce$tratamentoCID <- "Boceprevir"

BA_telo$UF <- "BA"
BA_telo$ano <- substr(BA_telo$AP_CMP  ,1,4)
BA_telo$tratamentoCID <- "Telaprevir"

BA_sofo$UF <- "BA"
BA_sofo$ano <- substr(BA_sofo$AP_CMP  ,1,4)
BA_sofo$tratamentoCID <- "Sofosbuvir"

BA_dacla$UF <- "BA"
BA_dacla$ano <- substr(BA_dacla$AP_CMP  ,1,4)
BA_dacla$tratamentoCID <- "Daclatasvir"

BA_sime$UF <- "BA"
BA_sime$ano <- substr(BA_sime$AP_CMP  ,1,4)
BA_sime$tratamentoCID <- "Simeprevir"

BA_ovrd$UF <- "BA"
BA_ovrd$ano <- substr(BA_ovrd$AP_CMP  ,1,4)
BA_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"


## CE

CE_alfainterf$UF <- "CE"
CE_alfainterf$ano <- substr(CE_alfainterf$AP_CMP  ,1,4)
CE_alfainterf$tratamentoCID <- "Alfainterferona"

CE_alfapeg$UF <- "CE"
CE_alfapeg$ano <- substr(CE_alfapeg$AP_CMP  ,1,4)
CE_alfapeg$tratamentoCID <- "Alfapeginterferona"

CE_riba$UF <- "CE"
CE_riba$ano <- substr(CE_riba$AP_CMP  ,1,4)
CE_riba$tratamentoCID <- "Ribavirina"

CE_boce$UF <- "CE"
CE_boce$ano <- substr(CE_boce$AP_CMP  ,1,4)
CE_boce$tratamentoCID <- "Boceprevir"

CE_telo$UF <- "CE"
CE_telo$ano <- substr(CE_telo$AP_CMP  ,1,4)
CE_telo$tratamentoCID <- "Telaprevir"

CE_sofo$UF <- "CE"
CE_sofo$ano <- substr(CE_sofo$AP_CMP  ,1,4)
CE_sofo$tratamentoCID <- "Sofosbuvir"

CE_dacla$UF <- "CE"
CE_dacla$ano <- substr(CE_dacla$AP_CMP  ,1,4)
CE_dacla$tratamentoCID <- "Daclatasvir"

CE_sime$UF <- "CE"
CE_sime$ano <- substr(CE_sime$AP_CMP  ,1,4)
CE_sime$tratamentoCID <- "Simeprevir"

CE_ovrd$UF <- "CE"
CE_ovrd$ano <- substr(CE_ovrd$AP_CMP  ,1,4)
CE_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## DF

DF_alfainterf$UF <- "DF"
DF_alfainterf$ano <- substr(DF_alfainterf$AP_CMP  ,1,4)
DF_alfainterf$tratamentoCID <- "Alfainterferona"

DF_alfapeg$UF <- "DF"
DF_alfapeg$ano <- substr(DF_alfapeg$AP_CMP  ,1,4)
DF_alfapeg$tratamentoCID <- "Alfapeginterferona"

DF_riba$UF <- "DF"
DF_riba$ano <- substr(DF_riba$AP_CMP  ,1,4)
DF_riba$tratamentoCID <- "Ribavirina"

DF_boce$UF <- "DF"
DF_boce$ano <- substr(DF_boce$AP_CMP  ,1,4)
DF_boce$tratamentoCID <- "Boceprevir"

DF_telo$UF <- "DF"
DF_telo$ano <- substr(DF_telo$AP_CMP  ,1,4)
DF_telo$tratamentoCID <- "Telaprevir"

DF_sofo$UF <- "DF"
DF_sofo$ano <- substr(DF_sofo$AP_CMP  ,1,4)
DF_sofo$tratamentoCID <- "Sofosbuvir"

DF_dacla$UF <- "DF"
DF_dacla$ano <- substr(DF_dacla$AP_CMP  ,1,4)
DF_dacla$tratamentoCID <- "Daclatasvir"

DF_sime$UF <- "DF"
DF_sime$ano <- substr(DF_sime$AP_CMP  ,1,4)
DF_sime$tratamentoCID <- "Simeprevir"

DF_ovrd$UF <- "DF"
DF_ovrd$ano <- substr(DF_ovrd$AP_CMP  ,1,4)
DF_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## ES

ES_alfainterf$UF <- "ES"
ES_alfainterf$ano <- substr(ES_alfainterf$AP_CMP  ,1,4)
ES_alfainterf$tratamentoCID <- "Alfainterferona"

ES_alfapeg$UF <- "ES"
ES_alfapeg$ano <- substr(ES_alfapeg$AP_CMP  ,1,4)
ES_alfapeg$tratamentoCID <- "Alfapeginterferona"

ES_riba$UF <- "ES"
ES_riba$ano <- substr(ES_riba$AP_CMP  ,1,4)
ES_riba$tratamentoCID <- "Ribavirina"

ES_boce$UF <- "ES"
ES_boce$ano <- substr(ES_boce$AP_CMP  ,1,4)
ES_boce$tratamentoCID <- "Boceprevir"

ES_telo$UF <- "ES"
ES_telo$ano <- substr(ES_telo$AP_CMP  ,1,4)
ES_telo$tratamentoCID <- "Telaprevir"

ES_sofo$UF <- "ES"
ES_sofo$ano <- substr(ES_sofo$AP_CMP  ,1,4)
ES_sofo$tratamentoCID <- "Sofosbuvir"

ES_dacla$UF <- "ES"
ES_dacla$ano <- substr(ES_dacla$AP_CMP  ,1,4)
ES_dacla$tratamentoCID <- "Daclatasvir"

ES_sime$UF <- "ES"
ES_sime$ano <- substr(ES_sime$AP_CMP  ,1,4)
ES_sime$tratamentoCID <- "Simeprevir"

ES_ovrd$UF <- "ES"
ES_ovrd$ano <- substr(ES_ovrd$AP_CMP  ,1,4)
ES_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## GO

GO_alfainterf$UF <- "GO"
GO_alfainterf$ano <- substr(GO_alfainterf$AP_CMP  ,1,4)
GO_alfainterf$tratamentoCID <- "Alfainterferona"

GO_alfapeg$UF <- "GO"
GO_alfapeg$ano <- substr(GO_alfapeg$AP_CMP  ,1,4)
GO_alfapeg$tratamentoCID <- "Alfapeginterferona"

GO_riba$UF <- "GO"
GO_riba$ano <- substr(GO_riba$AP_CMP  ,1,4)
GO_riba$tratamentoCID <- "Ribavirina"

GO_boce$UF <- "GO"
GO_boce$ano <- substr(GO_boce$AP_CMP  ,1,4)
GO_boce$tratamentoCID <- "Boceprevir"

GO_telo$UF <- "GO"
GO_telo$ano <- substr(GO_telo$AP_CMP  ,1,4)
GO_telo$tratamentoCID <- "Telaprevir"

GO_sofo$UF <- "GO"
GO_sofo$ano <- substr(GO_sofo$AP_CMP  ,1,4)
GO_sofo$tratamentoCID <- "Sofosbuvir"

GO_dacla$UF <- "GO"
GO_dacla$ano <- substr(GO_dacla$AP_CMP  ,1,4)
GO_dacla$tratamentoCID <- "Daclatasvir"

GO_sime$UF <- "GO"
GO_sime$ano <- substr(GO_sime$AP_CMP  ,1,4)
GO_sime$tratamentoCID <- "Simeprevir"

GO_ovrd$UF <- "GO"
GO_ovrd$ano <- substr(GO_ovrd$AP_CMP  ,1,4)
GO_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## MA

MA_alfainterf$UF <- "MA"
MA_alfainterf$ano <- substr(MA_alfainterf$AP_CMP  ,1,4)
MA_alfainterf$tratamentoCID <- "Alfainterferona"

MA_alfapeg$UF <- "MA"
MA_alfapeg$ano <- substr(MA_alfapeg$AP_CMP  ,1,4)
MA_alfapeg$tratamentoCID <- "Alfapeginterferona"

MA_riba$UF <- "MA"
MA_riba$ano <- substr(MA_riba$AP_CMP  ,1,4)
MA_riba$tratamentoCID <- "Ribavirina"

MA_boce$UF <- "MA"
MA_boce$ano <- substr(MA_boce$AP_CMP  ,1,4)
MA_boce$tratamentoCID <- "Boceprevir"

MA_telo$UF <- "MA"
MA_telo$ano <- substr(MA_telo$AP_CMP  ,1,4)
MA_telo$tratamentoCID <- "Telaprevir"

MA_sofo$UF <- "MA"
MA_sofo$ano <- substr(MA_sofo$AP_CMP  ,1,4)
MA_sofo$tratamentoCID <- "Sofosbuvir"

MA_dacla$UF <- "MA"
MA_dacla$ano <- substr(MA_dacla$AP_CMP  ,1,4)
MA_dacla$tratamentoCID <- "Daclatasvir"

MA_sime$UF <- "MA"
MA_sime$ano <- substr(MA_sime$AP_CMP  ,1,4)
MA_sime$tratamentoCID <- "Simeprevir"

MA_ovrd$UF <- "MA"
MA_ovrd$ano <- substr(MA_ovrd$AP_CMP  ,1,4)
MA_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## MG

MG_alfainterf$UF <- "MG"
MG_alfainterf$ano <- substr(MG_alfainterf$AP_CMP  ,1,4)
MG_alfainterf$tratamentoCID <- "Alfainterferona"

MG_alfapeg$UF <- "MG"
MG_alfapeg$ano <- substr(MG_alfapeg$AP_CMP  ,1,4)
MG_alfapeg$tratamentoCID <- "Alfapeginterferona"

MG_riba$UF <- "MG"
MG_riba$ano <- substr(MG_riba$AP_CMP  ,1,4)
MG_riba$tratamentoCID <- "Ribavirina"

MG_boce$UF <- "MG"
MG_boce$ano <- substr(MG_boce$AP_CMP  ,1,4)
MG_boce$tratamentoCID <- "Boceprevir"

MG_telo$UF <- "MG"
MG_telo$ano <- substr(MG_telo$AP_CMP  ,1,4)
MG_telo$tratamentoCID <- "Telaprevir"

MG_sofo$UF <- "MG"
MG_sofo$ano <- substr(MG_sofo$AP_CMP  ,1,4)
MG_sofo$tratamentoCID <- "Sofosbuvir"

MG_dacla$UF <- "MG"
MG_dacla$ano <- substr(MG_dacla$AP_CMP  ,1,4)
MG_dacla$tratamentoCID <- "Daclatasvir"

MG_sime$UF <- "MG"
MG_sime$ano <- substr(MG_sime$AP_CMP  ,1,4)
MG_sime$tratamentoCID <- "Simeprevir"

MG_ovrd$UF <- "MG"
MG_ovrd$ano <- substr(MG_ovrd$AP_CMP  ,1,4)
MG_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## MS

MS_alfainterf$UF <- "MS"
MS_alfainterf$ano <- substr(MS_alfainterf$AP_CMP  ,1,4)
MS_alfainterf$tratamentoCID <- "Alfainterferona"

MS_alfapeg$UF <- "MS"
MS_alfapeg$ano <- substr(MS_alfapeg$AP_CMP  ,1,4)
MS_alfapeg$tratamentoCID <- "Alfapeginterferona"

MS_riba$UF <- "MS"
MS_riba$ano <- substr(MS_riba$AP_CMP  ,1,4)
MS_riba$tratamentoCID <- "Ribavirina"

MS_boce$UF <- "MS"
MS_boce$ano <- substr(MS_boce$AP_CMP  ,1,4)
MS_boce$tratamentoCID <- "Boceprevir"

MS_telo$UF <- "MS"
MS_telo$ano <- substr(MS_telo$AP_CMP  ,1,4)
MS_telo$tratamentoCID <- "Telaprevir"

MS_sofo$UF <- "MS"
MS_sofo$ano <- substr(MS_sofo$AP_CMP  ,1,4)
MS_sofo$tratamentoCID <- "Sofosbuvir"

MS_dacla$UF <- "MS"
MS_dacla$ano <- substr(MS_dacla$AP_CMP  ,1,4)
MS_dacla$tratamentoCID <- "Daclatasvir"

MS_sime$UF <- "MS"
MS_sime$ano <- substr(MS_sime$AP_CMP  ,1,4)
MS_sime$tratamentoCID <- "Simeprevir"

MS_ovrd$UF <- "MS"
MS_ovrd$ano <- substr(MS_ovrd$AP_CMP  ,1,4)
MS_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## MT

MT_alfainterf$UF <- "MT"
MT_alfainterf$ano <- substr(MT_alfainterf$AP_CMP  ,1,4)
MT_alfainterf$tratamentoCID <- "Alfainterferona"

MT_alfapeg$UF <- "MT"
MT_alfapeg$ano <- substr(MT_alfapeg$AP_CMP  ,1,4)
MT_alfapeg$tratamentoCID <- "Alfapeginterferona"

MT_riba$UF <- "MT"
MT_riba$ano <- substr(MT_riba$AP_CMP  ,1,4)
MT_riba$tratamentoCID <- "Ribavirina"

MT_boce$UF <- "MT"
MT_boce$ano <- substr(MT_boce$AP_CMP  ,1,4)
MT_boce$tratamentoCID <- "Boceprevir"

MT_telo$UF <- "MT"
MT_telo$ano <- substr(MT_telo$AP_CMP  ,1,4)
MT_telo$tratamentoCID <- "Telaprevir"

MT_sofo$UF <- "MT"
MT_sofo$ano <- substr(MT_sofo$AP_CMP  ,1,4)
MT_sofo$tratamentoCID <- "Sofosbuvir"

MT_dacla$UF <- "MT"
MT_dacla$ano <- substr(MT_dacla$AP_CMP  ,1,4)
MT_dacla$tratamentoCID <- "Daclatasvir"

MT_sime$UF <- "MT"
MT_sime$ano <- substr(MT_sime$AP_CMP  ,1,4)
MT_sime$tratamentoCID <- "Simeprevir"

MT_ovrd$UF <- "MT"
MT_ovrd$ano <- substr(MT_ovrd$AP_CMP  ,1,4)
MT_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## PA

PA_alfainterf$UF <- "PA"
PA_alfainterf$ano <- substr(PA_alfainterf$AP_CMP  ,1,4)
PA_alfainterf$tratamentoCID <- "Alfainterferona"

PA_alfapeg$UF <- "PA"
PA_alfapeg$ano <- substr(PA_alfapeg$AP_CMP  ,1,4)
PA_alfapeg$tratamentoCID <- "Alfapeginterferona"

PA_riba$UF <- "PA"
PA_riba$ano <- substr(PA_riba$AP_CMP  ,1,4)
PA_riba$tratamentoCID <- "Ribavirina"

PA_boce$UF <- "PA"
PA_boce$ano <- substr(PA_boce$AP_CMP  ,1,4)
PA_boce$tratamentoCID <- "Boceprevir"

PA_telo$UF <- "PA"
PA_telo$ano <- substr(PA_telo$AP_CMP  ,1,4)
PA_telo$tratamentoCID <- "Telaprevir"

PA_sofo$UF <- "PA"
PA_sofo$ano <- substr(PA_sofo$AP_CMP  ,1,4)
PA_sofo$tratamentoCID <- "Sofosbuvir"

PA_dacla$UF <- "PA"
PA_dacla$ano <- substr(PA_dacla$AP_CMP  ,1,4)
PA_dacla$tratamentoCID <- "Daclatasvir"

PA_sime$UF <- "PA"
PA_sime$ano <- substr(PA_sime$AP_CMP  ,1,4)
PA_sime$tratamentoCID <- "Simeprevir"

PA_ovrd$UF <- "PA"
PA_ovrd$ano <- substr(PA_ovrd$AP_CMP  ,1,4)
PA_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## PB

PB_alfainterf$UF <- "PB"
PB_alfainterf$ano <- substr(PB_alfainterf$AP_CMP  ,1,4)
PB_alfainterf$tratamentoCID <- "Alfainterferona"

PB_alfapeg$UF <- "PB"
PB_alfapeg$ano <- substr(PB_alfapeg$AP_CMP  ,1,4)
PB_alfapeg$tratamentoCID <- "Alfapeginterferona"

PB_riba$UF <- "PB"
PB_riba$ano <- substr(PB_riba$AP_CMP  ,1,4)
PB_riba$tratamentoCID <- "Ribavirina"

PB_boce$UF <- "PB"
PB_boce$ano <- substr(PB_boce$AP_CMP  ,1,4)
PB_boce$tratamentoCID <- "Boceprevir"

PB_telo$UF <- "PB"
PB_telo$ano <- substr(PB_telo$AP_CMP  ,1,4)
PB_telo$tratamentoCID <- "Telaprevir"

PB_sofo$UF <- "PB"
PB_sofo$ano <- substr(PB_sofo$AP_CMP  ,1,4)
PB_sofo$tratamentoCID <- "Sofosbuvir"

PB_dacla$UF <- "PB"
PB_dacla$ano <- substr(PB_dacla$AP_CMP  ,1,4)
PB_dacla$tratamentoCID <- "Daclatasvir"

PB_sime$UF <- "PB"
PB_sime$ano <- substr(PB_sime$AP_CMP  ,1,4)
PB_sime$tratamentoCID <- "Simeprevir"

PB_ovrd$UF <- "PB"
PB_ovrd$ano <- substr(PB_ovrd$AP_CMP  ,1,4)
PB_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## PE

PE_alfainterf$UF <- "PE"
PE_alfainterf$ano <- substr(PE_alfainterf$AP_CMP  ,1,4)
PE_alfainterf$tratamentoCID <- "Alfainterferona"

PE_alfapeg$UF <- "PE"
PE_alfapeg$ano <- substr(PE_alfapeg$AP_CMP  ,1,4)
PE_alfapeg$tratamentoCID <- "Alfapeginterferona"

PE_riba$UF <- "PE"
PE_riba$ano <- substr(PE_riba$AP_CMP  ,1,4)
PE_riba$tratamentoCID <- "Ribavirina"

PE_boce$UF <- "PE"
PE_boce$ano <- substr(PE_boce$AP_CMP  ,1,4)
PE_boce$tratamentoCID <- "Boceprevir"

PE_telo$UF <- "PE"
PE_telo$ano <- substr(PE_telo$AP_CMP  ,1,4)
PE_telo$tratamentoCID <- "Telaprevir"

PE_sofo$UF <- "PE"
PE_sofo$ano <- substr(PE_sofo$AP_CMP  ,1,4)
PE_sofo$tratamentoCID <- "Sofosbuvir"

PE_dacla$UF <- "PE"
PE_dacla$ano <- substr(PE_dacla$AP_CMP  ,1,4)
PE_dacla$tratamentoCID <- "Daclatasvir"

PE_sime$UF <- "PE"
PE_sime$ano <- substr(PE_sime$AP_CMP  ,1,4)
PE_sime$tratamentoCID <- "Simeprevir"

PE_ovrd$UF <- "PE"
PE_ovrd$ano <- substr(PE_ovrd$AP_CMP  ,1,4)
PE_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## PI

PI_alfainterf$UF <- "PI"
PI_alfainterf$ano <- substr(PI_alfainterf$AP_CMP  ,1,4)
PI_alfainterf$tratamentoCID <- "Alfainterferona"

PI_alfapeg$UF <- "PI"
PI_alfapeg$ano <- substr(PI_alfapeg$AP_CMP  ,1,4)
PI_alfapeg$tratamentoCID <- "Alfapeginterferona"

PI_riba$UF <- "PI"
PI_riba$ano <- substr(PI_riba$AP_CMP  ,1,4)
PI_riba$tratamentoCID <- "Ribavirina"

PI_boce$UF <- "PI"
PI_boce$ano <- substr(PI_boce$AP_CMP  ,1,4)
PI_boce$tratamentoCID <- "Boceprevir"

PI_telo$UF <- "PI"
PI_telo$ano <- substr(PI_telo$AP_CMP  ,1,4)
PI_telo$tratamentoCID <- "Telaprevir"

PI_sofo$UF <- "PI"
PI_sofo$ano <- substr(PI_sofo$AP_CMP  ,1,4)
PI_sofo$tratamentoCID <- "Sofosbuvir"

PI_dacla$UF <- "PI"
PI_dacla$ano <- substr(PI_dacla$AP_CMP  ,1,4)
PI_dacla$tratamentoCID <- "Daclatasvir"

PI_sime$UF <- "PI"
PI_sime$ano <- substr(PI_sime$AP_CMP  ,1,4)
PI_sime$tratamentoCID <- "Simeprevir"

PI_ovrd$UF <- "PI"
PI_ovrd$ano <- substr(PI_ovrd$AP_CMP  ,1,4)
PI_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## PR

PR_alfainterf$UF <- "PR"
PR_alfainterf$ano <- substr(PR_alfainterf$AP_CMP  ,1,4)
PR_alfainterf$tratamentoCID <- "Alfainterferona"

PR_alfapeg$UF <- "PR"
PR_alfapeg$ano <- substr(PR_alfapeg$AP_CMP  ,1,4)
PR_alfapeg$tratamentoCID <- "Alfapeginterferona"

PR_riba$UF <- "PR"
PR_riba$ano <- substr(PR_riba$AP_CMP  ,1,4)
PR_riba$tratamentoCID <- "Ribavirina"

PR_boce$UF <- "PR"
PR_boce$ano <- substr(PR_boce$AP_CMP  ,1,4)
PR_boce$tratamentoCID <- "Boceprevir"

PR_telo$UF <- "PR"
PR_telo$ano <- substr(PR_telo$AP_CMP  ,1,4)
PR_telo$tratamentoCID <- "Telaprevir"

PR_sofo$UF <- "PR"
PR_sofo$ano <- substr(PR_sofo$AP_CMP  ,1,4)
PR_sofo$tratamentoCID <- "Sofosbuvir"

PR_dacla$UF <- "PR"
PR_dacla$ano <- substr(PR_dacla$AP_CMP  ,1,4)
PR_dacla$tratamentoCID <- "Daclatasvir"

PR_sime$UF <- "PR"
PR_sime$ano <- substr(PR_sime$AP_CMP  ,1,4)
PR_sime$tratamentoCID <- "Simeprevir"

PR_ovrd$UF <- "PR"
PR_ovrd$ano <- substr(PR_ovrd$AP_CMP  ,1,4)
PR_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## RJ

RJ_alfainterf$UF <- "RJ"
RJ_alfainterf$ano <- substr(RJ_alfainterf$AP_CMP  ,1,4)
RJ_alfainterf$tratamentoCID <- "Alfainterferona"

RJ_alfapeg$UF <- "RJ"
RJ_alfapeg$ano <- substr(RJ_alfapeg$AP_CMP  ,1,4)
RJ_alfapeg$tratamentoCID <- "Alfapeginterferona"

RJ_riba$UF <- "RJ"
RJ_riba$ano <- substr(RJ_riba$AP_CMP  ,1,4)
RJ_riba$tratamentoCID <- "Ribavirina"

RJ_boce$UF <- "RJ"
RJ_boce$ano <- substr(RJ_boce$AP_CMP  ,1,4)
RJ_boce$tratamentoCID <- "Boceprevir"

RJ_telo$UF <- "RJ"
RJ_telo$ano <- substr(RJ_telo$AP_CMP  ,1,4)
RJ_telo$tratamentoCID <- "Telaprevir"

RJ_sofo$UF <- "RJ"
RJ_sofo$ano <- substr(RJ_sofo$AP_CMP  ,1,4)
RJ_sofo$tratamentoCID <- "Sofosbuvir"

RJ_dacla$UF <- "RJ"
RJ_dacla$ano <- substr(RJ_dacla$AP_CMP  ,1,4)
RJ_dacla$tratamentoCID <- "Daclatasvir"

RJ_sime$UF <- "RJ"
RJ_sime$ano <- substr(RJ_sime$AP_CMP  ,1,4)
RJ_sime$tratamentoCID <- "Simeprevir"

RJ_ovrd$UF <- "RJ"
RJ_ovrd$ano <- substr(RJ_ovrd$AP_CMP  ,1,4)
RJ_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## RN

RN_alfainterf$UF <- "RN"
RN_alfainterf$ano <- substr(RN_alfainterf$AP_CMP  ,1,4)
RN_alfainterf$tratamentoCID <- "Alfainterferona"

RN_alfapeg$UF <- "RN"
RN_alfapeg$ano <- substr(RN_alfapeg$AP_CMP  ,1,4)
RN_alfapeg$tratamentoCID <- "Alfapeginterferona"

RN_riba$UF <- "RN"
RN_riba$ano <- substr(RN_riba$AP_CMP  ,1,4)
RN_riba$tratamentoCID <- "Ribavirina"

RN_boce$UF <- "RN"
RN_boce$ano <- substr(RN_boce$AP_CMP  ,1,4)
RN_boce$tratamentoCID <- "Boceprevir"

RN_telo$UF <- "RN"
RN_telo$ano <- substr(RN_telo$AP_CMP  ,1,4)
RN_telo$tratamentoCID <- "Telaprevir"

RN_sofo$UF <- "RN"
RN_sofo$ano <- substr(RN_sofo$AP_CMP  ,1,4)
RN_sofo$tratamentoCID <- "Sofosbuvir"

RN_dacla$UF <- "RN"
RN_dacla$ano <- substr(RN_dacla$AP_CMP  ,1,4)
RN_dacla$tratamentoCID <- "Daclatasvir"

RN_sime$UF <- "RN"
RN_sime$ano <- substr(RN_sime$AP_CMP  ,1,4)
RN_sime$tratamentoCID <- "Simeprevir"

RN_ovrd$UF <- "RN"
RN_ovrd$ano <- substr(RN_ovrd$AP_CMP  ,1,4)
RN_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## RO

RO_alfainterf$UF <- "RO"
RO_alfainterf$ano <- substr(RO_alfainterf$AP_CMP  ,1,4)
RO_alfainterf$tratamentoCID <- "Alfainterferona"

RO_alfapeg$UF <- "RO"
RO_alfapeg$ano <- substr(RO_alfapeg$AP_CMP  ,1,4)
RO_alfapeg$tratamentoCID <- "Alfapeginterferona"

RO_riba$UF <- "RO"
RO_riba$ano <- substr(RO_riba$AP_CMP  ,1,4)
RO_riba$tratamentoCID <- "Ribavirina"

RO_boce$UF <- "RO"
RO_boce$ano <- substr(RO_boce$AP_CMP  ,1,4)
RO_boce$tratamentoCID <- "Boceprevir"

RO_telo$UF <- "RO"
RO_telo$ano <- substr(RO_telo$AP_CMP  ,1,4)
RO_telo$tratamentoCID <- "Telaprevir"

RO_sofo$UF <- "RO"
RO_sofo$ano <- substr(RO_sofo$AP_CMP  ,1,4)
RO_sofo$tratamentoCID <- "Sofosbuvir"

RO_dacla$UF <- "RO"
RO_dacla$ano <- substr(RO_dacla$AP_CMP  ,1,4)
RO_dacla$tratamentoCID <- "Daclatasvir"

RO_sime$UF <- "RO"
RO_sime$ano <- substr(RO_sime$AP_CMP  ,1,4)
RO_sime$tratamentoCID <- "Simeprevir"

RO_ovrd$UF <- "RO"
RO_ovrd$ano <- substr(RO_ovrd$AP_CMP  ,1,4)
RO_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## RR

RR_alfainterf$UF <- "RR"
RR_alfainterf$ano <- substr(RR_alfainterf$AP_CMP  ,1,4)
RR_alfainterf$tratamentoCID <- "Alfainterferona"

RR_alfapeg$UF <- "RR"
RR_alfapeg$ano <- substr(RR_alfapeg$AP_CMP  ,1,4)
RR_alfapeg$tratamentoCID <- "Alfapeginterferona"

RR_riba$UF <- "RR"
RR_riba$ano <- substr(RR_riba$AP_CMP  ,1,4)
RR_riba$tratamentoCID <- "Ribavirina"

RR_boce$UF <- "RR"
RR_boce$ano <- substr(RR_boce$AP_CMP  ,1,4)
RR_boce$tratamentoCID <- "Boceprevir"

RR_telo$UF <- "RR"
RR_telo$ano <- substr(RR_telo$AP_CMP  ,1,4)
RR_telo$tratamentoCID <- "Telaprevir"

RR_sofo$UF <- "RR"
RR_sofo$ano <- substr(RR_sofo$AP_CMP  ,1,4)
RR_sofo$tratamentoCID <- "Sofosbuvir"

RR_dacla$UF <- "RR"
RR_dacla$ano <- substr(RR_dacla$AP_CMP  ,1,4)
RR_dacla$tratamentoCID <- "Daclatasvir"

RR_sime$UF <- "RR"
RR_sime$ano <- substr(RR_sime$AP_CMP  ,1,4)
RR_sime$tratamentoCID <- "Simeprevir"

RR_ovrd$UF <- "RR"
RR_ovrd$ano <- substr(RR_ovrd$AP_CMP  ,1,4)
RR_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## RS

RS_alfainterf$UF <- "RS"
RS_alfainterf$ano <- substr(RS_alfainterf$AP_CMP  ,1,4)
RS_alfainterf$tratamentoCID <- "Alfainterferona"

RS_alfapeg$UF <- "RS"
RS_alfapeg$ano <- substr(RS_alfapeg$AP_CMP  ,1,4)
RS_alfapeg$tratamentoCID <- "Alfapeginterferona"

RS_riba$UF <- "RS"
RS_riba$ano <- substr(RS_riba$AP_CMP  ,1,4)
RS_riba$tratamentoCID <- "Ribavirina"

RS_boce$UF <- "RS"
RS_boce$ano <- substr(RS_boce$AP_CMP  ,1,4)
RS_boce$tratamentoCID <- "Boceprevir"

RS_telo$UF <- "RS"
RS_telo$ano <- substr(RS_telo$AP_CMP  ,1,4)
RS_telo$tratamentoCID <- "Telaprevir"

RS_sofo$UF <- "RS"
RS_sofo$ano <- substr(RS_sofo$AP_CMP  ,1,4)
RS_sofo$tratamentoCID <- "Sofosbuvir"

RS_dacla$UF <- "RS"
RS_dacla$ano <- substr(RS_dacla$AP_CMP  ,1,4)
RS_dacla$tratamentoCID <- "Daclatasvir"

RS_sime$UF <- "RS"
RS_sime$ano <- substr(RS_sime$AP_CMP  ,1,4)
RS_sime$tratamentoCID <- "Simeprevir"

RS_ovrd$UF <- "RS"
RS_ovrd$ano <- substr(RS_ovrd$AP_CMP  ,1,4)
RS_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## SC

SC_alfainterf$UF <- "SC"
SC_alfainterf$ano <- substr(SC_alfainterf$AP_CMP  ,1,4)
SC_alfainterf$tratamentoCID <- "Alfainterferona"

SC_alfapeg$UF <- "SC"
SC_alfapeg$ano <- substr(SC_alfapeg$AP_CMP  ,1,4)
SC_alfapeg$tratamentoCID <- "Alfapeginterferona"

SC_riba$UF <- "SC"
SC_riba$ano <- substr(SC_riba$AP_CMP  ,1,4)
SC_riba$tratamentoCID <- "Ribavirina"

SC_boce$UF <- "SC"
SC_boce$ano <- substr(SC_boce$AP_CMP  ,1,4)
SC_boce$tratamentoCID <- "Boceprevir"

SC_telo$UF <- "SC"
SC_telo$ano <- substr(SC_telo$AP_CMP  ,1,4)
SC_telo$tratamentoCID <- "Telaprevir"

SC_sofo$UF <- "SC"
SC_sofo$ano <- substr(SC_sofo$AP_CMP  ,1,4)
SC_sofo$tratamentoCID <- "Sofosbuvir"

SC_dacla$UF <- "SC"
SC_dacla$ano <- substr(SC_dacla$AP_CMP  ,1,4)
SC_dacla$tratamentoCID <- "Daclatasvir"

SC_sime$UF <- "SC"
SC_sime$ano <- substr(SC_sime$AP_CMP  ,1,4)
SC_sime$tratamentoCID <- "Simeprevir"

SC_ovrd$UF <- "SC"
SC_ovrd$ano <- substr(SC_ovrd$AP_CMP  ,1,4)
SC_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## SE

SE_alfainterf$UF <- "SE"
SE_alfainterf$ano <- substr(SE_alfainterf$AP_CMP  ,1,4)
SE_alfainterf$tratamentoCID <- "Alfainterferona"

SE_alfapeg$UF <- "SE"
SE_alfapeg$ano <- substr(SE_alfapeg$AP_CMP  ,1,4)
SE_alfapeg$tratamentoCID <- "Alfapeginterferona"

SE_riba$UF <- "SE"
SE_riba$ano <- substr(SE_riba$AP_CMP  ,1,4)
SE_riba$tratamentoCID <- "Ribavirina"

SE_boce$UF <- "SE"
SE_boce$ano <- substr(SE_boce$AP_CMP  ,1,4)
SE_boce$tratamentoCID <- "Boceprevir"

SE_telo$UF <- "SE"
SE_telo$ano <- substr(SE_telo$AP_CMP  ,1,4)
SE_telo$tratamentoCID <- "Telaprevir"

SE_sofo$UF <- "SE"
SE_sofo$ano <- substr(SE_sofo$AP_CMP  ,1,4)
SE_sofo$tratamentoCID <- "Sofosbuvir"

SE_dacla$UF <- "SE"
SE_dacla$ano <- substr(SE_dacla$AP_CMP  ,1,4)
SE_dacla$tratamentoCID <- "Daclatasvir"

SE_sime$UF <- "SE"
SE_sime$ano <- substr(SE_sime$AP_CMP  ,1,4)
SE_sime$tratamentoCID <- "Simeprevir"

SE_ovrd$UF <- "SE"
SE_ovrd$ano <- substr(SE_ovrd$AP_CMP  ,1,4)
SE_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## SP

SP_alfainterf$UF <- "SP"
SP_alfainterf$ano <- substr(SP_alfainterf$AP_CMP  ,1,4)
SP_alfainterf$tratamentoCID <- "Alfainterferona"

SP_alfapeg$UF <- "SP"
SP_alfapeg$ano <- substr(SP_alfapeg$AP_CMP  ,1,4)
SP_alfapeg$tratamentoCID <- "Alfapeginterferona"

SP_riba$UF <- "SP"
SP_riba$ano <- substr(SP_riba$AP_CMP  ,1,4)
SP_riba$tratamentoCID <- "Ribavirina"

SP_boce$UF <- "SP"
SP_boce$ano <- substr(SP_boce$AP_CMP  ,1,4)
SP_boce$tratamentoCID <- "Boceprevir"

SP_telo$UF <- "SP"
SP_telo$ano <- substr(SP_telo$AP_CMP  ,1,4)
SP_telo$tratamentoCID <- "Telaprevir"

SP_sofo$UF <- "SP"
SP_sofo$ano <- substr(SP_sofo$AP_CMP  ,1,4)
SP_sofo$tratamentoCID <- "Sofosbuvir"

SP_dacla$UF <- "SP"
SP_dacla$ano <- substr(SP_dacla$AP_CMP  ,1,4)
SP_dacla$tratamentoCID <- "Daclatasvir"

SP_sime$UF <- "SP"
SP_sime$ano <- substr(SP_sime$AP_CMP  ,1,4)
SP_sime$tratamentoCID <- "Simeprevir"

SP_ovrd$UF <- "SP"
SP_ovrd$ano <- substr(SP_ovrd$AP_CMP  ,1,4)
SP_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

## TO

TO_alfainterf$UF <- "TO"
TO_alfainterf$ano <- substr(TO_alfainterf$AP_CMP  ,1,4)
TO_alfainterf$tratamentoCID <- "Alfainterferona"

TO_alfapeg$UF <- "TO"
TO_alfapeg$ano <- substr(TO_alfapeg$AP_CMP  ,1,4)
TO_alfapeg$tratamentoCID <- "Alfapeginterferona"

TO_riba$UF <- "TO"
TO_riba$ano <- substr(TO_riba$AP_CMP  ,1,4)
TO_riba$tratamentoCID <- "Ribavirina"

TO_boce$UF <- "TO"
TO_boce$ano <- substr(TO_boce$AP_CMP  ,1,4)
TO_boce$tratamentoCID <- "Boceprevir"

TO_telo$UF <- "TO"
TO_telo$ano <- substr(TO_telo$AP_CMP  ,1,4)
TO_telo$tratamentoCID <- "Telaprevir"

TO_sofo$UF <- "TO"
TO_sofo$ano <- substr(TO_sofo$AP_CMP  ,1,4)
TO_sofo$tratamentoCID <- "Sofosbuvir"

TO_dacla$UF <- "TO"
TO_dacla$ano <- substr(TO_dacla$AP_CMP  ,1,4)
TO_dacla$tratamentoCID <- "Daclatasvir"

TO_sime$UF <- "TO"
TO_sime$ano <- substr(TO_sime$AP_CMP  ,1,4)
TO_sime$tratamentoCID <- "Simeprevir"

TO_ovrd$UF <- "TO"
TO_ovrd$ano <- substr(TO_ovrd$AP_CMP  ,1,4)
TO_ovrd$tratamentoCID <- "ombitasvir/veruprevir/ritonavir/dasabuvir"

#######
## Tabelas plot
#######

## Alfainterf 

AC_alfainterf <- table(AC_alfainterf$ano)
AL_alfainterf <- table(AL_alfainterf$ano)
AP_alfainterf <- table(AP_alfainterf$ano)
AM_alfainterf <- table(AM_alfainterf$ano)
BA_alfainterf <- table(BA_alfainterf$ano)
CE_alfainterf <- table(CE_alfainterf$ano)
DF_alfainterf <- table(DF_alfainterf$ano)
ES_alfainterf <- table(ES_alfainterf$ano)
GO_alfainterf <- table(GO_alfainterf$ano)
MA_alfainterf <- table(MA_alfainterf$ano)
MG_alfainterf <- table(MG_alfainterf$ano)
MS_alfainterf <- table(MS_alfainterf$ano)
MT_alfainterf <- table(MT_alfainterf$ano)
PA_alfainterf <- table(PA_alfainterf$ano)
PB_alfainterf <- table(PB_alfainterf$ano)
PE_alfainterf <- table(PE_alfainterf$ano)
PI_alfainterf <- table(PI_alfainterf$ano)
PR_alfainterf <- table(PR_alfainterf$ano)
RJ_alfainterf <- table(RJ_alfainterf$ano)
RN_alfainterf <- table(RN_alfainterf$ano)
RO_alfainterf <- table(RO_alfainterf$ano)
RR_alfainterf <- table(RR_alfainterf$ano)
RS_alfainterf <- table(RS_alfainterf$ano)
SC_alfainterf <- table(SC_alfainterf$ano)
SE_alfainterf <- table(SE_alfainterf$ano)
SP_alfainterf <- table(SP_alfainterf$ano)
TO_alfainterf <- table(TO_alfainterf$ano)

AC_alfainterf <- as.data.frame(AC_alfainterf)
AL_alfainterf <- as.data.frame(AL_alfainterf)
AM_alfainterf <- as.data.frame(AM_alfainterf)
AP_alfainterf <- as.data.frame(AP_alfainterf)
BA_alfainterf <- as.data.frame(BA_alfainterf)
CE_alfainterf <- as.data.frame(CE_alfainterf)
DF_alfainterf <- as.data.frame(DF_alfainterf)
ES_alfainterf <- as.data.frame(ES_alfainterf)
GO_alfainterf <- as.data.frame(GO_alfainterf)
MA_alfainterf <- as.data.frame(MA_alfainterf)
MG_alfainterf <- as.data.frame(MG_alfainterf)
MS_alfainterf <- as.data.frame(MS_alfainterf)
MT_alfainterf <- as.data.frame(MT_alfainterf)
PA_alfainterf <- as.data.frame(PA_alfainterf)
PB_alfainterf <- as.data.frame(PB_alfainterf)
PE_alfainterf <- as.data.frame(PE_alfainterf)
PI_alfainterf <- as.data.frame(PI_alfainterf)
PR_alfainterf <- as.data.frame(PR_alfainterf)
RJ_alfainterf <- as.data.frame(RJ_alfainterf)
RN_alfainterf <- as.data.frame(RN_alfainterf)
RO_alfainterf <- as.data.frame(RO_alfainterf)
RR_alfainterf <- as.data.frame(RR_alfainterf)
RS_alfainterf <- as.data.frame(RS_alfainterf)
SC_alfainterf <- as.data.frame(SC_alfainterf)
SE_alfainterf <- as.data.frame(SE_alfainterf)
SP_alfainterf <- as.data.frame(SP_alfainterf)
TO_alfainterf <- as.data.frame(TO_alfainterf)

## Alfpeg

AC_alfapeg <- table(AC_alfapeg$ano)
AL_alfapeg <- table(AL_alfapeg$ano)
AP_alfapeg <- table(AP_alfapeg$ano)
AM_alfapeg <- table(AM_alfapeg$ano)
BA_alfapeg <- table(BA_alfapeg$ano)
CE_alfapeg <- table(CE_alfapeg$ano)
DF_alfapeg <- table(DF_alfapeg$ano)
ES_alfapeg <- table(ES_alfapeg$ano)
GO_alfapeg <- table(GO_alfapeg$ano)
MA_alfapeg <- table(MA_alfapeg$ano)
MG_alfapeg <- table(MG_alfapeg$ano)
MS_alfapeg <- table(MS_alfapeg$ano)
MT_alfapeg <- table(MT_alfapeg$ano)
PA_alfapeg <- table(PA_alfapeg$ano)
PB_alfapeg <- table(PB_alfapeg$ano)
PE_alfapeg <- table(PE_alfapeg$ano)
PI_alfapeg <- table(PI_alfapeg$ano)
PR_alfapeg <- table(PR_alfapeg$ano)
RJ_alfapeg <- table(RJ_alfapeg$ano)
RN_alfapeg <- table(RN_alfapeg$ano)
RO_alfapeg <- table(RO_alfapeg$ano)
RR_alfapeg <- table(RR_alfapeg$ano)
RS_alfapeg <- table(RS_alfapeg$ano)
SC_alfapeg <- table(SC_alfapeg$ano)
SE_alfapeg <- table(SE_alfapeg$ano)
SP_alfapeg <- table(SP_alfapeg$ano)
TO_alfapeg <- table(TO_alfapeg$ano)

AC_alfapeg <- as.data.frame(AC_alfapeg)
AL_alfapeg <- as.data.frame(AL_alfapeg)
AM_alfapeg <- as.data.frame(AM_alfapeg)
AP_alfapeg <- as.data.frame(AP_alfapeg)
BA_alfapeg <- as.data.frame(BA_alfapeg)
CE_alfapeg <- as.data.frame(CE_alfapeg)
DF_alfapeg <- as.data.frame(DF_alfapeg)
ES_alfapeg <- as.data.frame(ES_alfapeg)
GO_alfapeg <- as.data.frame(GO_alfapeg)
MA_alfapeg <- as.data.frame(MA_alfapeg)
MG_alfapeg <- as.data.frame(MG_alfapeg)
MS_alfapeg <- as.data.frame(MS_alfapeg)
MT_alfapeg <- as.data.frame(MT_alfapeg)
PA_alfapeg <- as.data.frame(PA_alfapeg)
PB_alfapeg <- as.data.frame(PB_alfapeg)
PE_alfapeg <- as.data.frame(PE_alfapeg)
PI_alfapeg <- as.data.frame(PI_alfapeg)
PR_alfapeg <- as.data.frame(PR_alfapeg)
RJ_alfapeg <- as.data.frame(RJ_alfapeg)
RN_alfapeg <- as.data.frame(RN_alfapeg)
RO_alfapeg <- as.data.frame(RO_alfapeg)
RR_alfapeg <- as.data.frame(RR_alfapeg)
RS_alfapeg <- as.data.frame(RS_alfapeg)
SC_alfapeg <- as.data.frame(SC_alfapeg)
SE_alfapeg <- as.data.frame(SE_alfapeg)
SP_alfapeg <- as.data.frame(SP_alfapeg)
TO_alfapeg <- as.data.frame(TO_alfapeg)

## Riba

AC_riba <- table(AC_riba$ano)
AL_riba <- table(AL_riba$ano)
AP_riba <- table(AP_riba$ano)
AM_riba <- table(AM_riba$ano)
BA_riba <- table(BA_riba$ano)
CE_riba <- table(CE_riba$ano)
DF_riba <- table(DF_riba$ano)
ES_riba <- table(ES_riba$ano)
GO_riba <- table(GO_riba$ano)
MA_riba <- table(MA_riba$ano)
MG_riba <- table(MG_riba$ano)
MS_riba <- table(MS_riba$ano)
MT_riba <- table(MT_riba$ano)
PA_riba <- table(PA_riba$ano)
PB_riba <- table(PB_riba$ano)
PE_riba <- table(PE_riba$ano)
PI_riba <- table(PI_riba$ano)
PR_riba <- table(PR_riba$ano)
RJ_riba <- table(RJ_riba$ano)
RN_riba <- table(RN_riba$ano)
RO_riba <- table(RO_riba$ano)
RR_riba <- table(RR_riba$ano)
RS_riba <- table(RS_riba$ano)
SC_riba <- table(SC_riba$ano)
SE_riba <- table(SE_riba$ano)
SP_riba <- table(SP_riba$ano)
TO_riba <- table(TO_riba$ano)

AC_riba <- as.data.frame(AC_riba)
AL_riba <- as.data.frame(AL_riba)
AM_riba <- as.data.frame(AM_riba)
AP_riba <- as.data.frame(AP_riba)
BA_riba <- as.data.frame(BA_riba)
CE_riba <- as.data.frame(CE_riba)
DF_riba <- as.data.frame(DF_riba)
ES_riba <- as.data.frame(ES_riba)
GO_riba <- as.data.frame(GO_riba)
MA_riba <- as.data.frame(MA_riba)
MG_riba <- as.data.frame(MG_riba)
MS_riba <- as.data.frame(MS_riba)
MT_riba <- as.data.frame(MT_riba)
PA_riba <- as.data.frame(PA_riba)
PB_riba <- as.data.frame(PB_riba)
PE_riba <- as.data.frame(PE_riba)
PI_riba <- as.data.frame(PI_riba)
PR_riba <- as.data.frame(PR_riba)
RJ_riba <- as.data.frame(RJ_riba)
RN_riba <- as.data.frame(RN_riba)
RO_riba <- as.data.frame(RO_riba)
RR_riba <- as.data.frame(RR_riba)
RS_riba <- as.data.frame(RS_riba)
SC_riba <- as.data.frame(SC_riba)
SE_riba <- as.data.frame(SE_riba)
SP_riba <- as.data.frame(SP_riba)
TO_riba <- as.data.frame(TO_riba)

## Boceprevir

AC_boce <- table(AC_boce$ano)
AL_boce <- table(AL_boce$ano)
AP_boce <- table(AP_boce$ano)
AM_boce <- table(AM_boce$ano)
BA_boce <- table(BA_boce$ano)
CE_boce <- table(CE_boce$ano)
DF_boce <- table(DF_boce$ano)
ES_boce <- table(ES_boce$ano)
GO_boce <- table(GO_boce$ano)
MA_boce <- table(MA_boce$ano)
MG_boce <- table(MG_boce$ano)
MS_boce <- table(MS_boce$ano)
MT_boce <- table(MT_boce$ano)
PA_boce <- table(PA_boce$ano)
PB_boce <- table(PB_boce$ano)
PE_boce <- table(PE_boce$ano)
PI_boce <- table(PI_boce$ano)
PR_boce <- table(PR_boce$ano)
RJ_boce <- table(RJ_boce$ano)
RN_boce <- table(RN_boce$ano)
RO_boce <- table(RO_boce$ano)
RR_boce <- table(RR_boce$ano)
RS_boce <- table(RS_boce$ano)
SC_boce <- table(SC_boce$ano)
SE_boce <- table(SE_boce$ano)
SP_boce <- table(SP_boce$ano)
TO_boce <- table(TO_boce$ano)

AC_boce <- as.data.frame(AC_boce)
AL_boce <- as.data.frame(AL_boce)
AM_boce <- as.data.frame(AM_boce)
AP_boce <- as.data.frame(AP_boce)
BA_boce <- as.data.frame(BA_boce)
CE_boce <- as.data.frame(CE_boce)
DF_boce <- as.data.frame(DF_boce)
ES_boce <- as.data.frame(ES_boce)
GO_boce <- as.data.frame(GO_boce)
MA_boce <- as.data.frame(MA_boce)
MG_boce <- as.data.frame(MG_boce)
MS_boce <- as.data.frame(MS_boce)
MT_boce <- as.data.frame(MT_boce)
PA_boce <- as.data.frame(PA_boce)
PB_boce <- as.data.frame(PB_boce)
PE_boce <- as.data.frame(PE_boce)
PI_boce <- as.data.frame(PI_boce)
PR_boce <- as.data.frame(PR_boce)
RJ_boce <- as.data.frame(RJ_boce)
RN_boce <- as.data.frame(RN_boce)
RO_boce <- as.data.frame(RO_boce)
RR_boce <- as.data.frame(RR_boce)
RS_boce <- as.data.frame(RS_boce)
SC_boce <- as.data.frame(SC_boce)
SE_boce <- as.data.frame(SE_boce)
SP_boce <- as.data.frame(SP_boce)
TO_boce <- as.data.frame(TO_boce)

## Telaprevir

AC_telo <- table(AC_telo$ano)
AL_telo <- table(AL_telo$ano)
AP_telo <- table(AP_telo$ano)
AM_telo <- table(AM_telo$ano)
BA_telo <- table(BA_telo$ano)
CE_telo <- table(CE_telo$ano)
DF_telo <- table(DF_telo$ano)
ES_telo <- table(ES_telo$ano)
GO_telo <- table(GO_telo$ano)
MA_telo <- table(MA_telo$ano)
MG_telo <- table(MG_telo$ano)
MS_telo <- table(MS_telo$ano)
MT_telo <- table(MT_telo$ano)
PA_telo <- table(PA_telo$ano)
PB_telo <- table(PB_telo$ano)
PE_telo <- table(PE_telo$ano)
PI_telo <- table(PI_telo$ano)
PR_telo <- table(PR_telo$ano)
RJ_telo <- table(RJ_telo$ano)
RN_telo <- table(RN_telo$ano)
RO_telo <- table(RO_telo$ano)
RR_telo <- table(RR_telo$ano)
RS_telo <- table(RS_telo$ano)
SC_telo <- table(SC_telo$ano)
SE_telo <- table(SE_telo$ano)
SP_telo <- table(SP_telo$ano)
TO_telo <- table(TO_telo$ano)

AC_telo <- as.data.frame(AC_telo)
AL_telo <- as.data.frame(AL_telo)
AM_telo <- as.data.frame(AM_telo)
AP_telo <- as.data.frame(AP_telo)
BA_telo <- as.data.frame(BA_telo)
CE_telo <- as.data.frame(CE_telo)
DF_telo <- as.data.frame(DF_telo)
ES_telo <- as.data.frame(ES_telo)
GO_telo <- as.data.frame(GO_telo)
MA_telo <- as.data.frame(MA_telo)
MG_telo <- as.data.frame(MG_telo)
MS_telo <- as.data.frame(MS_telo)
MT_telo <- as.data.frame(MT_telo)
PA_telo <- as.data.frame(PA_telo)
PB_telo <- as.data.frame(PB_telo)
PE_telo <- as.data.frame(PE_telo)
PI_telo <- as.data.frame(PI_telo)
PR_telo <- as.data.frame(PR_telo)
RJ_telo <- as.data.frame(RJ_telo)
RN_telo <- as.data.frame(RN_telo)
RO_telo <- as.data.frame(RO_telo)
RR_telo <- as.data.frame(RR_telo)
RS_telo <- as.data.frame(RS_telo)
SC_telo <- as.data.frame(SC_telo)
SE_telo <- as.data.frame(SE_telo)
SP_telo <- as.data.frame(SP_telo)
TO_telo <- as.data.frame(TO_telo)

## Sofosbuvir

AC_sofo <- table(AC_sofo$ano)
AL_sofo <- table(AL_sofo$ano)
AP_sofo <- table(AP_sofo$ano)
AM_sofo <- table(AM_sofo$ano)
BA_sofo <- table(BA_sofo$ano)
CE_sofo <- table(CE_sofo$ano)
DF_sofo <- table(DF_sofo$ano)
ES_sofo <- table(ES_sofo$ano)
GO_sofo <- table(GO_sofo$ano)
MA_sofo <- table(MA_sofo$ano)
MG_sofo <- table(MG_sofo$ano)
MS_sofo <- table(MS_sofo$ano)
MT_sofo <- table(MT_sofo$ano)
PA_sofo <- table(PA_sofo$ano)
PB_sofo <- table(PB_sofo$ano)
PE_sofo <- table(PE_sofo$ano)
PI_sofo <- table(PI_sofo$ano)
PR_sofo <- table(PR_sofo$ano)
RJ_sofo <- table(RJ_sofo$ano)
RN_sofo <- table(RN_sofo$ano)
RO_sofo <- table(RO_sofo$ano)
RR_sofo <- table(RR_sofo$ano)
RS_sofo <- table(RS_sofo$ano)
SC_sofo <- table(SC_sofo$ano)
SE_sofo <- table(SE_sofo$ano)
SP_sofo <- table(SP_sofo$ano)
TO_sofo <- table(TO_sofo$ano)

AC_sofo <- as.data.frame(AC_sofo)
AL_sofo <- as.data.frame(AL_sofo)
AM_sofo <- as.data.frame(AM_sofo)
AP_sofo <- as.data.frame(AP_sofo)
BA_sofo <- as.data.frame(BA_sofo)
CE_sofo <- as.data.frame(CE_sofo)
DF_sofo <- as.data.frame(DF_sofo)
ES_sofo <- as.data.frame(ES_sofo)
GO_sofo <- as.data.frame(GO_sofo)
MA_sofo <- as.data.frame(MA_sofo)
MG_sofo <- as.data.frame(MG_sofo)
MS_sofo <- as.data.frame(MS_sofo)
MT_sofo <- as.data.frame(MT_sofo)
PA_sofo <- as.data.frame(PA_sofo)
PB_sofo <- as.data.frame(PB_sofo)
PE_sofo <- as.data.frame(PE_sofo)
PI_sofo <- as.data.frame(PI_sofo)
PR_sofo <- as.data.frame(PR_sofo)
RJ_sofo <- as.data.frame(RJ_sofo)
RN_sofo <- as.data.frame(RN_sofo)
RO_sofo <- as.data.frame(RO_sofo)
RR_sofo <- as.data.frame(RR_sofo)
RS_sofo <- as.data.frame(RS_sofo)
SC_sofo <- as.data.frame(SC_sofo)
SE_sofo <- as.data.frame(SE_sofo)
SP_sofo <- as.data.frame(SP_sofo)
TO_sofo <- as.data.frame(TO_sofo)

## Daclatasvir

AC_dacla <- table(AC_dacla$ano)
AL_dacla <- table(AL_dacla$ano)
AP_dacla <- table(AP_dacla$ano)
AM_dacla <- table(AM_dacla$ano)
BA_dacla <- table(BA_dacla$ano)
CE_dacla <- table(CE_dacla$ano)
DF_dacla <- table(DF_dacla$ano)
ES_dacla <- table(ES_dacla$ano)
GO_dacla <- table(GO_dacla$ano)
MA_dacla <- table(MA_dacla$ano)
MG_dacla <- table(MG_dacla$ano)
MS_dacla <- table(MS_dacla$ano)
MT_dacla <- table(MT_dacla$ano)
PA_dacla <- table(PA_dacla$ano)
PB_dacla <- table(PB_dacla$ano)
PE_dacla <- table(PE_dacla$ano)
PI_dacla <- table(PI_dacla$ano)
PR_dacla <- table(PR_dacla$ano)
RJ_dacla <- table(RJ_dacla$ano)
RN_dacla <- table(RN_dacla$ano)
RO_dacla <- table(RO_dacla$ano)
RR_dacla <- table(RR_dacla$ano)
RS_dacla <- table(RS_dacla$ano)
SC_dacla <- table(SC_dacla$ano)
SE_dacla <- table(SE_dacla$ano)
SP_dacla <- table(SP_dacla$ano)
TO_dacla <- table(TO_dacla$ano)

AC_dacla <- as.data.frame(AC_dacla)
AL_dacla <- as.data.frame(AL_dacla)
AM_dacla <- as.data.frame(AM_dacla)
AP_dacla <- as.data.frame(AP_dacla)
BA_dacla <- as.data.frame(BA_dacla)
CE_dacla <- as.data.frame(CE_dacla)
DF_dacla <- as.data.frame(DF_dacla)
ES_dacla <- as.data.frame(ES_dacla)
GO_dacla <- as.data.frame(GO_dacla)
MA_dacla <- as.data.frame(MA_dacla)
MG_dacla <- as.data.frame(MG_dacla)
MS_dacla <- as.data.frame(MS_dacla)
MT_dacla <- as.data.frame(MT_dacla)
PA_dacla <- as.data.frame(PA_dacla)
PB_dacla <- as.data.frame(PB_dacla)
PE_dacla <- as.data.frame(PE_dacla)
PI_dacla <- as.data.frame(PI_dacla)
PR_dacla <- as.data.frame(PR_dacla)
RJ_dacla <- as.data.frame(RJ_dacla)
RN_dacla <- as.data.frame(RN_dacla)
RO_dacla <- as.data.frame(RO_dacla)
RR_dacla <- as.data.frame(RR_dacla)
RS_dacla <- as.data.frame(RS_dacla)
SC_dacla <- as.data.frame(SC_dacla)
SE_dacla <- as.data.frame(SE_dacla)
SP_dacla <- as.data.frame(SP_dacla)
TO_dacla <- as.data.frame(TO_dacla)

## Simeprevir

AC_sime <- table(AC_sime$ano)
AL_sime <- table(AL_sime$ano)
AP_sime <- table(AP_sime$ano)
AM_sime <- table(AM_sime$ano)
BA_sime <- table(BA_sime$ano)
CE_sime <- table(CE_sime$ano)
DF_sime <- table(DF_sime$ano)
ES_sime <- table(ES_sime$ano)
GO_sime <- table(GO_sime$ano)
MA_sime <- table(MA_sime$ano)
MG_sime <- table(MG_sime$ano)
MS_sime <- table(MS_sime$ano)
MT_sime <- table(MT_sime$ano)
PA_sime <- table(PA_sime$ano)
PB_sime <- table(PB_sime$ano)
PE_sime <- table(PE_sime$ano)
PI_sime <- table(PI_sime$ano)
PR_sime <- table(PR_sime$ano)
RJ_sime <- table(RJ_sime$ano)
RN_sime <- table(RN_sime$ano)
RO_sime <- table(RO_sime$ano)
RR_sime <- table(RR_sime$ano)
RS_sime <- table(RS_sime$ano)
SC_sime <- table(SC_sime$ano)
SE_sime <- table(SE_sime$ano)
SP_sime <- table(SP_sime$ano)
TO_sime <- table(TO_sime$ano)

AC_sime <- as.data.frame(AC_sime)
AL_sime <- as.data.frame(AL_sime)
AM_sime <- as.data.frame(AM_sime)
AP_sime <- as.data.frame(AP_sime)
BA_sime <- as.data.frame(BA_sime)
CE_sime <- as.data.frame(CE_sime)
DF_sime <- as.data.frame(DF_sime)
ES_sime <- as.data.frame(ES_sime)
GO_sime <- as.data.frame(GO_sime)
MA_sime <- as.data.frame(MA_sime)
MG_sime <- as.data.frame(MG_sime)
MS_sime <- as.data.frame(MS_sime)
MT_sime <- as.data.frame(MT_sime)
PA_sime <- as.data.frame(PA_sime)
PB_sime <- as.data.frame(PB_sime)
PE_sime <- as.data.frame(PE_sime)
PI_sime <- as.data.frame(PI_sime)
PR_sime <- as.data.frame(PR_sime)
RJ_sime <- as.data.frame(RJ_sime)
RN_sime <- as.data.frame(RN_sime)
RO_sime <- as.data.frame(RO_sime)
RR_sime <- as.data.frame(RR_sime)
RS_sime <- as.data.frame(RS_sime)
SC_sime <- as.data.frame(SC_sime)
SE_sime <- as.data.frame(SE_sime)
SP_sime <- as.data.frame(SP_sime)
TO_sime <- as.data.frame(TO_sime)

## OVRD

AC_ovrd <- table(AC_ovrd$ano)
AL_ovrd <- table(AL_ovrd$ano)
AP_ovrd <- table(AP_ovrd$ano)
AM_ovrd <- table(AM_ovrd$ano)
BA_ovrd <- table(BA_ovrd$ano)
CE_ovrd <- table(CE_ovrd$ano)
DF_ovrd <- table(DF_ovrd$ano)
ES_ovrd <- table(ES_ovrd$ano)
GO_ovrd <- table(GO_ovrd$ano)
MA_ovrd <- table(MA_ovrd$ano)
MG_ovrd <- table(MG_ovrd$ano)
MS_ovrd <- table(MS_ovrd$ano)
MT_ovrd <- table(MT_ovrd$ano)
PA_ovrd <- table(PA_ovrd$ano)
PB_ovrd <- table(PB_ovrd$ano)
PE_ovrd <- table(PE_ovrd$ano)
PI_ovrd <- table(PI_ovrd$ano)
PR_ovrd <- table(PR_ovrd$ano)
RJ_ovrd <- table(RJ_ovrd$ano)
RN_ovrd <- table(RN_ovrd$ano)
RO_ovrd <- table(RO_ovrd$ano)
RR_ovrd <- table(RR_ovrd$ano)
RS_ovrd <- table(RS_ovrd$ano)
SC_ovrd <- table(SC_ovrd$ano)
SE_ovrd <- table(SE_ovrd$ano)
SP_ovrd <- table(SP_ovrd$ano)
TO_ovrd <- table(TO_ovrd$ano)

AC_ovrd <- as.data.frame(AC_ovrd)
AL_ovrd <- as.data.frame(AL_ovrd)
AM_ovrd <- as.data.frame(AM_ovrd)
AP_ovrd <- as.data.frame(AP_ovrd)
BA_ovrd <- as.data.frame(BA_ovrd)
CE_ovrd <- as.data.frame(CE_ovrd)
DF_ovrd <- as.data.frame(DF_ovrd)
ES_ovrd <- as.data.frame(ES_ovrd)
GO_ovrd <- as.data.frame(GO_ovrd)
MA_ovrd <- as.data.frame(MA_ovrd)
MG_ovrd <- as.data.frame(MG_ovrd)
MS_ovrd <- as.data.frame(MS_ovrd)
MT_ovrd <- as.data.frame(MT_ovrd)
PA_ovrd <- as.data.frame(PA_ovrd)
PB_ovrd <- as.data.frame(PB_ovrd)
PE_ovrd <- as.data.frame(PE_ovrd)
PI_ovrd <- as.data.frame(PI_ovrd)
PR_ovrd <- as.data.frame(PR_ovrd)
RJ_ovrd <- as.data.frame(RJ_ovrd)
RN_ovrd <- as.data.frame(RN_ovrd)
RO_ovrd <- as.data.frame(RO_ovrd)
RR_ovrd <- as.data.frame(RR_ovrd)
RS_ovrd <- as.data.frame(RS_ovrd)
SC_ovrd <- as.data.frame(SC_ovrd)
SE_ovrd <- as.data.frame(SE_ovrd)
SP_ovrd <- as.data.frame(SP_ovrd)
TO_ovrd <- as.data.frame(TO_ovrd)


### Brasil ###

br_alfainterf <- do.call("rbind", list( AC_alfainterf, AL_alfainterf , AM_alfainterf,AP_alfainterf, BA_alfainterf , CE_alfainterf , DF_alfainterf , ES_alfainterf, GO_alfainterf, MA_alfainterf, MG_alfainterf, MS_alfainterf, MT_alfainterf, PA_alfainterf, PB_alfainterf, PE_alfainterf,  PR_alfainterf, PI_alfainterf, RJ_alfainterf, RO_alfainterf, RR_alfainterf, RS_alfainterf, SC_alfainterf, SP_alfainterf, SE_alfainterf, TO_alfainterf ))

write.csv(br_alfainterf, file = '/Volumes/Mikael_backup3/produto3/br_alfainterf.csv' )

br_alfapeg <- do.call("rbind", list( AC_alfapeg, AL_alfapeg , AM_alfapeg,AP_alfapeg, BA_alfapeg , CE_alfapeg , DF_alfapeg , ES_alfapeg, GO_alfapeg, MA_alfapeg, MG_alfapeg, MS_alfapeg, MT_alfapeg, PA_alfapeg, PB_alfapeg, PE_alfapeg,  PR_alfapeg, PI_alfapeg, RJ_alfapeg, RO_alfapeg, RR_alfapeg, RS_alfapeg, SC_alfapeg, SP_alfapeg, SE_alfapeg, TO_alfapeg ))

write.csv(br_alfapeg, file = '/Volumes/Mikael_backup3/produto3/br_alfapeg.csv' )

br_riba <- do.call("rbind", list( AC_riba, AL_riba , AM_riba,AP_riba, BA_riba , CE_riba , DF_riba , ES_riba, GO_riba, MA_riba, MG_riba, MS_riba, MT_riba, PA_riba, PB_riba, PE_riba,  PR_riba, PI_riba, RJ_riba, RO_riba, RR_riba, RS_riba, SC_riba, SP_riba, SE_riba, TO_riba ))

write.csv(br_riba, file = '/Volumes/Mikael_backup3/produto3/br_riba.csv' )

br_boce <- do.call("rbind", list( AC_boce, AL_boce , AM_boce,AP_boce, BA_boce , CE_boce , DF_boce , ES_boce, GO_boce, MA_boce, MG_boce, MS_boce, PA_boce, PE_boce,  PR_boce, RJ_boce, RO_boce, RS_boce, SC_boce, SP_boce, SE_boce ))                                      

write.csv(br_boce, file = '/Volumes/Mikael_backup3/produto3/br_boce.csv' )

br_telo <- do.call("rbind", list( AC_telo, AL_telo , AM_telo,AP_telo, BA_telo , CE_telo , DF_telo , ES_telo, GO_telo, MA_telo, MG_telo, MS_telo, MT_telo, PA_telo, PB_telo, PE_telo,  PR_telo, RJ_telo, RO_telo, RR_telo, RS_telo, SP_telo, SE_telo ))

write.csv(br_telo, file = '/Volumes/Mikael_backup3/produto3/br_telo.csv' )

br_sofo <- do.call("rbind", list( AC_sofo, AL_sofo , AM_sofo,AP_sofo, BA_sofo , CE_sofo , DF_sofo , ES_sofo, GO_sofo, MA_sofo, MG_sofo, MS_sofo, MT_sofo, PA_sofo, PB_sofo, PE_sofo,  PR_sofo, PI_sofo, RJ_sofo, RO_sofo, RR_sofo, RS_sofo, SC_sofo, SP_sofo, SE_sofo, TO_sofo ))

write.csv(br_sofo, file = '/Volumes/Mikael_backup3/produto3/br_sofo.csv' )

br_dacla <- do.call("rbind", list( AC_dacla, AL_dacla , AM_dacla,AP_dacla, BA_dacla , CE_dacla , DF_dacla , ES_dacla, GO_dacla, MA_dacla, MG_dacla, MS_dacla, MT_dacla, PA_dacla, PB_dacla, PE_dacla,  PR_dacla, PI_dacla, RJ_dacla, RO_dacla, RR_dacla, RS_dacla, SC_dacla, SP_dacla, SE_dacla, TO_dacla ))

write.csv(br_dacla, file = '/Volumes/Mikael_backup3/produto3/br_dacla.csv' )

br_sime <- do.call("rbind", list( AC_sime, AL_sime , AM_sime,AP_sime, BA_sime , CE_sime , DF_sime , ES_sime, GO_sime, MA_sime, MG_sime, MS_sime, MT_sime, PA_sime, PB_sime, PE_sime,  PR_sime, PI_sime, RJ_sime, RO_sime, RR_sime, RS_sime, SC_sime, SP_sime, SE_sime, TO_sime ))

write.csv(br_sime, file = '/Volumes/Mikael_backup3/produto3/br_sime.csv' )

br_ovrd <- do.call("rbind", list( AC_ovrd, AL_ovrd , AM_ovrd,AP_ovrd, BA_ovrd , CE_ovrd , DF_ovrd , ES_ovrd, GO_ovrd, MA_ovrd, MG_ovrd, MS_ovrd, MT_ovrd, PA_ovrd, PB_ovrd, PE_ovrd,  PR_ovrd, PI_ovrd, RJ_ovrd, RO_ovrd, RR_ovrd, RS_ovrd, SC_ovrd, SP_ovrd, SE_ovrd, TO_ovrd ))

write.csv(br_ovrd, file = '/Volumes/Mikael_backup3/produto3/br_ovrd.csv' )

br_tratamento <- do.call("rbind", list( br_alfainterf, br_alfapeg , br_boce,br_dacla, br_ovrd , br_riba , br_sime , br_sofo, br_telo ))

write.csv(br_tratamento, file = '/Volumes/Mikael_backup3/produto3/br_tratamento.csv' )

### agravos por ano e agravos por uf

write.csv(agravos_plot, file = '/Volumes/Mikael_backup3/produto3/agravos_plot.csv' )

write.csv(agravos_plot_uf, file = '/Volumes/Mikael_backup3/produto3/agravos_plot_uf.csv' )

### obitos por ano e obitos por uf

write.csv(obt_plot, file = '/Volumes/Mikael_backup3/produto3/obt_plot.csv' )

write.csv(obt_plot_uf, file = '/Volumes/Mikael_backup3/produto3/obt_plot_uf.csv' )

###############################
###### PreparaÃ§ao dos plots
##############################

br_tratamentos_plot <- read.csv('/Volumes/Mikael_backup3/produto3/br_tratamento.csv' )

br_tratamentos_plot <- read.csv('/produto3/br_tratamento.csv' )

obt_plot <-  read.csv('/produto3/obt_plot.csv' )

br_tratamentos_plot <- select(br_tratamentos_plot, -X)

agravos_plot$UF <- ""

agravos_plot <- select(agravos_plot, Var1, Freq, UF, tratamentoCID = CID )

obt_plot$UF <- ""

obt_plot <- select(obt_plot, -X)

obt_plot <- select(obt_plot, Var1, Freq, UF, Tratamento_CID = CID )

br_tratamentos_plot <- select(br_tratamentos_plot, Var1, Freq, UF, Tratamento_CID = tratamentoCID )


br_tratamento_agravos_plot <- do.call("rbind", list( br_tratamentos_plot, agravos_plot ))

br_tratamento_obt_plot <- do.call("rbind", list( br_tratamentos_plot, obt_plot ))

#########################
####### scatter plot - tratamentos x agravos e tratamentos x obt
#########################

################# scatter plot linhas ########################
 ggplot(data = df_agravo_trat , aes(x =Var1, 
                                y = Freqt, 
                                group=Procedimentos_hepC_CID_agravos, 
                                color=Procedimentos_hepC_CID_agravos )) +
  geom_line() +
  geom_point() + 
  labs( 
    y="FrequÃªncia (log2)", 
    x="Ano"
  ) + geom_text(aes(label=Freqt),hjust=0, vjust=0, check_overlap = TRUE, size = 3) + scale_x_continuous(breaks = seq(2008, 2019, by = 1)) + theme_minimal() + scale_color_brewer(palette = "Paired") + scale_y_continuous(trans="log2") 

#############################################################

################# scatter plot linhas ########################
ggplot(data = df_obt_trat , aes(x =Var1, 
                                               y = Freqt, 
                                               group=Tratamento_CID, 
                                               color=Tratamento_CID )) + geom_line() +
  geom_point() + 
  labs( 
    y="Frequência (log2)", 
    x="Ano"
  ) + geom_text(aes(label=Freqt),hjust=0, vjust=0, check_overlap = TRUE, size = 3) + scale_x_continuous(breaks = seq(2006, 2018, by = 1)) + theme_minimal() + scale_y_continuous(trans="log2") + scale_color_brewer(palette = "Paired")
#############################################################

 df_agravo_trat <- br_tratamento_agravos_plot  %>% group_by(tratamentoCID, Var1) %>% summarise("Freqt"  = sum(Freq))

df_obt_trat <- br_tratamento_obt_plot  %>% group_by(Tratamento_CID, Var1) %>% summarise("Freqt"  = sum(Freq))

brewer.pal(n = 12, name = "Paired")


df_agravo_trat <- select(df_agravo_trat, Procedimentos_hepC_CID_agravos = tratamentoCID, Var1, Freqt)

df_obt_trat <- select(df_obt_trat, Procedimentos_hepC_CID_Ã³bitos = tratamentoCID, Var1, Freqt)


############################
####### Barplots procedimentos x UF
###########################

dr_proc_uf <- br_tratamentos_plot %>% group_by(tratamentoCID, UF) %>% summarise("Freq"  = sum(Freq))

dr_proc_uf <- select( dr_proc_uf, Tratamento = tratamentoCID , UF, Freq  )
#### 1 #####
################################################
ggplot(data=dr_proc_uf , aes(x=reorder(UF, -Freq), y=Freq , fill=Tratamento)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Freq), position = position_stack(vjust = 0.8), size=2.5)+
  theme_minimal()  + labs(x="UF", y = "Frequência (log10)") + scale_color_brewer(palette = "Paired") + scale_y_log10()
###########################################

#### 2 #####
######################################################
ggplot(data=dr_proc_uf , aes(x=reorder(UF, -Freq), y=Freq , fill=Tratamento)) +
  geom_bar(stat="identity") +
  theme_minimal()  + labs(x="UF", y = "Frequência") + scale_color_brewer(palette = "Paired") 
######################################################

#### 3 ####
#######################################################
ggplot(data=dr_proc_uf , aes(x=reorder(UF, -Freq), y=Freq , fill=tratamentoCID)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal()  + labs(x="UF", y = "FrequÃªncia") + scale_color_brewer(palette = "Paired") 


#######################################################
#Split Dataframe into list based on the Type.of.Feature factor
factor_list <-split.data.frame(dr_proc_uf, f= dr_proc_uf$UF)


#Create new column with frequency sum for each of the level of factor above
for( lnam in names(factor_list)){
  factor_list[[lnam]]["group_sum"]<- sum(factor_list[[lnam]]["Freq"])
}

#Get back the data into dataframe
dr_proc_uf<- rbind_list(factor_list)

#Use newly created group frequency to order your bars
ggplot(dr_proc_uf, aes(x=reorder(UF, -group_sum), y=Freq, fill=tratamentoCID)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label=Freq), position = position_stack(vjust = 0.8), size=2.5)+
  theme_minimal()  + labs(x="UF", y = "FrequÃªncia") + scale_color_brewer(palette = "Paired") + scale_y_log10()




