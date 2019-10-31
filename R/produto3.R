############ Produto 3 ###########################
##################################################
###### script desenvolvido por Mikael Lemos ######
###### vers?o 1.0 - 22.07.2019 ###################
##################################################

######
### Carregando / instalando pacotes
######

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
####### Comparando tabelas por diferençcas em colunas ##############################################################################
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
  
#### anti join quem não está morto por cirrose (até 2016)

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


#### anti join quem não está morto por carcinoma hepatocelular (até 2016)

#### Pessoas com ccarcinoma hepatocelular de 2008 - 2018 (novembro) 

hepc_carc_hepat_anti_join <- anti_join(hepc_carc_hepat_un, SIM_hepc_hepatocel,by="ID_PACIENTE")


### numero de mortos por carcinoma hepatocelular 2006 - 2016

hepc_carc_hepat_rigth_join <- right_join(hepc_carc_hepat_un, SIM_hepc_hepatocel,by="ID_PACIENTE")


### transplante de fígado ###

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

###  Transplante de fígado

hepc_trans <- do.call("rbind", list(APAC_hepc_trans_unitecol, AIH_hepc_trans_unitecol))

hepc_trans  <- distinct(hepc_trans)

hepc_trans_un <- distinct(hepc_trans, ID_PACIENTE, .keep_all = TRUE)


### anti join quem não está morto por cirrose (até 2016)

#### Pessoas com cirrose de 2008 - 2018 (novembro) 

hepc_carc_hepat_anti_join <- anti_join(hepc_carc_hepat_un, SIM_hepc_hepatocel,by="ID_PACIENTE")


### numero de mortos por cirrose 2006 - 2016

hepc_carc_hepat_rigth_join <- right_join(hepc_carc_hepat_un, SIM_hepc_hepatocel,by="ID_PACIENTE")


### anti join quem não está morto por transplante de fígado (até 2016)

#### Pessoas com transplante de fígado de 2008 - 2018 (novembro) 

hepc_trans_anti_join <- anti_join(hepc_trans_un, SIM_hepc_trans,by="ID_PACIENTE")


### numero de mortos por transplante de fígado 2006 - 2016

hepc_trans_right_join <- right_join(hepc_trans_un, SIM_hepc_trans,by="ID_PACIENTE")


############
######
###### OBS : as mortes foram checadas entre 2016 e 2018 (novembro) pelo CID_OBITO do AIH
###########

############################################################################
#############################  PLOTS #######################################
############################################################################

######## óbitos ########

SIM_hepc_cir$agravo_morte <- "cirrose"  

SIM_hepc_hepatocel$agravo_morte <- "carcinoma hepatocelular"

SIM_hepc_trans$agravo_morte <- "transplante de fígado"

morte_agravo_plot <- do.call("rbind", list(SIM_hepc_cir, SIM_hepc_hepatocel))

morte_agravo_plot <- table(morte_agravo_plot$agravo_morte)

morte_agravo_plot <- as.data.frame(morte_agravo_plot)

morte_agravo_plot$ano <- "HCV - 2006-2016"

################################ Plot mortes por agravos ##################################

ggplot(data=morte_agravo_plot, aes(x=ano, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width = 0.4) +
  geom_text(aes(label=Freq), position = position_stack(vjust = 0.5))+
  theme_minimal()  + labs(x="Hepatite viral", y = "Frequência de óbitos") + labs(fill = "Agravos")


######## agravos vivos - 2008 - 2018 ########

hepc_carc_hepat_anti_join <- unite(hepc_carc_hepat_anti_join, "CID", CID1, CID2, CID3,CID4, CID5, CID6, CID7, CID8, CID9, CID10, CID11, CID12, sep = " ")

hepc_carc_hepat_anti_join <- unite(hepc_carc_hepat_anti_join, "PROCEDIMENTO", PROCEDIMENTO1, PROCEDIMENTO2,PROCEDIMENTO3, sep = " ")


hepc_cirrose_anti_join <- unite(hepc_cirrose_anti_join, "CID", CID1, CID2, CID3,CID4, CID5, CID6, CID7, CID8, CID9, CID10, CID11, CID12, sep = " ")

hepc_cirrose_anti_join <- unite(hepc_cirrose_anti_join, "PROCEDIMENTO", PROCEDIMENTO1, PROCEDIMENTO2,PROCEDIMENTO3, sep = " ")


hepc_cirrose_anti_join$agravo <- "cirrose"  

hepc_carc_hepat_anti_join$agravo <- "carcinoma hepatocelular"

hepc_trans_anti_join$agravo <- "transplante de fígado"

agravo_plot <- do.call("rbind", list(hepc_cirrose_anti_join, hepc_carc_hepat_anti_join, hepc_trans_anti_join))

agravo_plot <- table(agravo_plot$agravo)

agravo_plot <- as.data.frame(agravo_plot)

agravo_plot$ano <- "HCV - 2008-2018"

################################ Plot mortes por agravos ##################################

ggplot(data=agravo_plot, aes(x=ano, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width = 0.4) +
  geom_text(aes(label=Freq), position = position_stack(vjust = 0.5))+
  theme_minimal()  + labs(x="Hepatite viral", y = "Frequência de agravos") + scale_fill_manual("Agravos", values = c("carcinoma hepatocelular" = "#f8766d", "cirrose" = "#00bfc4", "transplante de fígado" = "#a667d0"))


##########################################################################################
############################  Scatter plot agravos por ano ###############################
##########################################################################################

hepc_carc_hepat_anti_join <- hepc_carc_hepat_anti_join %>% separate(DT_OCOR, sep="-", into = c("ano", "mes", "dia"))

hepc_cirrose_anti_join <- hepc_cirrose_anti_join %>% separate(DT_OCOR, sep="-", into = c("ano", "mes", "dia"))

hepc_trans_anti_join <- hepc_trans_anti_join %>% separate(DT_OCOR, sep="-", into = c("ano", "mes", "dia"))

carc_hepat_plot <- table(hepc_carc_hepat_anti_join$ano)
carc_hepat_plot <- as.data.frame(carc_hepat_plot)
carc_hepat_plot$agravo <- "carcinoma hepatocelular"

cirrose_plot <- table(hepc_cirrose_anti_join$ano)
cirrose_plot <- as.data.frame(cirrose_plot)
cirrose_plot$agravo <- "cirrose"

trans_plot <- table(hepc_trans_anti_join$ano)
trans_plot <- as.data.frame(trans_plot)
trans_plot$agravo <- " transplante de fígado"

agravos_plot <- do.call("rbind", list(carc_hepat_plot, cirrose_plot, trans_plot))

################# scatter plot linhas ########################
ggplot(data = agravos_plot, aes(x = Var1, 
                             y = Freq, 
                             group=agravo, 
                             color=agravo )) +
  geom_line() +
  geom_point() + 
  labs( 
    y="Frequência", 
    x="Ano"
  ) + geom_text(aes(label=Freq),hjust=0, vjust=0, check_overlap = TRUE, size = 3) 
#############################################################
