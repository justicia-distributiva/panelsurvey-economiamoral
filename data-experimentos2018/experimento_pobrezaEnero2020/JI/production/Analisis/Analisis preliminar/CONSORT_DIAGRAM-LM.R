############################################
# Fecha: Mayo 2020                         #
# Topico: CONSORT Participant Flow Diagram #
############################################

# JULIO: linea 82

# Example: Boas (2016).

# 4 etapas
  # 1: Assessed for eligibility
  # 2: Randomized
  # 3: Check manipulation y atencion
  # 4: Outcomes

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# 1: Assessed for eligibility: 
# 1781 aceptan participar y tienen solo 1 respuesta 
########################################################
  # Is the number providing a valid response to the consent question, not including those who may have viewed the consent form but clicked away without beginning the survey.

setwd('/Users/luismaldonado/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/JI/input/data/original')
w01 <- sjlabelled::read_spss("Estudio_3_ola1.sav",verbose = F)
w02 <- sjlabelled::read_spss("Estudio_3_ola2.sav",verbose = F)

# Ola 1
table(w01$Intro,exclude=NULL) # (2) 221 no aceptan, (1) 2236 aceptan
w01a <- filter(w01,Intro==1) # Elimina los que no aceptan
table(w01a$Intro,exclude=NULL) # 2236 

# Ola 2
table(w02$Intro,exclude=NULL) # (2)  56 no aceptan, (1)  1809 aceptan, (3) 17 NAs
w02a <- filter(w02,Intro==1) # Elimina los que no aceptan
table(w02a$Intro,exclude=NULL) # 1809

# Ola 2
# Preparar wave02
colnames(w02a) <- paste(colnames(w02a), "w2", sep = "_") # Add suffix for wave 02 identification
w02a$ID <- stringr::str_split_fixed(w02a$ticket, "_", 4)[,1]  # En la variable ticket el primer código es el ID, todo lo demás se borra

w02b <- w02a
w02b$tratamiento <- NA
w02b$tratamiento[w02b$FL_6_DO_Control_w2==1] <- 0 
w02b$tratamiento[w02b$FL_6_DO_Tratamiento_w2==1] <- 1 
w02b$tratamiento[w02b$FL_6_DO_Tratamiento_desigualdad_w2==1] <- 2 
w02b$tratamiento <- factor(w02b$tratamiento,levels = c(0,1,2),labels = c("control","pobreza","desigual"))

# Eliminacion de casos duplicados
b <- dplyr::select(w02b,ID)
tab3<- questionr::freq(b$ID)
tab4 <- subset(x = tab3,subset = (n>1))
dim(tab4) # Tenemos 28 casos son ID duplicado.

dupw2 <- w02b %>% filter(ID %in% rownames(tab4)) %>% 
  dplyr::select(ID,tratamiento,ResponseId_w2,everything())

dupw2a <- dupw2 %>% group_by(ID) %>% summarize(maxDate=max(StartDate_w2))
dupw2b <- dupw2 %>% group_by(ID,ResponseId_w2) %>% summarize(maxDate=max(StartDate_w2)) %>% filter(maxDate %in% max(maxDate)) 
dupw2a$maxDate==dupw2b$maxDate # Check

filterID <- as.character(dupw2b$ResponseId_w2) #vector para realizar filtro

w02c<- filter(w02b, !(ResponseId_w2 %in% filterID)) #w02 solo con primera respuesta para duplicados 
dim(w02b) # 1809
dim(w02c) # 1781, 28 casos eliminados.

w02c$ID2 <- 1:1781 # Identificador

# Aceptacion
table(w02c$Intro,exclude=NULL) # 1781

# 2: Randomized:
###########################
        # Poverty: 592
        # Inequality: 595
        # Placebo: 592
        # Total: 
        # 2 NAs. JULIO***********

table(w02c$FL_6_DO_Tratamiento_w2,exclude=NULL) # 592
table(w02c$FL_6_DO_Control_w2,exclude=NULL) # 592
table(w02c$FL_6_DO_Tratamiento_desigualdad_w2,exclude=NULL) # 595

w02c$tratamiento <- NA
w02c$tratamiento[w02c$FL_6_DO_Control_w2==1] <- 0 
w02c$tratamiento[w02c$FL_6_DO_Tratamiento_w2==1] <- 1 
w02c$tratamiento[w02c$FL_6_DO_Tratamiento_desigualdad_w2==1] <- 2 
w02c$tratamiento <- factor(w02c$tratamiento,levels = c(0,1,2),labels = c("control","pobreza","desigual"))

table(w02c$tratamiento,exclude=NULL) # 0 Control, 1 Pobreza, 2 Desigualdad

# 3: Check manipulation y atencion
#######################################

# Check de manipulation
# Variable generada: manip
table(w02c$mani_poverty_w2,exclude=NULL) # N=586, deberian ser 592
prop.table(table(w02c$mani_poverty_w2)) # 80% correcto

table(w02c$mani_placebo_w2,exclude=NULL) # N=589, deberian ser 592
prop.table(table(w02c$mani_placebo_w2)) # 83% correcto

table(w02c$mani_desigualdad_w2,exclude=NULL) # N=587, deberian ser 595
prop.table(table(w02c$mani_desigualdad_w2)) # 79% correcto

w02c$manip <- 0 # Incorrectos + NAs = Otros
w02c$manip[w02c$mani_poverty_w2==4]=1 # correcto pobreza
w02c$manip[w02c$mani_placebo_w2==3]=2 # correcto placebo
w02c$manip[w02c$mani_desigualdad_w2==3]=3 # correcto desigualdad
table(w02c$manip,exclude=NULL)
prop.table(table(w02c$manip,exclude=NULL))

# Check de atencion
# Variable generada: atencionB
table(w02c$atencion_w2,exclude=NULL) # 1724 ponen atencion, 57 no ponen atencion

w02c$atencionB <- 0
w02c$atencionB[w02c$atencion_w2==5]=1
table(w02c$atencionB,w02c$atencion_w2,exclude=NULL)

# 4: Outcomes
##################

# Control: 589, 3 NAs
# Pobreza: 583, 9 NAs
# Desigual: 587, 8 NAs
# 2 NAs

# Indicadores estructurales
table(w02c$get_ah_estr_w2,exclude=NULL) # Educacion de calidad, 22 NAs
table(w02c$get_ah_estr_w2,w02c$tratamiento,exclude=NULL)
margin.table(table(w02c$get_ah_estr_w2,w02c$tratamiento),2)

table(w02c$get_ah_1_w2,exclude=NULL) # Familia rica, 22 NAs
table(w02c$get_ah_1_w2,w02c$tratamiento,exclude=NULL) 
margin.table(table(w02c$get_ah_1_w2,w02c$tratamiento),2) 

# Indicadores individuales
table(w02c$get_ah_3_w2,exclude=NULL) # Buen nivel educacional, 22 NAs
table(w02c$get_ah_3_w2,w02c$tratamiento,exclude=NULL) 
margin.table(table(w02c$get_ah_3_w2,w02c$tratamiento),2) 

table(w02c$get_ah_5_w2,exclude=NULL) # Trabajo duro, 22 NAs
table(w02c$get_ah_5_w2,w02c$tratamiento,exclude=NULL) 
margin.table(table(w02c$get_ah_5_w2,w02c$tratamiento),2) 

table(w02c$get_ah_indv_w2,exclude=NULL) # Haber alcanzado un buen nivel de educación, 22 NAs
table(w02c$get_ah_indv_w2,w02c$tratamiento,exclude=NULL)
margin.table(table(w02c$get_ah_indv_w2,w02c$tratamiento),2)





















