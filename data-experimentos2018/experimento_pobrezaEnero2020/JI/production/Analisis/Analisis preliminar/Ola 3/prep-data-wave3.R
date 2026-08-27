###################################
# Fecha: Septiembre 2020          #
# Topico: Preparacion datos ola 3 #
###################################

# Ver IPs duplicadas en 
# "fondecyt regular 2015\Estudio cuantitativo\data-experimentos2018\experimento_pobrezaEnero2020\JI\output\tables\ip_duplicated.htm" 

# Identificacion de casos duplicados para primera ola 1
###########################################################
# Paso 1
 # Crea objeto tab2 con los IDs duplicados
# Paso 2
 # Crea objeto dupw1b para identificar la primera respuesta de los IDs duplicados
 # En base a dupw1b, crea filterID para hacer filtro.
# Paso 3
 # En base a filtro crea la base w01c.


# Packages
##############
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

setwd('/Users/luismaldonado/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/JI/input/data/original')
w01 <- sjlabelled::read_spss("Estudio_3_ola1.sav",verbose = F)
w02 <- sjlabelled::read_spss("Estudio_3_ola2.sav",verbose = F)
w03 <- sjlabelled::read_spss("Estudio_3_ola3.sav",verbose = F)

# Aceptan 
########################

# Ola 1
table(w01$Intro,exclude=NULL) # (2) 221 no aceptan, (1) 2236 aceptan
w01a <- filter(w01,Intro==1) # Elimina los que no aceptan
table(w01a$Intro,exclude=NULL) # 2236 

# Ola 2
table(w02$Intro,exclude=NULL) # (2)  56 no aceptan, (1)  1809 aceptan, (3) 17 NAs
w02a <- filter(w02,Intro==1) # Elimina los que no aceptan
table(w02a$Intro,exclude=NULL) # 1809

# Ola 3
names(w03)
table(w03$Intro,exclude=NULL) # No hay Intro
w03a <- w03

# Preparar las olas
######################

# Preparar wave01
colnames(w01a) <- paste(colnames(w01a), "w1", sep = "_") # Add suffix for wave 01 identification
w01a$ID <- stringr::str_split_fixed(w01a$ticket,"_", 4)[,1] # En la variable ticket el primer código es el ID, todo lo demás se borra

w01b <- w01a 
w01b$grupoEM <- NA
w01b$grupoEM[w01b$FL_21_DO_merit_perc_pref_julio19v01_w1==1] <- 1 #Versión A de escala meritocracia
w01b$grupoEM[w01b$FL_21_DO_merit_perc_pref_julio19v02_w1==1] <- 2 #Versión B de escala meritocracia
w01b$grupoEM[w01b$FL_21_DO_merit_perc_pref_julio19v03_w1==1] <- 3 #Versión C de escala meritocracia
w01b$grupoEM <- factor(w01b$grupoEM,levels = c(1,2,3),labels = c("A","B","C"))

# Preparar wave02
colnames(w02a) <- paste(colnames(w02a), "w2", sep = "_") # Add suffix for wave 02 identification
w02a$ID <- stringr::str_split_fixed(w02a$ticket, "_", 4)[,1]  # En la variable ticket el primer código es el ID, todo lo demás se borra

# Tratamiento
w02b <- w02a
w02b$tratamiento <- NA
w02b$tratamiento[w02b$FL_6_DO_Control_w2==1] <- 0 
w02b$tratamiento[w02b$FL_6_DO_Tratamiento_w2==1] <- 1 
w02b$tratamiento[w02b$FL_6_DO_Tratamiento_desigualdad_w2==1] <- 2 
w02b$tratamiento <- factor(w02b$tratamiento,levels = c(0,1,2),labels = c("control","pobreza","desigual"))

table(w02b$tratamiento,exclude=NULL) # 0 Control, 1 Pobreza, 2 Desigualdad

# Prepapar wave03
colnames(w03a) <- paste(colnames(w03a), "w3", sep = "_") # Add suffix for wave 02 identification
w03a$ID <- stringr::str_split_fixed(w03a$ticket, "_", 4)[,1]  # En la variable ticket el primer código es el ID, todo lo demás se borra

w03b <- w03a 
names(w03b)

# Casos duplicados
####################

# Ola 1
# Paso 1
a <- dplyr::select(w01b,ID)
tab1<- questionr::freq(a$ID)
tab2 <- subset(x = tab1,subset = (n>1))
dim(tab2) # Tenemos 30 casos con ID duplicado.

# Paso 2: para que?
dupw1 <- w01b %>% filter(ID %in% rownames(tab2)) %>% 
  dplyr::select(ID,ResponseId_w1,everything())

dupw1a <- dupw1 %>% group_by(ID) %>% summarize(maxDate=max(StartDate_w1))
dupw1b <- dupw1 %>% group_by(ID,ResponseId_w1) %>% summarize(maxDate=max(StartDate_w1)) %>% filter(maxDate %in% max(maxDate)) 
dupw1a$maxDate==dupw1b$maxDate # Check

# Paso 3
filterID <- as.character(dupw1b$ResponseId_w1) #vector para realizar filtro

w01c<- filter(w01b, !(ResponseId_w1 %in% filterID)) #w02 solo con primera respuesta para duplicados 
dim(w01b) # 2236
dim(w01c) # 2206, 28 casos eliminados.


# Ola 2
b <- dplyr::select(w02b,ID)
tab3<- questionr::freq(b$ID) # tabla de frecuencia con IDs
tab4 <- subset(x = tab3,subset = (n>1)) # tabla de frecuencia para identificar si Ids esta mas de 1 vez.
dim(tab4) # Tenemos 28 casos son ID duplicado.

dupw2 <- w02b %>% filter(ID %in% rownames(tab4)) %>% # Base solo con duplicados
  dplyr::select(ID,tratamiento,ResponseId_w2,everything())

dupw2a <- dupw2 %>% group_by(ID) %>% summarize(maxDate=max(StartDate_w2))
dupw2b <- dupw2 %>% group_by(ID,ResponseId_w2) %>% summarize(maxDate=max(StartDate_w2)) %>% filter(maxDate %in% max(maxDate)) 
dupw2a$maxDate==dupw2b$maxDate # Check

filterID <- as.character(dupw2b$ResponseId_w2) #vector para realizar filtro

w02c<- filter(w02b, !(ResponseId_w2 %in% filterID)) #w02 solo con primera respuesta para duplicados 
dim(w02b) # 1809
dim(w02c) # 1781, 28 casos eliminados.

w02c$ID2 <- 1:1781 # Identificador

#Ola 3
b <- dplyr::select(w03b,ID)
tab3<- questionr::freq(b$ID)
tab4 <- subset(x = tab3,subset = (n>1))
dim(tab4) # Tenemos 36 casos son ID duplicado.

dupw3 <- w03b %>% filter(ID %in% rownames(tab4)) %>% 
  dplyr::select(ID,ResponseId_w3,everything())

dupw3a <- dupw3 %>% group_by(ID) %>% summarize(maxDate=max(StartDate_w3))
dupw3b <- dupw3 %>% group_by(ID,ResponseId_w3) %>% summarize(maxDate=max(StartDate_w3)) %>% filter(maxDate %in% max(maxDate)) 
dupw3a$maxDate=dupw3b$maxDate # Check

filterID <- as.character(dupw3b$ResponseId_w3) #vector para realizar filtro

w03c<- filter(w03b, !(ResponseId_w3 %in% filterID)) #w02 solo con primera respuesta para duplicados 
dim(w03b) # 1489
dim(w03c) # 1453, 36 casos eliminados.

w03c$ID3 <- 1:1453 # Identificador

# --------------- Outcomes -------------- #
# Indicadores estructurales
table(w03c$get_ah_estr_w3,exclude=NULL) # Educacion de calidad
prop.table(table(w03c$get_ah_estr_w3,exclude=NULL)) # 22 NAs
w03c$get_ah_estr_w3a <- w03c$get_ah_estr_w3
w03c$get_ah_estr_w3a[is.na(w03c$get_ah_estr_w3)]=mean(w03c$get_ah_estr_w3,na.rm=T)
table(w03c$get_ah_estr_w3a,exclude=NULL)

table(w03c$get_ah_1_w3,exclude=NULL) # Familia rica
prop.table(table(w03c$get_ah_1_w3,exclude=NULL)) # 22 NAs
w03c$get_ah_1_w3a <- w03c$get_ah_1_w3
w03c$get_ah_1_w3a[is.na(w03c$get_ah_1_w3)]=mean(w03c$get_ah_1_w3,na.rm=T)
table(w03c$get_ah_1_w3a,exclude=NULL)

# Indicadores individuales
table(w03c$get_ah_3_w3,exclude=NULL) # Buen nivel educacional
prop.table(table(w03c$get_ah_3_w3,exclude=NULL)) # 22 NAs
w03c$get_ah_3_w3a <- w03c$get_ah_3_w3
w03c$get_ah_3_w3a[is.na(w03c$get_ah_3_w3)]=mean(w03c$get_ah_3_w3,na.rm=T)
table(w03c$get_ah_3_w3a,exclude=NULL)

table(w03c$get_ah_5_w3,exclude=NULL) # Trabajo duro
prop.table(table(w03c$get_ah_5_w3,exclude=NULL)) # 22 NAs
w03c$get_ah_5_w3a <- w03c$get_ah_5_w3 
w03c$get_ah_5_w3a[is.na(w03c$get_ah_5_w3)]=mean(w03c$get_ah_5_w3,na.rm=T)
table(w03c$get_ah_5_w3a,exclude=NULL)

table(w03c$get_ah_indv_w3,exclude=NULL) # Haber alcanzado un buen nivel de educación
prop.table(table(w03c$get_ah_indv_w3,exclude=NULL)) # 22 NAs
w03c$get_ah_indv_w3a <- w03c$get_ah_indv_w3
w03c$get_ah_indv_w3a[is.na(w03c$get_ah_indv_w3)]=mean(w03c$get_ah_indv_w3,na.rm=T)
table(w03c$get_ah_indv_w3a,exclude=NULL)

names(w03c)


# Merge
############
wide23 <- merge(w02c,w03c, by = "ID") 
dim(wide23) # 1454, 202 variables
names(wide23)

# Abrir wide12
wide123 <- merge(wide12,w03c, by = "ID") 
dim(wide123) # 1454, 209 variables 
names(wide123)

# Check
table(w03c$get_ah_estr_w3a,exclude=NULL)
table(wide123$get_ah_estr_w3a,exclude=NULL)

table(w03c$get_ah_1_w3a,exclude=NULL) # Check valor de 2
table(wide123$get_ah_1_w3a,exclude=NULL)

table(w03c$get_ah_3_w3a,exclude=NULL)
table(wide123$get_ah_3_w3a,exclude=NULL)

table(w03c$get_ah_5_w3a,exclude=NULL)
table(wide123$get_ah_5_w3a,exclude=NULL)

table(w03c$get_ah_indv_w3a,exclude=NULL) # Check valor de 4
table(wide123$get_ah_indv_w3a,exclude=NULL)

table(wide123$tratamiento,exclude=NULL)

# Indice de educacion
wide123$dvE3 <- summary(princomp(~get_ah_estr_w3a+get_ah_3_w3a+get_ah_indv_w3a,
                               data=wide123,na.action=na.exclude))$scores[,1]

summary(wide123$dvE3) # 0 NAs


# ------------- Estimaciones ---------------- #

library(texreg)
library(xtable)
library(estimatr)

# Estandarizacion 2
glass_delta <- function(Y, Z, name = "control"){
  Y/sd(Y[Z == name], na.rm = TRUE)
}

wide123$zofamilia2 <- glass_delta(Y=wide123$get_ah_1_w3a,wide123$tratamiento)                
wide123$zhwork2 <- glass_delta(Y=wide123$get_ah_5_w3a,wide123$tratamiento)
wide123$zedstr2 <- glass_delta(Y=wide123$get_ah_estr_w3a,wide123$tratamiento)
wide123$zed2 <- glass_delta(Y=wide123$get_ah_3_w3a,wide123$tratamiento)
wide123$zednew2 <- glass_delta(Y=wide123$get_ah_indv_w3a,wide123$tratamiento)
wide123$zdvE3 <- glass_delta(Y=wide123$dvE3,wide123$tratamiento)


# -----------------------ATEs------------------- #
# Origen
###########
m1a <- lm_robust(zofamilia2~factor(tratamiento),data=wide123)

m1b <- lm_robust(zofamilia2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                  data=wide123)

screenreg(l=list(m1a,m1b), single.row = TRUE, stars = c(0.01, 0.05))

# Trabajo duro
######################
m2a <- lm_robust(zhwork2~factor(tratamiento),data=wide123)

m2b <- lm_robust(zhwork2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide123)

screenreg(l=list(m2a,m2b), single.row = TRUE, stars = c(0.01, 0.05))

# Indice educacion
#####################
m3a <- lm_robust(zdvE3~factor(tratamiento),data=wide123)

m3b <- lm_robust(zdvE3~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide123)

screenreg(l=list(m3a,m3b), single.row = TRUE, stars = c(0.01, 0.05))




# ---------------- Comparaciones frecuencias --------------- #

# Educacion de calidad
table(wide123$get_ah_estr_w3a,exclude=NULL)
table(wide123$get_ah_estr_w2a,exclude=NULL)

wide123 %>% select(get_ah_estr_w3a,get_ah_estr_w2a) %>% cor() # 0.4025531 

# Familia rica
table(wide123$get_ah_1_w3a,exclude=NULL)
table(wide123$get_ah_1_w2a,exclude=NULL)

wide123 %>% select(get_ah_1_w3a,get_ah_1_w2a) %>% cor() # 0.5500154 

# Buen nivel educacional usted
table(wide123$get_ah_3_w3a,exclude=NULL)
table(wide123$get_ah_3_w2a,exclude=NULL)

wide123 %>% select(get_ah_3_w3a,get_ah_3_w2a) %>% cor() # 0.3380899

# Trabajo duro
table(wide123$get_ah_5_w3a,exclude=NULL)
table(wide123$get_ah_5_w2a,exclude=NULL)

wide123 %>% select(get_ah_5_w3a,get_ah_5_w2a) %>% cor() # 0.4194961 

# Haber alcanzado un buen nivel de educación
table(wide123$get_ah_indv_w3a,exclude=NULL)
table(wide123$get_ah_indv_w2a,exclude=NULL)

wide123 %>% select(get_ah_indv_w3a,get_ah_indv_w2a) %>% cor() # 0.3636803 

# Indice de educacion
summary(wide123$dvE)
summary(wide123$dvE3)

wide123 %>% select(dvE,dvE3) %>% cor() # 0.4495385 


# ----------------- Variables sociodemograficas ------------------- #

# Ingreso
table(wide123$ingresos_w1,exclude=NULL)
table(wide12$ingresos_w1,exclude=NULL)

prop.table(table(wide123$ingresos_w1,exclude=NULL))
prop.table(table(wide12$ingresos_w1,exclude=NULL))


w123 <- (c(0.004129387,0.008258775,0.002752925,0.008258775,0.010323469,0.013076394,0.028905712, 
0.023399862,0.072264281,0.124569855,0.362009635,0.212663455,0.077770131,0.045423262,0.006194081))*100

w12 <- (c(0.004491859,0.007860752,0.003368894,0.010106682,0.010668164,0.012914093,0.027512633, 
0.026389669,0.076923077,0.128017967,0.358225716,0.209432903,0.075238630,0.042672656,0.006176305))*100 

w12 - w123







