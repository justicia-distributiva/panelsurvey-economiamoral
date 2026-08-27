#############################
# Fecha: Febrero 2020       #
# Topico: Preparacion datos #
#############################

# Ver IPs duplicadas en 
# "fondecyt regular 2015\Estudio cuantitativo\data-experimentos2018\experimento_pobrezaEnero2020\JI\output\tables\ip_duplicated.htm" 

# Tareas
  # Preparacion de olas: creacion de ID, version escala meritocracia, crea var. para tratamientos
  # Casos duplicados
  # Merge: base en formato wide
  # Check demograficos: coincidencia y NAs
  # Visualizacion base definitiva
  # Generacion de ponderadores y merge con base definitiva

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

setwd('/Users/luismaldonado/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/JI/input/data/original')
w01 <- sjlabelled::read_spss("Estudio_3_ola1.sav",verbose = F)
w02 <- sjlabelled::read_spss("Estudio_3_ola2.sav",verbose = F)

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

# Merge
############
wide12 <- full_join(w01c,w02c, by = "ID") 
dim(wide12) # 2209, hay 3 casos que no estan en la ola 1 pero si en la ola 2

# Check de demograficos
############################
wide12 <- arrange(wide12,ID2) # sort

wide12 <- filter(wide12,ID2!="NA") # Se dejan los 1781 que estan en ambas olas.
dim(wide12)

# Sexo: 1 es hombre, 2 Mujer
wide12$sexoC <- wide12$sexo_w1 - wide12$sexo_w2
table(wide12$sexoC,exclude=NULL) # 1737 coinciden

table(wide12$sexo_w1,exclude=NULL) # 4 missings
table(wide12$sexo_w2,exclude=NULL) # 39 missings

wide12 <- wide12 %>% 
  mutate(sexoW=NA,
         sexoW=ifelse(sexo_w1==1 & sexo_w2==1,1,
               ifelse(sexo_w1==2 & sexo_w2==2,2,
               ifelse(sexo_w1!=sexo_w2,sexo_w1,sexoW)))) # Se usa valor en la primera ola

wide12$sexoW <- with(wide12, ifelse(is.na(sexo_w1),sexo_w2,sexoW)) # Reemplazo NA               
wide12$sexoW <- with(wide12, ifelse(is.na(sexo_w2),sexo_w1,sexoW)) # Reemplazo NA  

table(wide12$sexoW,exclude=NULL)

# Edad
wide12$edadC <- wide12$edad_w1 - wide12$edad_w2
table(wide12$edadC,exclude=NULL) # 1702 coinciden

table(wide12$edad_w1,exclude=NULL) # 4 missings
table(wide12$edad_w2,exclude=NULL) # 38 missings

wide12 <- wide12 %>% 
  mutate(edadW=NA,
         edadW=ifelse(edad_w1==1 & edad_w2==1,1, # 18-24
               ifelse(edad_w1==2 & edad_w2==2,2, # 25-34
               ifelse(edad_w1==3 & edad_w2==3,3, # 35-44
               ifelse(edad_w1==4 & edad_w2==4,4, # 45-54
               ifelse(edad_w1==5 & edad_w2==5,5, # 55 o mas
               ifelse(edad_w1!=edad_w2,edad_w1,edadW))))))) # Se usa valor en la primera ola

wide12$edadW <- with(wide12, ifelse(is.na(edad_w1),edad_w2,edadW)) # Reemplazo NA               
wide12$edadW <- with(wide12, ifelse(is.na(edad_w2),edad_w1,edadW)) # Reemplazo NA  

table(wide12$edadW,exclude=NULL)

# Educacion
wide12$edcepC <- wide12$edcep_w1 - wide12$edcep_w2
table(wide12$edcepC,exclude=NULL) # 1396 coinciden, 338 no coinciden, 47 NA

wide12$edcepCb <- NA
wide12$edcepCb[wide12$edcepC==0]=1
wide12$edcepCb[wide12$edcepC!=0]=0
wide12$edcepCb[is.na(wide12$edcepC)]=NA
table(wide12$edcepCb,exclude=NULL) # 1396 coinciden

table(wide12$edcep_w1,exclude=NULL) # 9 missings
table(wide12$edcep_w2,exclude=NULL) # 38 missings

wide12 <- wide12 %>% 
  mutate(edcepW=NA,
         edcepW=ifelse(edcep_w1==1 & edcep_w2==1,1,
                ifelse(edcep_w1==2 & edcep_w2==2,2,
                ifelse(edcep_w1==3 & edcep_w2==3,3,
                ifelse(edcep_w1==4 & edcep_w2==4,4,
                ifelse(edcep_w1==5 & edcep_w2==5,5,
                ifelse(edcep_w1==6 & edcep_w2==6,6,
                ifelse(edcep_w1==7 & edcep_w2==7,7,
                ifelse(edcep_w1==8 & edcep_w2==8,8, 
                ifelse(edcep_w1==9 & edcep_w2==9,9,
                ifelse(edcep_w1==10 & edcep_w2==10,10,
                ifelse(edcep_w1!=edcep_w2,edcep_w1,edcepW)))))))))))) # Se usa valor en la primera ola

wide12$edcepW <- with(wide12, ifelse(is.na(edcep_w1),edcep_w2,edcepW)) # Reemplazo NA               
wide12$edcepW <- with(wide12, ifelse(is.na(edcep_w2),edcep_w1,edcepW)) # Reemplazo NA  

table(wide12$edcepW,exclude=NULL)

# Comuna
# Hay comunas de netquest que no estan en CEP. 
# Ponderadores no considera comunas.

sjPlot::view_df(x = as.data.frame(wide12$comuna_w1),max.len=100,show.prc = T)
# Regiones: Antofagasta,Valparaíso,Viña del Mar,Concepción,Talcahuano,
#           Arica,Iquique,Calama,Copiapó,La Serena,Coquimbo,Rancagua,
#           Curicó,Talca,Chillán,Temuco,Puerto Montt,Osorno,Coyhaique,Valdivia

# Santiago: Cerrillos,Cerro Navia,Conchalí,El Bosque,Estación Central,Huechuraba,
#           Independencia,La Cisterna,La Florida,La Granja,La Pintana,La Reina,Las Condes,
#           Lo Barnechea,Lo Espejo,Lo Prado,Macul,Maipú,Ñuñoa,Pedro Aguirre Cerda,Peñalolén,
#           Providencia,Pudahuel,Puente Alto,Quilicura,Quinta Normal,Recoleta,Renca,San Bernardo,
#           San Joaquín,San Miguel,San Ramón,Santiago,Vitacura

wide12$comuna_w1B <-NA # Nueva variable de comuna con loops, 1 Santiago, 0 regiones

for (i in 1065:1069){
  wide12$comuna_w1B[wide12$comuna_w1==i]=0 
}

for (i in 1070:1103){
  wide12$comuna_w1B[wide12$comuna_w1==i]=1 
}

for (i in 1104:1158){
  wide12$comuna_w1B[wide12$comuna_w1==i]=0 
}

table(wide12$comuna_w1,exclude=NULL)
table(wide12$comuna_w1B,exclude=NULL) # 1=1282 es Santiago, 0=400 en regiones, 99 NAs

wide12$comuna_w1C <- 0
wide12$comuna_w1C[wide12$comuna_w1B==1]=1
wide12$comuna_w1C[is.na(wide12$comuna_w1B)]=2
table(wide12$comuna_w1B,wide12$comuna_w1C,exclude=NULL)

# Visualizacion base definitiva
##################################
dim(wide12) # 1781
names(wide12)

# Eliminacion de variables
wide12$sexoC <- NULL
wide12$edadC <- NULL
wide12$edcepC <- NULL
wide12$edcepCb <- NULL

sjPlot::view_df(x = wide12,show.values = T,show.frq = T,show.prc = T)

# Generacion de ponderadores y merge con base definitiva
#########################################################
# Ponderadores no considera comunas, solo sexo, edad y educacion (formato cep recodificado)

library(foreign)

# Para ingresos, ejecutar parte de ingreso1.R

wide12Weight <- dplyr::select(wide12,ID2,sexoW,edadW,edcepW,ingresosLb)
str(wide12Weight)
write.dta(wide12Weight,'/Users/luismaldonado/Desktop/wide12Weight.dta')


# Merge sin considerar ingresos en weights
#################################################
setwd('/Users/luismaldonado/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/LM/Weighting')
wide12Weight <- read_stata("wide12Weight.dta")
str(wide12Weight)

# Merge
wide12Weight$ID2 <- as.integer(wide12Weight$ID2)
wide12 <- full_join(wide12,wide12Weight, by = "ID2") 
names(wide12)  

rm(tab1,tab2,tab3,tab4)
rm(w01,w01a,w01b,w01c)
rm(w02,w02a,w02b,w02c)
rm(wide12Weight)
rm(filterID)
save.image("~/Desktop/wide12.RData")



# Merge sin considerar ingresos en weights
#################################################
setwd('/Users/luismaldonado/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/LM/Weighting')
wide12Weight <- read_stata("wide12Weight.dta")
str(wide12Weight)

# Merge
wide12Weight$ID2 <- as.integer(wide12Weight$ID2)
wide12 <- full_join(wide12,wide12Weight, by = "ID2") 
names(wide12)  











