################################################
# Fecha: Octubre 2020                          #
# Topico: Estudio replicacion, septiembre 2020 #
################################################

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

setwd('/Users/luismaldonado/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/JI/input/data/original')
w01 <- sjlabelled::read_spss("Estudio_4_ola1.sav",verbose = F)
names(w01)

# Aceptan 
########################

# Ola 1
table(w01$consen,exclude=NULL) # (2) 245 no aceptan, (1) 2115 aceptan
w01a <- filter(w01,consen==1) # Elimina los que no aceptan
table(w01a$consen,exclude=NULL) # 2115


# Tratamiento
########################
w01a$tratamiento <- NA
w01a$tratamiento[w01a$FL_551_DO_Control==1] <- 0 
w01a$tratamiento[w01a$FL_551_DO_Tratamiento_pobreza==1] <- 1 
w01a$tratamiento[w01a$FL_551_DO_Tratamiento_desigualdad==1] <- 2 
w01a$tratamiento <- factor(w01a$tratamiento,levels = c(0,1,2),labels = c("control","pobreza","desigual"))

table(w01a$tratamiento,exclude=NULL) # 0 Control, 1 Pobreza, 2 Desigualdad



# --------------- Outcomes -------------- #
with(w01a,summary(get_ah_1)) # Educacion calidad
sjPlot::view_df(x = as.data.frame(w01a$get_ah_1),max.len=100,show.prc = T)

with(w01a,summary(get_ah_2)) # Origen
sjPlot::view_df(x = as.data.frame(w01a$get_ah_2),max.len=100,show.prc = T)

with(w01a,summary(get_ah_3)) # Usted buen nivel educacional
sjPlot::view_df(x = as.data.frame(w01a$get_ah_3),max.len=100,show.prc = T)

with(w01a,summary(get_ah_4)) # Trabajo duro
sjPlot::view_df(x = as.data.frame(w01a$get_ah_4),max.len=100,show.prc = T)

with(w01a,summary(get_ah_5)) # buen nivel educacional
sjPlot::view_df(x = as.data.frame(w01a$get_ah_5),max.len=100,show.prc = T)

# Indicadores estructurales
table(w01a$get_ah_2,exclude=NULL) # Origen
prop.table(table(w01a$get_ah_2,exclude=NULL)) # 65 NAs
w01a$get_ah_2a <- w01a$get_ah_2
w01a$get_ah_2a[is.na(w01a$get_ah_2)]=mean(w01a$get_ah_2,na.rm=T)
table(w01a$get_ah_2a,exclude=NULL)

# Indicadores individuales
table(w01a$get_ah_4,exclude=NULL) # trabajo duro
prop.table(table(w01a$get_ah_4,exclude=NULL)) # 65 NAs
w01a$get_ah_4a <- w01a$get_ah_4
w01a$get_ah_4a[is.na(w01a$get_ah_4)]=mean(w01a$get_ah_4,na.rm=T)
table(w01a$get_ah_4a,exclude=NULL)

# Indicadores educacionales
table(w01a$get_ah_1,exclude=NULL) # Educacion de calidad
prop.table(table(w01a$get_ah_1,exclude=NULL)) # 65 NAs
w01a$get_ah_1a <- w01a$get_ah_1
w01a$get_ah_1a[is.na(w01a$get_ah_1)]=mean(w01a$get_ah_1,na.rm=T)
table(w01a$get_ah_1a,exclude=NULL)

table(w01a$get_ah_3,exclude=NULL) # usted buen nivel educacional
prop.table(table(w01a$get_ah_3,exclude=NULL)) # 65 NAs
w01a$get_ah_3a <- w01a$get_ah_3
w01a$get_ah_3a[is.na(w01a$get_ah_3)]=mean(w01a$get_ah_3,na.rm=T)
table(w01a$get_ah_3a,exclude=NULL)

table(w01a$get_ah_5,exclude=NULL) # buen nivel educacional
prop.table(table(w01a$get_ah_5,exclude=NULL)) # 65 NAs
w01a$get_ah_5a <- w01a$get_ah_5
w01a$get_ah_5a[is.na(w01a$get_ah_5)]=mean(w01a$get_ah_5,na.rm=T)
table(w01a$get_ah_5a,exclude=NULL)

w01a$dvE <- summary(princomp(~get_ah_1a+get_ah_3a+get_ah_5a,
                               data=w01a,na.action=na.exclude))$scores[,1]

summary(w01a$dvE) # 0 NAs


# ------------- Estimaciones ---------------- #

library(texreg)
library(xtable)
library(estimatr)

# Estandarizacion 2
glass_delta <- function(Y, Z, name = "control"){
  Y/sd(Y[Z == name], na.rm = TRUE)
}

w01a$zofamilia2 <- glass_delta(Y=w01a$get_ah_2a,w01a$tratamiento)                
w01a$zhwork2 <- glass_delta(Y=w01a$get_ah_4a,w01a$tratamiento)
w01a$zdvE2 <- glass_delta(Y=w01a$dvE,w01a$tratamiento)

# Origen
###########
m1a <- lm_robust(zofamilia2~factor(tratamiento),data=w01a)
summary(m1a)

# Trabajo duro
######################
m2a <- lm_robust(zhwork2~factor(tratamiento),data=w01a)
summary(m2a)

# Indice educacion
#####################
m3a <- lm_robust(zdvE2~factor(tratamiento),data=w01a)
summary(m3a)







