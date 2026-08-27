
# data wave 1  ------------------------------------------------------------

library(sjlabelled) # Para funcion read_spss
library(dplyr)
library(stargazer)
library(texreg)

data01 <- read_spss(path = "Estudio_3_ola1_January+9,+2020_07.26.sav")


setwd('/Users/luismaldonado/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/Ola 1')
wave1 <- read_spss(path = "Estudio_3_ola1_January+9,+2020_07.26.sav")

dim(wave1) # 2457 respuestas
names(wave1)

# Aceptar, finalizar
#####################

# 2100 aceptan + finalizan

# Introduccion: Donde está la variable acepto?
# 1 es aceptar; 2 es no aceptar
table(wave1$Intro,exclude=NULL) # (2) 221 no aceptan, (1) 2236 aceptan

wave1a <- filter(wave1,Intro==1) # Elimina los que no aceptan
table(wave1a$Intro,exclude=NULL) # 2236 

# Variables para ver si finalizo encuesta
table(wave1a$Finished,exclude=NULL) # (1) 2100 finalizan, (0) 136 no terminan

wave1b <- filter(wave1a,Finished==1) # Elimina los que no aceptan
table(wave1b$Intro,exclude=NULL) # 2100


# Duracion
####################
summary(wave1b$Duration__in_seconds_) # media: 1665.7/60 = 27.76167 minutos

boxplot(wave1b$Duration__in_seconds_)

wave1b$duration2 <- (wave1b$Duration__in_seconds_/60) # Minutos
summary(wave1b$duration2)

quantile(wave1b$duration2,probs=c(0.1,0.25,0.5,0.75,0.9))
quantile(wave1b$duration2,probs=c(0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99))

wave1b$duration3 <- 0
wave1b$duration3[wave1b$duration2>240]=1 # Mayor del percentil 99
table(wave1b$duration3,exclude=NULL) # 2079 no estan, 21 si estan por sobre p99 (mas de 4 horas)


# --------------------- Variables sociodemograficas ----------------- #

# Sexo
###########
table(wave1b$sexo,exclude=NULL)
margin.table(table(wave1b$sexo,exclude=NULL))

# Edad
##########
table(wave1b$edad,exclude=NULL)
margin.table(table(wave1b$edad,exclude=NULL))

# Educacion, version netquest
###############################
table(wave1b$educat,exclude=NULL)
margin.table(table(wave1b$educat,exclude=NULL))

# Educacion, version cep
###############################
table(wave1b$edcep,exclude=NULL)
margin.table(table(wave1b$edcep,exclude=NULL))

# Comuna
###############################
table(wave1b$comuna,exclude=NULL) # 105 missings
margin.table(table(wave1b$comuna,exclude=NULL))

# Ingresos
###############################
table(wave1b$ingresos,exclude=NULL) # 2 missings
margin.table(table(wave1b$ingresos,exclude=NULL))

# Estatus laboral
###################
table(wave1b$estlab,exclude=NULL) # 6 missings
margin.table(table(wave1b$estlab,exclude=NULL))


# ------------------ Moderadores ----------------- #

# Variable de conocimiento de pobreza
##########################################
table(wave1b$percep_pov,exclude=NULL) # 0 missing!
margin.table(table(wave1b$percep_pov,exclude=NULL))

# Igualitarismo
######################
table(wave1b$egal_1,exclude=NULL) # 2 missing!
margin.table(table(wave1b$egal_1,exclude=NULL))

table(wave1b$egal_2,exclude=NULL) # 2 missing!
margin.table(table(wave1b$egal_2,exclude=NULL))

table(wave1b$egal_5,exclude=NULL) # 3 missing!
margin.table(table(wave1b$egal_5,exclude=NULL))

table(wave1b$egal_6,exclude=NULL) # 3 missing!
margin.table(table(wave1b$egal_6,exclude=NULL))

# --------------- Covariables ------------ #

# Get ahead
###############

table(wave1b$get_ah_1,exclude=NULL) # 0 missing!
margin.table(table(wave1b$get_ah_1,exclude=NULL))

table(wave1b$get_ah_2,exclude=NULL) # 0 missing!
margin.table(table(wave1b$get_ah_2,exclude=NULL))

table(wave1b$get_ah_3,exclude=NULL) # 0 missing!
margin.table(table(wave1b$get_ah_3,exclude=NULL))

table(wave1b$get_ah_4,exclude=NULL) # 0 missing!
margin.table(table(wave1b$get_ah_4,exclude=NULL))

table(wave1b$get_ah_5,exclude=NULL) # 0 missing!
margin.table(table(wave1b$get_ah_5,exclude=NULL))

table(wave1b$get_ah_6,exclude=NULL) # 0 missing!
margin.table(table(wave1b$get_ah_6,exclude=NULL))






