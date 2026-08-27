###################################
# Fecha: Agosto 2020              #
# Topico: Preparacion ingreso     #
###################################

library(haven)
library(sjlabelled)
library(sjPlot)
library(foreign)



# Ejecutar prep-covariables-LM.R

sjPlot::view_df(x = as.data.frame(wide12$ingresos_w1),max.len=100,show.prc = T)


# ------- Ingreso I: version categorica ----------- #
table(wide12$ingresos_w1,exclude=NULL)

q <- wide12 %>% dplyr::select(ingresos_w1)
q <- na.omit(q)
quantile(q$ingresos_w1)

wide12$ingresosQ <- 1 # q1
wide12$ingresosQ[wide12$ingresos_w1==11]=2 # q2
wide12$ingresosQ[wide12$ingresos_w1==12]=3 # q3
wide12$ingresosQ[wide12$ingresos_w1>12]=4 # q4
wide12$ingresosQ[is.na(wide12$ingresos_w1)]=5 # NA
table(wide12$ingresos_w1,wide12$ingresosQ,exclude=NULL)
table(wide12$ingresosQ,exclude=NULL)

# -------- Ingreso II: version lineal, imputacion max. ----------- #
library(binsmooth)
library(binequality)
library(datapasta)

# Middle points
###################

# Data base
# df_paste()

data1 <-data.frame(
  id = c(1,1,1,1,1,1,
         1,1,1,1,1,1,1,1),
  hb = c(8,14,6,18,19,
         23,49,47,137,228,638,373,134,76),
  binmin = c(0,35001,56001,
             78001,101001,134001,179001,224001,291001,
             358001,448001,1000001,2000001,3000001),
  binmax = c(35000,56000,78000,
             101000,134000,179000,224000,291000,358000,
             448000,1000000,2000000,3000000,NA)
)

data1

# Estimations: RPME, gini: 0.440835 
bin_mids <- getMids(ID=data1$id,hb=data1$hb,lb=data1$binmin,ub=data1$binmax,alpha_bound=numeric(0))
bin_mids # alpha: 2.506688, last bin: 4991123.6

bin_mids_stats <- midStats(data = bin_mids$mids) # Estimacion de Gini.
bin_mids_stats

# Tramos de ingreso
bin1=(35000 + 0)/2 # 17500
bin2=(35001+56000)/2 # 45500.5
bin3=(56001+78000)/2 # 67000.5
bin4=(78001+101000)/2 # 89500.5
bin5=(101001+134000)/2 # 117500.5
bin6=(134001+179000)/2 # 156500.5
bin7=(179001+224000)/2 # 201500.5
bin8=(224001+291000)/2 # 257500.5
bin9=(291001+358000)/2 # 324500.5
bin10=(358001+448000)/2 # 403000.5
bin11=(448001+1000000)/2 # 724000.5
bin12=(1000001+2000000)/2 # 1500000.5
bin13=(2000001+3000000)/2 # 2500000.5

# Variable ingreso
wide12$ingresosL <- NA
wide12$ingresosL[wide12$ingresos_w1==1]=bin1
wide12$ingresosL[wide12$ingresos_w1==2]=bin2
wide12$ingresosL[wide12$ingresos_w1==3]=bin3
wide12$ingresosL[wide12$ingresos_w1==4]=bin4
wide12$ingresosL[wide12$ingresos_w1==5]=bin5
wide12$ingresosL[wide12$ingresos_w1==6]=bin6
wide12$ingresosL[wide12$ingresos_w1==7]=bin7
wide12$ingresosL[wide12$ingresos_w1==8]=bin8
wide12$ingresosL[wide12$ingresos_w1==9]=bin9
wide12$ingresosL[wide12$ingresos_w1==10]=bin10
wide12$ingresosL[wide12$ingresos_w1==11]=bin11
wide12$ingresosL[wide12$ingresos_w1==12]=bin12
wide12$ingresosL[wide12$ingresos_w1==13]=bin13
wide12$ingresosL[wide12$ingresos_w1==14]=4991123.6

mean(wide12$ingresosL,na.rm=T) # 1074959

wide12$ingresosL[is.na(wide12$ingresos_w1)]=1074959 # reemplazo de missings
table(wide12$ingresosL,exclude=NULL)

table(wide12$ingresos_w1,exclude=NULL)
table(wide12$ingresosL,exclude=NULL)

rm(bin1,bin2,bin3,bin4,bin5,bin6,bin7,bin8,bin9,bin10,bin11,bin12,bin13)

# Correlacion con igualitarismo
corr <- dplyr::select(wide12,ingresosL,irt_z1)
summary(corr)
cor(corr) # -0.2080914

# ----------- Comparacion con ingreso CEP --------------- #

# netquest
################
table(wide12$ingresos_w1,exclude=NULL)

wide12$ingresosLb <- 1 # 403000.5
wide12$ingresosLb[wide12$ingresosL==724000.5]=2 
wide12$ingresosLb[wide12$ingresosL==1500000.5]=3 
wide12$ingresosLb[wide12$ingresosL==1074959]=3 
wide12$ingresosLb[wide12$ingresosL>1500000.5]=4 
wide12$ingresosLb[is.na(wide12$ingresosL)]=NA 
table(wide12$ingresosLb,wide12$ingresos_w1,exclude=NULL)


netquest <- wide12 %>% dplyr::select(ingresos_w1)
netquest <- na.omit(netquest)
(prop.table(table(netquest$ingresos_w1)))*100 # continua


netquest <- wide12 %>% dplyr::select(ingresosLb)
(prop.table(table(netquest$ingresosLb)))*100 # categorica


# cep julio 2019, toda la poblacion
#######################################
library(haven)
setwd('/Users/luismaldonado/Downloads/Bases/encuesta_cep83_jun2019')
cep83 <- read_sav("CEP83.sav", encoding = 'latin1')

table(cep83$DS_P39,exclude=NULL)
cep83$income <- cep83$DS_P39
cep83$income[cep83$DS_P39>14]=NA
table(cep83$income,exclude=NULL)
(prop.table(table(cep83$income)))*100

cep83$incb <- 1
cep83$incb[cep83$DS_P39==11]=2 
cep83$incb[cep83$DS_P39==12]=3 
cep83$incb[cep83$DS_P39>12]=4 
cep83$incb[cep83$DS_P39>14]=NA 
table(cep83$incb,cep83$DS_P39,exclude=NULL)

#cep con ponderador, sin missing en income!
cep <- cep83 %>% dplyr::select(income,incb,FACTOR)
cep <- na.omit(cep)

library(survey)
surveyobject <- svydesign(ids = ~0, data = cep, weights = cep$FACTOR) 
svymean(~interaction(income),surveyobject) # lineal

a <- c(0.0019683, 
0.0034654, 
0.0059997, 
0.0124491, 
0.0230110, 
0.0289918, 
0.0536668, 
0.0747837, 
0.1025855, 
0.1437297, 
0.3402853, 
0.1323124, 
0.0412188, 
0.0355325) 

a <- as.data.frame(a)

svymean(~interaction(incb),surveyobject) # Categorico: 45.07, 34.03, 13.23, 7.68

# Base imputada, ver codigo stata
b <- c(0.20,0.33,0.54,1.19,2.63,3.00,5.53,7.18,10.11,13.57,34.32,14.19,4.15,3.05) # imputado y ponderado
b <- as.data.frame(b) # lineal

c <- c(44.19,34.35,14.31,7.15) # imputada y ponderada, linea 62 en codigo Stata, ver descargas
c <- as.data.frame(c) # categorica

# Casen
##########

setwd('/Users/luismaldonado/Downloads/Bases/Casen2017')
casen17 = read.dta("Casen2017.dta")
dim(casen17)
names(casen17)



# Comparacion
################

# No agrupada
(prop.table(table(netquest$ingresos_w1)))*100 # netquest
a*100 # cep con ponderador, sin imputacion
b # cep imputada

# Tramos
(prop.table(table(netquest$ingresosLb)))*100
c #cep, imputada















