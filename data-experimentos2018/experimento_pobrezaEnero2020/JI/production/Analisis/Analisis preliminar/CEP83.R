###################################
# Fecha: Agosto 2020              #
# Topico: Encuesta CEP 83         #
###################################

rm(list=ls())

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

library(haven)
library(sjlabelled)
library(sjPlot)
library(foreign)
library(stargazer)

library(car)
library(questionr) # Uso de ponderador.

# -------------------- Bases ------------------- #
setwd('/Users/luismaldonado/Downloads/Bases/encuesta_cep83_jun2019')
cep83 <- sjlabelled::read_spss("CEP83.sav",verbose = F, enc='latin1')
dim(cep83)
names(cep83)

# Ingreso 1
###################
sjPlot::view_df(x = as.data.frame(cep83$DS_P39),max.len=200,show.prc = T)
table(cep83$DS_P39,exclude=NULL)

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

# Tramo superior
Ltop=3000001
V=(log(17+15)-log(15))/(log(3000001)-log(2000001))
bin14=1/2*Ltop*(1+V/(V-1))

# Variable ingreso
cep83$income <- NA
cep83$income[cep83$DS_P39==1]=bin1
cep83$income[cep83$DS_P39==2]=bin2
cep83$income[cep83$DS_P39==3]=bin3
cep83$income[cep83$DS_P39==4]=bin4
cep83$income[cep83$DS_P39==5]=bin5
cep83$income[cep83$DS_P39==6]=bin6
cep83$income[cep83$DS_P39==7]=bin7
cep83$income[cep83$DS_P39==8]=bin8
cep83$income[cep83$DS_P39==9]=bin9
cep83$income[cep83$DS_P39==10]=bin10
cep83$income[cep83$DS_P39==11]=bin11
cep83$income[cep83$DS_P39==12]=bin12
cep83$income[cep83$DS_P39==13]=bin13
cep83$income[cep83$DS_P39==14]=4726752
table(cep83$DS_P39,exclude=NULL)
table(cep83$income,exclude=NULL)


# Ingreso 2
###################
library(binequality)

table(cep83$DS_P39,exclude=NULL)

# Data without imputation
data2 <-data.frame(
  id = c(1,1,1,1,1,1,
         1,1,1,1,1,1,1,1),
  hb = c(3,4,5,22,40,40,65,72,98,124,260,85,17,15),
  binmin = c(0,35001,56001,
             78001,101001,134001,179001,224001,291001,
             358001,448001,1000001,2000001,3000001),
  binmax = c(35000,56000,78000,
             101000,134000,179000,224000,291000,358000,
             448000,1000000,2000000,3000000,NA)
)

data2


# Data with imputation 1
data2Imp <-data.frame(
  id = c(1,1,1,1,1,1,
         1,1,1,1,1,1,1,1),
  hb = c(5,8,11,28,59,58,96,112,167,194,422,161,35,24),
  binmin = c(0,35001,56001,
             78001,101001,134001,179001,224001,291001,
             358001,448001,1000001,2000001,3000001),
  binmax = c(35000,56000,78000,
             101000,134000,179000,224000,291000,358000,
             448000,1000000,2000000,3000000,NA)
)

data2Imp 


# Data with imputation 2: con weight!!
data2Imp2 <-data.frame(
  id = c(1,1,1,1,1,1,
         1,1,1,1,1,1,1,1),
  hb = c(2.75772422,6.67064055,8.43888863,15.2601457,33.3961657,42.9127459,
	     75.6907777,98.1499982,148.480843,186.6075166,449.4557097,209.37125,60.3255946,42.4819993),
  binmin = c(0,35001,56001,
             78001,101001,134001,179001,224001,291001,
             358001,448001,1000001,2000001,3000001),
  binmax = c(35000,56000,78000,
             101000,134000,179000,224000,291000,358000,
             448000,1000000,2000000,3000000,NA)
)

data2Imp2 



# Estimations without imputations: RPME, gini: 0.479576
bin_mids <- getMids(ID=data2$id,hb=data2$hb,lb=data2$binmin,ub=data2$binmax,alpha_bound=numeric(0))
bin_mids # alpha: 2.151167, last bin: 6453503.4 

bin_mids_stats <- midStats(data = bin_mids$mids)
bin_mids_stats


# Estimations with imputations 1: RPME, gini: 0.4651378
bin_mids <- getMids(ID=data2Imp$id,hb=data2Imp$hb,lb=data2Imp$binmin,ub=data2Imp$binmax,alpha_bound=numeric(0))
bin_mids # alpha: 2.2184, last bin: 5462246.5

bin_mids_stats <- midStats(data = bin_mids$mids)
bin_mids_stats


# Estimations with imputations 2: RPME, gini: 0.4751433
bin_mids <- getMids(ID=data2Imp2$id,hb=data2Imp2$hb,lb=data2Imp2$binmin,ub=data2Imp2$binmax,alpha_bound=numeric(0))
bin_mids # alpha: 2.179668, last bin: 5543091.2 

bin_mids_stats <- midStats(data = bin_mids$mids)
bin_mids_stats















