#####################################
# Fecha: Septiembre 2020            #
# Topico: Comparacion con ISSP 2019 #
#####################################

library(plyr)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(questionr)

# Experimento
load("~/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/JI/input/data/proc/wide12.RData")

# Issp 2019
setwd('/Users/luismaldonado/Downloads/Bases/encuesta_cep83_jun2019')
cep83 <- sjlabelled::read_spss("CEP83.sav",verbose = F, enc='latin1')

# Experimento
####################
# Familia rica
# 1 No es importante 
# 2 No muy importante 
# 3 Bastante importante 
# 4 Muy importante
# 5 Esencial

# Familia rica
sjPlot::view_df(x = as.data.frame(wide12$get_ah_1_w2),max.len=200,show.prc = T)
prop.table(table(wide12$get_ah_1_w2[wide12$tratamiento=="control"])) 

wide12$origen=NA
wide12$origen[wide12$get_ah_1_w2==1]=5
wide12$origen[wide12$get_ah_1_w2==2]=4
wide12$origen[wide12$get_ah_1_w2==3]=3
wide12$origen[wide12$get_ah_1_w2==4]=2
wide12$origen[wide12$get_ah_1_w2==5]=1
table(wide12$origen,wide12$get_ah_1_w2,exclude=NULL)

prop.table(table(wide12$origen[wide12$tratamiento=="control"])) 

# Trabajo duro
sjPlot::view_df(x = as.data.frame(wide12$get_ah_5_w2),max.len=200,show.prc = T)
prop.table(table(wide12$get_ah_5_w2[wide12$tratamiento=="control"])) 

wide12$trabajo=NA
wide12$trabajo[wide12$get_ah_5_w2==1]=5
wide12$trabajo[wide12$get_ah_5_w2==2]=4
wide12$trabajo[wide12$get_ah_5_w2==3]=3
wide12$trabajo[wide12$get_ah_5_w2==4]=2
wide12$trabajo[wide12$get_ah_5_w2==5]=1
table(wide12$trabajo,wide12$get_ah_5_w2,exclude=NULL)

prop.table(table(wide12$trabajo[wide12$tratamiento=="control"])) 


# Issp
############
# 5 No es importante 
# 4 No muy importante 
# 3 Bastante importante 
# 2 Muy importante
# 1 Esencial

# Trabajo duro
sjPlot::view_df(x = as.data.frame(cep83$M2_P1_5),max.len=200,show.prc = T)
table(cep83$M2_P1_5,exclude=NULL)

cep83$trabajo <- cep83$M2_P1_5
cep83$trabajo[cep83$M2_P1_5>5]=NA
table(cep83$trabajo,cep83$M2_P1_5,exclude=NULL)
prop.table(table(cep83$trabajo))
prop.table(wtd.table(cep83$trabajo,weights = cep83$FACTOR))

# Provenir de una familia rica
sjPlot::view_df(x = as.data.frame(cep83$M2_P1_2),max.len=200,show.prc = T)
table(cep83$M2_P1_2,exclude=NULL)

cep83$familia <- cep83$M2_P1_2
cep83$familia[cep83$M2_P1_2>5]=NA
table(cep83$familia,cep83$M2_P1_2,exclude=NULL)
prop.table(table(cep83$familia))
prop.table(wtd.table(cep83$familia,weights = cep83$FACTOR))

# Comparacion
#################
# 5 No es importante 
# 4 No muy importante 
# 3 Bastante importante 
# 2 Muy importante
# 1 Esencial

# Origen
prop.table(wtd.table(cep83$familia,weights = cep83$FACTOR)) # 1+2: 0.314558
prop.table(table(wide12$origen[wide12$tratamiento=="control"])) # 1+2: 0.295416

# Trabajo
prop.table(wtd.table(cep83$trabajo,weights = cep83$FACTOR)) # 1+2: 0.7312166
prop.table(table(wide12$trabajo[wide12$tratamiento=="control"])) # 1+2: 0.6604414




