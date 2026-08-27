###################################
# Fecha: Mayo 2020                #
# Topico: Lin estimator           #
###################################

# Base: wide12

# Estandarizacion de Glass
glass_delta <- function(Y, Z, name = "control"){
  Y/sd(Y[Z == name], na.rm = TRUE)
}

wide12$zofamilia2 <- glass_delta(Y=wide12$get_ah_1_w2,wide12$tratamiento)                
wide12$zhwork2 <- glass_delta(Y=wide12$get_ah_5_w2,wide12$tratamiento)
wide12$zedstr2 <- glass_delta(Y=wide12$get_ah_estr_w2,wide12$tratamiento)
wide12$zed2 <- glass_delta(Y=wide12$get_ah_3_w2,wide12$tratamiento)
wide12$zednew2 <- glass_delta(Y=wide12$get_ah_indv_w2,wide12$tratamiento)
wide12$zdvE2 <- glass_delta(Y=wide12$dvE,wide12$tratamiento)

# Procesamiento de datos
#############################
wide12b <- wide12 %>% dplyr::select(zofamilia2,zhwork2,zedstr2,zed2,zednew2,zdvE2,
                                    tratamiento,sexoW,edadW,edcepWB,
                                    estlabB,ingresosQ,comuna_w1C,
                                    get_ah_1_w1,get_ah_2_w1,ideo,weight)                                  

wide12b <- na.omit(wide12b)
dim(wide12b)

# Generacion de dummies
table(wide12b$sexoW,exclude=NULL)
wide12b$sexoW1 <- 0
wide12b$sexoW1[wide12b$sexoW==2]=1
table(wide12b$sexoW,wide12b$sexoW1,exclude=NULL)

table(wide12b$edcepWB,exclude=NULL)
wide12b$edcepWB1 <- 0
wide12b$edcepWB1[wide12b$edcepWB==1]=1
table(wide12b$edcepWB,wide12b$edcepWB1,exclude=NULL)
wide12b$edcepWB2 <- 0
wide12b$edcepWB2[wide12b$edcepWB==2]=1
table(wide12b$edcepWB,wide12b$edcepWB2,exclude=NULL)
wide12b$edcepWB3 <- 0
wide12b$edcepWB3[wide12b$edcepWB==3]=1
table(wide12b$edcepWB,wide12b$edcepWB3,exclude=NULL)

table(wide12b$estlabB,exclude=NULL)

table(wide12b$ingresosQ,exclude=NULL)
wide12b$ingresosQ2 <- 0
wide12b$ingresosQ2[wide12b$ingresosQ==2]=1
table(wide12b$ingresosQ,wide12b$ingresosQ2,exclude=NULL)
wide12b$ingresosQ3 <- 0
wide12b$ingresosQ3[wide12b$ingresosQ==3]=1
table(wide12b$ingresosQ,wide12b$ingresosQ3,exclude=NULL)
wide12b$ingresosQ4 <- 0
wide12b$ingresosQ4[wide12b$ingresosQ==4]=1
table(wide12b$ingresosQ,wide12b$ingresosQ4,exclude=NULL)
wide12b$ingresosQ5 <- 0
wide12b$ingresosQ5[wide12b$ingresosQ==5]=1
table(wide12b$ingresosQ,wide12b$ingresosQ5,exclude=NULL)

table(wide12b$comuna_w1C,exclude=NULL)
wide12b$comuna_w1C1 <- 0
wide12b$comuna_w1C1[wide12b$comuna_w1C==1]=1
table(wide12b$comuna_w1C,wide12b$comuna_w1C1,exclude=NULL)
wide12b$comuna_w1C2 <- 0
wide12b$comuna_w1C2[wide12b$comuna_w1C==2]=1
table(wide12b$comuna_w1C,wide12b$comuna_w1C2,exclude=NULL)

table(wide12b$ideo,exclude=NULL)
wide12b$ideo1 <- 0
wide12b$ideo1[wide12b$ideo==1]=1
table(wide12b$ideo,wide12b$ideo1,exclude=NULL)
wide12b$ideo2 <- 0
wide12b$ideo2[wide12b$ideo==2]=1
table(wide12b$ideo,wide12b$ideo2,exclude=NULL)
wide12b$ideo3 <- 0
wide12b$ideo3[wide12b$ideo==3]=1
table(wide12b$ideo,wide12b$ideo3,exclude=NULL)


wide12b1 <- dplyr::select(wide12b,sexoW1,edadW,edcepWB1,
                          edcepWB2,edcepWB3,estlabB,
                          ingresosQ2,ingresosQ3,ingresosQ4,ingresosQ5,
                          comuna_w1C1,comuna_w1C2,get_ah_1_w1,get_ah_2_w1,
                          ideo1,ideo2,ideo3,weight) 

covar <- as.matrix(wide12b1)


# --------------------- Lin estimator -------------------- #
library(estimatr)

# Solo control y pobreza
wide12c <- dplyr::filter(wide12b, tratamiento!="desigual") 
dim(wide12c)

lin1 <- lm_lin(zedstr2~tratamiento,data=wide12c,covariates=~weight)

summary(lin1)










