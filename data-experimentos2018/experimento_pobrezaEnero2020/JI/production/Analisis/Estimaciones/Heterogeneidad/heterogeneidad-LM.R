###################################
# Fecha: Junio 2020               #
# Topico: Heterogeneidad          #
###################################

  # Importantes
    # Origen y tratamiento de pobreza
    # Origen y tratamiento de desigualdad



# Ejecutar prep.covariables-LM.R
library(texreg)
library(xtable)
library(estimatr)
library(margins)

# Estandarizacion 2
glass_delta <- function(Y, Z, name = "control"){
  Y/sd(Y[Z == name], na.rm = TRUE)
}

wide12$zofamilia2 <- glass_delta(Y=wide12$get_ah_1_w2a,wide12$tratamiento)                
wide12$zhwork2 <- glass_delta(Y=wide12$get_ah_5_w2a,wide12$tratamiento)
wide12$zedstr2 <- glass_delta(Y=wide12$get_ah_estr_w2a,wide12$tratamiento)
wide12$zed2 <- glass_delta(Y=wide12$get_ah_3_w2a,wide12$tratamiento)
wide12$zednew2 <- glass_delta(Y=wide12$get_ah_indv_w2a,wide12$tratamiento)
wide12$zdvE2 <- glass_delta(Y=wide12$dvE,wide12$tratamiento)

# Generacion de dummies
##########################

# Tratamiento
wide12$Tplacebo=0
wide12$Tplacebo[wide12$tratamiento=="control"]=1
wide12$Tplacebo[is.na(wide12$tratamiento)]=NA
table(wide12$Tplacebo,wide12$tratamiento,exclude=NULL)

wide12$Tpob=0
wide12$Tpob[wide12$tratamiento=="pobreza"]=1
wide12$Tpob[is.na(wide12$tratamiento)]=NA
table(wide12$Tpob,wide12$tratamiento,exclude=NULL)

wide12$Tdes=0
wide12$Tdes[wide12$tratamiento=="desigual"]=1
wide12$Tdes[is.na(wide12$tratamiento)]=NA
table(wide12$Tdes,wide12$tratamiento,exclude=NULL)

# Educacion
table(wide12$edcepWB,exclude=NULL)

wide12$basica=0
wide12$basica[wide12$edcepWB==0]=1
wide12$basica[is.na(wide12$edcepWB)]=NA
table(wide12$basica,wide12$edcepWB,exclude=NULL)

wide12$media=0
wide12$media[wide12$edcepWB==1]=1
wide12$media[is.na(wide12$edcepWB)]=NA
table(wide12$media,wide12$edcepWB,exclude=NULL)

wide12$suptec=0
wide12$suptec[wide12$edcepWB==2]=1
wide12$suptec[is.na(wide12$edcepWB)]=NA
table(wide12$suptec,wide12$edcepWB,exclude=NULL)

wide12$supun=0
wide12$supun[wide12$edcepWB==3]=1
wide12$supun[is.na(wide12$edcepWB)]=NA
table(wide12$supun,wide12$edcepWB,exclude=NULL)

# Percepcion pobreza
table(wide12$percep_pov2,exclude=NULL)

wide12$igualpov=0
wide12$igualpov[wide12$percep_pov2==1]=1
wide12$igualpov[is.na(wide12$percep_pov2)]=NA
table(wide12$igualpov,wide12$percep_pov2,exclude=NULL)

wide12$mayorpov=0
wide12$mayorpov[wide12$percep_pov2==2]=1
wide12$mayorpov[is.na(wide12$percep_pov2)]=NA
table(wide12$mayorpov,wide12$percep_pov2,exclude=NULL)

# Ingreso
table(wide12$ingresosL,exclude=NULL)

# -------- Estimaciones OLS, moderador es ingreso -------- #
# Poor
wide12$ingresosLb <- 0
wide12$ingresosLb[wide12$ingresosL>403000.5]=1
table(wide12$ingresosLb,wide12$ingresosL)
prop.table(table(wide12$ingresosLb)) #69% is 1

# Origen
###########
origen <- lm_robust(zofamilia2~Tpob+Tdes+ingresosLb+
                      Tpob:ingresosLb+Tdes:ingresosLb+
                      media+suptec+supun+
                      factor(sexoW)+edadW+factor(estlabB)+
                      factor(comuna_w1C)+
                      get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                      data=wide12)

summary(origen)
summary(margins(origen,variables="Tpob", at=list(ingresosLb=range(wide12$ingresosLb))))
summary(margins(origen,variables="Tdes", at=list(ingresosLb=range(wide12$ingresosLb))))


# Trabajo duro
################
work <- lm_robust(zhwork2~Tpob+Tdes+ingresosLb+
                      Tpob:ingresosLb+Tdes:ingresosLb+
                      media+suptec+supun+
                      factor(sexoW)+edadW+factor(estlabB)+
                      factor(comuna_w1C)+
                      get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                      data=wide12)

summary(work)
summary(margins(work,variables="Tpob", at=list(ingresosLb=range(wide12$ingresosLb))))
summary(margins(work,variables="Tdes", at=list(ingresosLb=range(wide12$ingresosLb))))


# -------- Estimaciones OLS, moderador es educacion -------- #

# Origen
###########
origen <- lm_robust(zofamilia2~Tpob+Tdes+media+suptec+supun+
                     Tpob:media+Tpob:suptec+Tpob:supun+
                     Tdes:media+Tdes:suptec+Tdes:supun+
                     factor(sexoW)+edadW+factor(estlabB)+
                     ingresosL+factor(comuna_w1C)+
                     get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                     data=wide12)

summary(origen)

# Basica
summary(margins(origen,variables="Tpob", at=list(media=0,suptec=0,supun=0)))
summary(margins(origen,variables="Tdes", at=list(media=0,suptec=0,supun=0)))

# Media
summary(margins(origen,variables="Tpob", at=list(media=1,suptec=0,supun=0)))
summary(margins(origen,variables="Tdes", at=list(media=1,suptec=0,supun=0)))

# Superior no universitaria
summary(margins(origen,variables="Tpob", at=list(media=0,suptec=1,supun=0)))
summary(margins(origen,variables="Tdes", at=list(media=0,suptec=1,supun=0)))

# Superior universitaria
summary(margins(origen,variables="Tpob", at=list(media=0,suptec=0,supun=1)))
summary(margins(origen,variables="Tdes", at=list(media=0,suptec=0,supun=1)))


# Trabajo duro
######################
work <- lm_robust(zhwork2~Tpob+Tdes+media+suptec+supun+
                      Tpob:media+Tpob:suptec+Tpob:supun+
                      Tdes:media+Tdes:suptec+Tdes:supun+
                      factor(sexoW)+edadW+factor(estlabB)+
                      factor(ingresosQ)+factor(comuna_w1C)+
                      get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                      data=wide12)

summary(work)

# Basica
summary(margins(work,variables="Tpob", at=list(media=0,suptec=0,supun=0)))
summary(margins(work,variables="Tdes", at=list(media=0,suptec=0,supun=0)))

# Media
summary(margins(work,variables="Tpob", at=list(media=1,suptec=0,supun=0)))
summary(margins(work,variables="Tdes", at=list(media=1,suptec=0,supun=0)))

# Superior no universitaria
summary(margins(work,variables="Tpob", at=list(media=0,suptec=1,supun=0)))
summary(margins(work,variables="Tdes", at=list(media=0,suptec=1,supun=0)))

# Superior universitaria
summary(margins(work,variables="Tpob", at=list(media=0,suptec=0,supun=1)))
summary(margins(work,variables="Tdes", at=list(media=0,suptec=0,supun=1)))


# Haber accedido a una educación de calidad 
################################################
educ1 <- lm_robust(zedstr2~Tpob+Tdes+media+suptec+supun+
                            Tpob:media+Tpob:suptec+Tpob:supun+
                            Tdes:media+Tdes:suptec+Tdes:supun+
                            factor(sexoW)+edadW+factor(estlabB)+
                            factor(ingresosQ)+factor(comuna_w1C)+
                            get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                            data=wide12)

summary(educ1)

# Basica
summary(margins(educ1,variables="Tpob", at=list(media=0,suptec=0,supun=0)))
summary(margins(educ1,variables="Tdes", at=list(media=0,suptec=0,supun=0)))

# Media
summary(margins(educ1,variables="Tpob", at=list(media=1,suptec=0,supun=0)))
summary(margins(educ1,variables="Tdes", at=list(media=1,suptec=0,supun=0)))

# Superior no universitaria
summary(margins(educ1,variables="Tpob", at=list(media=0,suptec=1,supun=0)))
summary(margins(educ1,variables="Tdes", at=list(media=0,suptec=1,supun=0)))

# Superior universitaria
summary(margins(educ1,variables="Tpob", at=list(media=0,suptec=0,supun=1)))
summary(margins(educ1,variables="Tdes", at=list(media=0,suptec=0,supun=1)))


# Haber alcanzado un buen nivel de educación 
##############################################
educ2 <- lm_robust(zed2~Tpob+Tdes+media+suptec+supun+
                     Tpob:media+Tpob:suptec+Tpob:supun+
                     Tdes:media+Tdes:suptec+Tdes:supun+
                     factor(sexoW)+edadW+factor(estlabB)+
                     factor(ingresosQ)+factor(comuna_w1C)+
                     get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                     data=wide12)

summary(educ2)

# Basica
summary(margins(educ2,variables="Tpob", at=list(media=0,suptec=0,supun=0)))
summary(margins(educ2,variables="Tdes", at=list(media=0,suptec=0,supun=0)))

# Media
summary(margins(educ2,variables="Tpob", at=list(media=1,suptec=0,supun=0)))
summary(margins(educ2,variables="Tdes", at=list(media=1,suptec=0,supun=0)))

# Superior no universitaria
summary(margins(educ2,variables="Tpob", at=list(media=0,suptec=1,supun=0)))
summary(margins(educ2,variables="Tdes", at=list(media=0,suptec=1,supun=0)))

# Superior universitaria
summary(margins(educ2,variables="Tpob", at=list(media=0,suptec=0,supun=1)))
summary(margins(educ2,variables="Tdes", at=list(media=0,suptec=0,supun=1)))


# Que usted tenga un buen nivel de educación 
################################################
educ3 <- lm_robust(zednew2~Tpob+Tdes+media+suptec+supun+
                     Tpob:media+Tpob:suptec+Tpob:supun+
                     Tdes:media+Tdes:suptec+Tdes:supun+
                     factor(sexoW)+edadW+factor(estlabB)+
                     factor(ingresosQ)+factor(comuna_w1C)+
                     get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                     data=wide12)

summary(educ3)

# Basica
summary(margins(educ3,variables="Tpob", at=list(media=0,suptec=0,supun=0)))
summary(margins(educ3,variables="Tdes", at=list(media=0,suptec=0,supun=0)))

# Media
summary(margins(educ3,variables="Tpob", at=list(media=1,suptec=0,supun=0)))
summary(margins(educ3,variables="Tdes", at=list(media=1,suptec=0,supun=0)))

# Superior no universitaria
summary(margins(educ3,variables="Tpob", at=list(media=0,suptec=1,supun=0)))
summary(margins(educ3,variables="Tdes", at=list(media=0,suptec=1,supun=0)))

# Superior universitaria
summary(margins(educ3,variables="Tpob", at=list(media=0,suptec=0,supun=1)))
summary(margins(educ3,variables="Tdes", at=list(media=0,suptec=0,supun=1)))


# Indice educacion
#####################
educ4 <- lm_robust(zdvE2~Tpob+Tdes+media+suptec+supun+
                     Tpob:media+Tpob:suptec+Tpob:supun+
                     Tdes:media+Tdes:suptec+Tdes:supun+
                     factor(sexoW)+edadW+factor(estlabB)+
                     ingresosL+factor(comuna_w1C)+
                     get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                     data=wide12)

summary(educ4)

# Basica
summary(margins(educ4,variables="Tpob", at=list(media=0,suptec=0,supun=0)))
summary(margins(educ4,variables="Tdes", at=list(media=0,suptec=0,supun=0)))

# Media
summary(margins(educ4,variables="Tpob", at=list(media=1,suptec=0,supun=0)))
summary(margins(educ4,variables="Tdes", at=list(media=1,suptec=0,supun=0)))

# Superior no universitaria
summary(margins(educ4,variables="Tpob", at=list(media=0,suptec=1,supun=0)))
summary(margins(educ4,variables="Tdes", at=list(media=0,suptec=1,supun=0)))

# Superior universitaria
summary(margins(educ4,variables="Tpob", at=list(media=0,suptec=0,supun=1)))
summary(margins(educ4,variables="Tdes", at=list(media=0,suptec=0,supun=1)))


# -------- Estimaciones OLS, moderador es igualitarismo 1 -------- #

# Distribucion igualitarismo
boxplot(wide12$irt_z1~wide12$tratamiento)

# Bases separadas
wide12pob <- dplyr::filter(wide12,Tdes==0) 
wide12pob <- dplyr::select(wide12pob,zofamilia2,zhwork2,zdvE2,Tpob,irt_z1)
wide12pob <- na.omit(wide12pob)
summary(wide12pob)

wide12des <- dplyr::filter(wide12,Tpob==0) 
wide12des <- dplyr::select(wide12des,zofamilia2,zhwork2,zdvE2,Tdes,irt_z1)
wide12des <- na.omit(wide12des)
summary(wide12des)

# Origen
###########
#library(interplot)
library(interflex)

# 1: raw, LID plot
inter.raw(Y = "zofamilia2", D = "Tpob", X = "irt_z1", data = wide12pob, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")
 
inter.raw(Y = "zofamilia2", D = "Tdes", X = "irt_z1", data = wide12des, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

# 2: binning
out <- interflex(Y = "zofamilia2", D = "Tpob", X = "irt_z1", data = wide12pob, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

out <- interflex(Y = "zofamilia2", D = "Tdes", X = "irt_z1", data = wide12des, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests


# Trabajo duro
#################
# 1: raw, LID plot
inter.raw(Y = "zhwork2", D = "Tpob", X = "irt_z1", data = wide12pob, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

inter.raw(Y = "zhwork2", D = "Tdes", X = "irt_z1", data = wide12des, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

# 2: binning
out <- interflex(Y = "zhwork2", D = "Tpob", X = "irt_z1", data = wide12pob, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

out <- interflex(Y = "zhwork2", D = "Tdes", X = "irt_z1", data = wide12des, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

# Indice educacion
#####################
# 1: raw, LID plot
inter.raw(Y = "zdvE2", D = "Tpob", X = "irt_z1", data = wide12pob, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

inter.raw(Y = "zdvE2", D = "Tdes", X = "irt_z1", data = wide12des, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

# 2: binning
out <- interflex(Y = "zdvE2", D = "Tpob", X = "irt_z1", data = wide12pob, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

out <- interflex(Y = "zdvE2", D = "Tdes", X = "irt_z1", data = wide12des, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

# -------- Estimaciones OLS, moderador es igualitarismo 2 -------- #

# 50%
wide12$irt_z1b <- 0
wide12$irt_z1b[wide12$irt_z1>-0.02410968]=1
table(wide12$irt_z1,wide12$irt_z1b)
table(wide12$irt_z1b,exclude=NULL)


# Origen
###########
origen2a <- lm_robust(zofamilia2~Tpob+Tdes+irt_z1b+
                                Tpob:irt_z1b+Tdes:irt_z1b+
                                factor(sexoW)+edadW+factor(estlabB)+factor(edcepWB)+
                                ingresosL+factor(comuna_w1C)+
                                get_ah_1_w1+get_ah_2_w1+factor(ideo)+igualpov+mayorpov,data=wide12)

summary(origen2a)
summary(margins(origen2a,variables="Tpob", at=list(irt_z1b=range(wide12$irt_z1b))))
summary(margins(origen2a,variables="Tdes", at=list(irt_z1b=range(wide12$irt_z1b))))

# Trabajo duro
#################
work2 <- lm_robust(zhwork2~Tpob+Tdes+irt_z1b+
                       Tpob:irt_z1b+Tdes:irt_z1b+
                       factor(sexoW)+edadW+factor(estlabB)+factor(edcepWB)+
                       ingresosL+factor(comuna_w1C)+
                       get_ah_1_w1+get_ah_2_w1+factor(ideo)+igualpov+mayorpov,data=wide12)

summary(work2)
summary(margins(work2,variables="Tpob", at=list(irt_z1b=range(wide12$irt_z1b))))
summary(margins(work2,variables="Tdes", at=list(irt_z1b=range(wide12$irt_z1b))))

# Indice educacion
#####################
educ2 <- lm_robust(zdvE2~Tpob+Tdes+irt_z1b+
                     Tpob:irt_z1b+Tdes:irt_z1b+
                     factor(sexoW)+edadW+factor(estlabB)+factor(edcepWB)+
                     factor(ingresosQ)+factor(comuna_w1C)+
                     get_ah_1_w1+get_ah_2_w1+factor(ideo)+igualpov+mayorpov,data=wide12)

summary(educ2)
summary(margins(educ2,variables="Tpob", at=list(irt_z1b=range(wide12$irt_z1b))))
summary(margins(educ2,variables="Tdes", at=list(irt_z1b=range(wide12$irt_z1b))))


# -------- Estimaciones OLS, moderador es percepcion pobreza -------- #

# Origen
###########
origen3 <- lm_robust(zofamilia2~Tpob+Tdes+igualpov+mayorpov+
                        Tpob:igualpov+Tdes:igualpov+
                        Tpob:mayorpov+Tdes:mayorpov+
                        factor(sexoW)+edadW+factor(estlabB)+factor(edcepWB)+
                        factor(ingresosQ)+factor(comuna_w1C)+
                        get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1,data=wide12)

summary(origen3)

# Menor
summary(margins(origen3,variables="Tpob", at=list(igualpov=0,mayorpov=0)))
summary(margins(origen3,variables="Tdes", at=list(igualpov=0,mayorpov=0)))

# Igual
summary(margins(origen3,variables="Tpob", at=list(igualpov=1,mayorpov=0)))
summary(margins(origen3,variables="Tdes", at=list(igualpov=1,mayorpov=0)))

# Mayor
summary(margins(origen3,variables="Tpob", at=list(igualpov=0,mayorpov=1)))
summary(margins(origen3,variables="Tdes", at=list(igualpov=0,mayorpov=1)))


# Trabajo duro
#################
work3 <- lm_robust(zhwork2~Tpob+Tdes+igualpov+mayorpov+
                       Tpob:igualpov+Tdes:igualpov+
                       Tpob:mayorpov+Tdes:mayorpov+
                       factor(sexoW)+edadW+factor(estlabB)+factor(edcepWB)+
                       factor(ingresosQ)+factor(comuna_w1C)+
                       get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1,data=wide12)

summary(work3)

# Menor
summary(margins(work3,variables="Tpob", at=list(igualpov=0,mayorpov=0)))
summary(margins(work3,variables="Tdes", at=list(igualpov=0,mayorpov=0)))

# Igual
summary(margins(work3,variables="Tpob", at=list(igualpov=1,mayorpov=0)))
summary(margins(work3,variables="Tdes", at=list(igualpov=1,mayorpov=0)))

# Mayor
summary(margins(work3,variables="Tpob", at=list(igualpov=0,mayorpov=1)))
summary(margins(work3,variables="Tdes", at=list(igualpov=0,mayorpov=1)))


# Indice educacion
#####################
educ3 <- lm_robust(zdvE2~Tpob+Tdes+igualpov+mayorpov+
                     Tpob:igualpov+Tdes:igualpov+
                     Tpob:mayorpov+Tdes:mayorpov+
                     factor(sexoW)+edadW+factor(estlabB)+factor(edcepWB)+
                     factor(ingresosQ)+factor(comuna_w1C)+
                     get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1,data=wide12)

summary(educ3)

# Menor
summary(margins(educ3,variables="Tpob", at=list(igualpov=0,mayorpov=0)))
summary(margins(educ3,variables="Tdes", at=list(igualpov=0,mayorpov=0)))

# Igual
summary(margins(educ3,variables="Tpob", at=list(igualpov=1,mayorpov=0)))
summary(margins(educ3,variables="Tdes", at=list(igualpov=1,mayorpov=0)))

# Mayor
summary(margins(educ3,variables="Tpob", at=list(igualpov=0,mayorpov=1)))
summary(margins(educ3,variables="Tdes", at=list(igualpov=0,mayorpov=1)))


# -------- Estimaciones OLS, moderador es ingreso -------- #
# Bases separadas
wide12pob <- dplyr::filter(wide12,Tdes==0) 
wide12pob <- dplyr::select(wide12pob,zofamilia2,zhwork2,zdvE2,Tpob,ingresos_w1)
wide12pob <- na.omit(wide12pob)
summary(wide12pob)

wide12des <- dplyr::filter(wide12,Tpob==0) 
wide12des <- dplyr::select(wide12des,zofamilia2,zhwork2,zdvE2,Tdes,ingresos_w1)
wide12des <- na.omit(wide12des)
summary(wide12des)

# Origen
###########
#library(interplot)
library(interflex)

# 1: raw, LID plot
inter.raw(Y = "zofamilia2", D = "Tpob", X = "ingresos_w1", data = wide12pob, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

inter.raw(Y = "zofamilia2", D = "Tdes", X = "ingresos_w1", data = wide12des, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

# 2: binning
out <- interflex(Y = "zofamilia2", D = "Tpob", X = "ingresos_w1", data = wide12pob, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

out <- interflex(Y = "zofamilia2", D = "Tdes", X = "ingresos_w1", data = wide12des, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests


# Trabajo duro
#################
# 1: raw, LID plot
inter.raw(Y = "zhwork2", D = "Tpob", X = "irt_z1", data = wide12pob, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

inter.raw(Y = "zhwork2", D = "Tdes", X = "irt_z1", data = wide12des, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

# 2: binning
out <- interflex(Y = "zhwork2", D = "Tpob", X = "irt_z1", data = wide12pob, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

out <- interflex(Y = "zhwork2", D = "Tdes", X = "irt_z1", data = wide12des, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

# Indice educacion
#####################
# 1: raw, LID plot
inter.raw(Y = "zdvE2", D = "Tpob", X = "irt_z1", data = wide12pob, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

inter.raw(Y = "zdvE2", D = "Tdes", X = "irt_z1", data = wide12des, 
          Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")

# 2: binning
out <- interflex(Y = "zdvE2", D = "Tpob", X = "irt_z1", data = wide12pob, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests

out <- interflex(Y = "zdvE2", D = "Tdes", X = "irt_z1", data = wide12des, 
                 estimator = 'binning',vartype = "robust", main = "Marginal Effects")

out$graph
out$tests




############################################################
# Origen
###########
origen <- lm_robust(zofamilia2~Tpob+Tdes+q2+q3+q4+q5+
                               Tpob:q2+Tpob:q3+Tpob:q4+Tpob:q5+
                               Tdes:q2+Tdes:q3+Tdes:q4+Tdes:q5+
                      factor(sexoW)+factor(edcepWB)+edadW+factor(estlabB)+
                      factor(comuna_w1C)+
                      get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                      data=wide12)

summary(origen)

origen <- lm_robust(zofamilia2~Tpob+Tdes+ingresos_w1+
                      Tpob:ingresos_w1+
                      Tdes:ingresos_w1+
                      factor(sexoW)+factor(edcepWB)+edadW+factor(estlabB)+
                      factor(comuna_w1C)+
                      get_ah_1_w1+get_ah_2_w1+factor(ideo)+irt_z1+igualpov+mayorpov,
                      data=wide12)


summary(margins(origen,variables="Tpob", at=list(ingresos_w1=fivenum(wide12$ingresos_w1))))
summary(margins(origen,variables="Tdes", at=list(irt_z1b=range(wide12$irt_z1b))))

# q1
summary(margins(origen,variables="Tpob", at=list(q2=0,q3=0,q4=0,q5=0)))
summary(margins(origen,variables="Tdes", at=list(q2=0,q3=0,q4=0,q5=0)))















