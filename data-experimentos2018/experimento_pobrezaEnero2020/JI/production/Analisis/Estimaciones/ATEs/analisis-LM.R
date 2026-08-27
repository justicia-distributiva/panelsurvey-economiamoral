###################################
# Fecha: Febrero 2020             #
# Topico: ATEs                    #
###################################

library(lmtest)
library(sandwich)
library(texreg)
library(xtable)
library(estimatr)

# Estandarizacion 1
wide12$zofamilia <- scale(wide12$get_ah_1_w2a)
wide12$zhwork <- scale(wide12$get_ah_5_w2a)
wide12$zedstr <- scale(wide12$get_ah_estr_w2a)
wide12$zed <- scale(wide12$get_ah_3_w2a)
wide12$zednew <- scale(wide12$get_ah_indv_w2a)

# Estandarizacion 2
glass_delta <- function(Y, Z, name = "control"){
  Y/sd(Y[Z == name], na.rm = TRUE)
}

wide12$zofamilia2 <- glass_delta(Y=wide12$get_ah_1_w2a,wide12$tratamiento)                
wide12$zhwork2 <- glass_delta(Y=wide12$get_ah_5_w2a,wide12$tratamiento)
wide12$zedstr2 <- glass_delta(Y=wide12$get_ah_estr_w2a,wide12$tratamiento)
wide12$zed2 <- glass_delta(Y=wide12$get_ah_3_w2a,wide12$tratamiento)
wide12$zednew2 <- glass_delta(Y=wide12$get_ah_indv_w2a,wide12$tratamiento)
wide12$zdvE2 <- glass_delta(Y=wide12$dvEa,wide12$tratamiento)

# Correlation for weights and outcomes
corr <- wide12 %>% dplyr::select(zofamilia,zhwork,zedstr,zed,zednew,SdvE,weight)
corr <- na.omit(corr)
cor(corr)

# -----------------------ATEs------------------- #
# Origen
###########

# OLS: 
m1a <- lm(zofamilia~factor(tratamiento),data=wide12)
m1a1 = coeftest(m1a, vcov.= vcovHC(m1a, "HC2"))

m1b <- lm(zofamilia~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m1b1 = coeftest(m1b, vcov.= vcovHC(m1b, "HC2"))

m1c <- lm(zofamilia~factor(tratamiento),data=wide12,weights=weight)
m1c1 = coeftest(m1c, vcov.= vcovHC(m1c, "HC2"))

m1d <- lm(zofamilia~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m1d1 = coeftest(m1d, vcov.= vcovHC(m1d, "HC2"))

screenreg(l=list(m1a,m1b,m1c,m1d), single.row = TRUE, stars = c(0.01, 0.05))
screenreg(l=list(m1a1,m1b1,m1c1,m1d1), single.row = TRUE, stars = c(0.01, 0.05))

# OLS, Glass estandarization
m1a <- lm(zofamilia2~factor(tratamiento),data=wide12)
m1a1 = coeftest(m1a, vcov.= vcovHC(m1a, "HC2"))

m1b <- lm(zofamilia2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo)+igual,
          data=wide12)
m1b1 = coeftest(m1b, vcov.= vcovHC(m1b, "HC2"))

m1c <- lm(zofamilia2~factor(tratamiento),data=wide12,weights=weight)
m1c1 = coeftest(m1c, vcov.= vcovHC(m1c, "HC2"))

m1d <- lm(zofamilia2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo)+igual,
          data=wide12,weights=weight)
m1d1 = coeftest(m1d, vcov.= vcovHC(m1d, "HC2"))

screenreg(l=list(m1a1,m1b1,m1c1,m1d1), single.row = TRUE, stars = c(0.01, 0.05))

# Trabajo duro
######################
m2a <- lm(zhwork~factor(tratamiento),data=wide12)
m2a1 = coeftest(m2a, vcov.= vcovHC(m2a, "HC2"))

m2b <- lm(zhwork~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m2b1 = coeftest(m2b, vcov.= vcovHC(m2b, "HC2"))

m2c <- lm(zhwork~factor(tratamiento),data=wide12,weights=weight)
m2c1 = coeftest(m2c, vcov.= vcovHC(m2c, "HC2"))

m2d <- lm(zhwork~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m2d1 = coeftest(m2d, vcov.= vcovHC(m2d, "HC2"))

screenreg(l=list(m2a1,m2b1,m2c1,m2d1), single.row = TRUE, stars = c(0.01, 0.05))

# Glass estandarization
m2a <- lm(zhwork2~factor(tratamiento),data=wide12)
m2a1 = coeftest(m2a, vcov.= vcovHC(m2a, "HC2"))

m2b <- lm(zhwork2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m2b1 = coeftest(m2b, vcov.= vcovHC(m2b, "HC2"))

m2c <- lm(zhwork2~factor(tratamiento),data=wide12,weights=weight)
m2c1 = coeftest(m2c, vcov.= vcovHC(m2c, "HC2"))

m2d <- lm(zhwork2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m2d1 = coeftest(m2d, vcov.= vcovHC(m2d, "HC2"))

screenreg(l=list(m2a1,m2b1,m2c1,m2d1), single.row = TRUE, stars = c(0.01, 0.05))


# Haber accedido a una educación de calidad 
################################################
m3a <- lm(zedstr~factor(tratamiento),data=wide12)
m3a1 = coeftest(m3a, vcov.= vcovHC(m3a, "HC2"))

m3b <- lm(zedstr~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m3b1 = coeftest(m3b, vcov.= vcovHC(m3b, "HC2"))

m3c <- lm(zedstr~factor(tratamiento),data=wide12,weights=weight)
m3c1 = coeftest(m3c, vcov.= vcovHC(m3c, "HC2"))

m3d <- lm(zedstr~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m3d1 = coeftest(m3d, vcov.= vcovHC(m3d, "HC2"))

screenreg(l=list(m3a1,m3b1,m3c1,m3d1), single.row = TRUE, stars = c(0.01, 0.05))

# Glass estandarization
m3a <- lm(zedstr2~factor(tratamiento),data=wide12)
m3a1 = coeftest(m3a, vcov.= vcovHC(m3a, "HC2"))

m3b <- lm(zedstr2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m3b1 = coeftest(m3b, vcov.= vcovHC(m3b, "HC2"))

m3c <- lm(zedstr2~factor(tratamiento),data=wide12,weights=weight)
m3c1 = coeftest(m3c, vcov.= vcovHC(m3c, "HC2"))

m3d <- lm(zedstr2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m3d1 = coeftest(m3d, vcov.= vcovHC(m3d, "HC2"))

screenreg(l=list(m3a1,m3b1,m3c1,m3d1), single.row = TRUE, stars = c(0.01, 0.05))

# Haber alcanzado un buen nivel de educación 
##############################################
m4a <- lm(zed~factor(tratamiento),data=w02a)

m4a <- lm(zed~factor(tratamiento),data=wide12)
m4a1 = coeftest(m4a, vcov.= vcovHC(m4a, "HC2"))

m4b <- lm(zed~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m4b1 = coeftest(m4b, vcov.= vcovHC(m4b, "HC2"))

m4c <- lm(zed~factor(tratamiento),data=wide12,weights=weight)
m4c1 = coeftest(m4c, vcov.= vcovHC(m4c, "HC2"))

m4d <- lm(zed~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m4d1 = coeftest(m4d, vcov.= vcovHC(m4d, "HC2"))

screenreg(l=list(m4a1,m4b1,m4c1,m4d1), single.row = TRUE, stars = c(0.01, 0.05))

# Glass estandarization
m4a <- lm(zed2~factor(tratamiento),data=w02a)

m4a <- lm(zed2~factor(tratamiento),data=wide12)
m4a1 = coeftest(m4a, vcov.= vcovHC(m4a, "HC2"))

m4b <- lm(zed2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m4b1 = coeftest(m4b, vcov.= vcovHC(m4b, "HC2"))

m4c <- lm(zed2~factor(tratamiento),data=wide12,weights=weight)
m4c1 = coeftest(m4c, vcov.= vcovHC(m4c, "HC2"))

m4d <- lm(zed2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m4d1 = coeftest(m4d, vcov.= vcovHC(m4d, "HC2"))

screenreg(l=list(m4a1,m4b1,m4c1,m4d1), single.row = TRUE, stars = c(0.01, 0.05))

# Que usted tenga un buen nivel de educación 
################################################
m5a <- lm(zednew~factor(tratamiento),data=w02a)

m5a <- lm(zednew~factor(tratamiento),data=wide12)
m5a1 = coeftest(m5a, vcov.= vcovHC(m5a, "HC2"))

m5b <- lm(zednew~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m5b1 = coeftest(m5b, vcov.= vcovHC(m5b, "HC2"))

m5c <- lm(zednew~factor(tratamiento),data=wide12,weights=weight)
m5c1 = coeftest(m5c, vcov.= vcovHC(m5c, "HC2"))

m5d <- lm(zednew~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m5d1 = coeftest(m5d, vcov.= vcovHC(m5d, "HC2"))

screenreg(l=list(m5a1,m5b1,m5c1,m5d1), single.row = TRUE, stars = c(0.01, 0.05))

# Glass estandarization
m5a <- lm(zednew2~factor(tratamiento),data=w02a)

m5a <- lm(zednew2~factor(tratamiento),data=wide12)
m5a1 = coeftest(m5a, vcov.= vcovHC(m5a, "HC2"))

m5b <- lm(zednew2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m5b1 = coeftest(m5b, vcov.= vcovHC(m5b, "HC2"))

m5c <- lm(zednew2~factor(tratamiento),data=wide12,weights=weight)
m5c1 = coeftest(m5c, vcov.= vcovHC(m5c, "HC2"))

m5d <- lm(zednew2~factor(tratamiento)+
            factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
            get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m5d1 = coeftest(m5d, vcov.= vcovHC(m5d, "HC2"))

screenreg(l=list(m5a1,m5b1,m5c1,m5d1), single.row = TRUE, stars = c(0.01, 0.05))

# Indice educacion
#####################
m6a <- lm(zdvE2~factor(tratamiento),data=wide12)
m6a1 = coeftest(m6a, vcov.= vcovHC(m6a, "HC2"))

m6b <- lm(zdvE2~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12)
m6b1 = coeftest(m6b, vcov.= vcovHC(m6b, "HC2"))

m6c <- lm(zdvE2~factor(tratamiento),data=wide12,weights=weight)
m6c1 = coeftest(m6c, vcov.= vcovHC(m6c, "HC2"))

m6d <- lm(zdvE2~factor(tratamiento)+
          factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
          get_ah_1_w1+get_ah_2_w1+factor(ideo),
          data=wide12,weights=weight)
m6d1 = coeftest(m6d, vcov.= vcovHC(m6d, "HC2"))

screenreg(l=list(m6a1,m6b1,m6c1,m6d1), single.row = TRUE, stars = c(0.01, 0.05))


# ----------------- Comparaciones multiples ----------------- #

# Origen
###########
m1 <- lm_robust(zofamilia2~factor(tratamiento)+
                factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
                get_ah_1_w1+get_ah_2_w1+factor(ideo),data=wide12)

summary(m1)

# Trabajo duro
######################
m2 <- lm_robust(zhwork2~factor(tratamiento)+
                  factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
                  get_ah_1_w1+get_ah_2_w1+factor(ideo),data=wide12)

summary(m2)

# Indice educacion
#####################
m3 <- lm_robust(zdvE2~factor(tratamiento)+
                  factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+factor(ingresosQ)+factor(comuna_w1C)+
                  get_ah_1_w1+get_ah_2_w1+factor(ideo),data=wide12)

summary(m3)

# Correcion comparaciones multiples
#####################################
tidy(m1)
pOri <- m1$p.value[2:3]
pOri

m2 <- tidy(m2)
pWork <- m2$p.value[2:3]
pWork

m3 <- tidy(m3)
pIndex <- m3$p.value[2:3]
pIndex

p <- c(0.008801584,0.004815380,0.54767770,0.01713801,0.1416409,0.9830031)
alpha <- 0.05

# Sin correccion
sig <- p < alpha 
sig

# Con correccion
bonferroni_sig <- p.adjust(p, "bonferroni") < alpha 
BH_sig <- p.adjust(p, "BH") <alpha # Benjamini-Hochberg
holm_sig <- p.adjust(p, "holm") < alpha 
bonferroni_sig
BH_sig 
holm_sig


