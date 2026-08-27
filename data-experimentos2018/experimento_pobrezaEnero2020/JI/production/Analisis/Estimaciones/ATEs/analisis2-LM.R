###################################
# Fecha: Julio 2020               #
# Topico: ATEs                    #
###################################

library(texreg)
library(xtable)
library(estimatr)
library(car)

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

# -----------------------ATEs------------------- #
# Origen
###########
m1a <- lm_robust(zofamilia2~factor(tratamiento),data=wide12)

m1b <- lm_robust(zofamilia2~factor(tratamiento)+
                factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                ingresosL+factor(comuna_w1C)+
                get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                data=wide12)

screenreg(l=list(m1a,m1b), single.row = TRUE, stars = c(0.01, 0.05))

# Trabajo duro
######################
m2a <- lm_robust(zhwork2~factor(tratamiento),data=wide12)

m2b <- lm_robust(zhwork2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12)

screenreg(l=list(m2a,m2b), single.row = TRUE, stars = c(0.01, 0.05))

# Indice educacion
#####################
m3a <- lm_robust(zdvE2~factor(tratamiento),data=wide12)

m3b <- lm_robust(zdvE2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12)

screenreg(l=list(m3a,m3b), single.row = TRUE, stars = c(0.01, 0.05))


# Correcion comparaciones multiples
#####################################
summary(m1b)
summary(m2b)
summary(m3b)

tidy(m1b)
pOri <- m1b$p.value[2:3]
pOri

m2 <- tidy(m2b)
pWork <- m2b$p.value[2:3]
pWork

m3 <- tidy(m3b)
pIndex <- m3b$p.value[2:3]
pIndex

p <- c(0.007343939,0.004611688,0.48671472,0.01635493,0.1752247,0.9686856)
alpha <- 0.05

# Sin correccion
sig <- p < alpha 
sig

# Con correccion
bonferroni_sig <- p.adjust(p, "bonferroni") < alpha 
BH_sig <- p.adjust(p, "BH") <alpha # Benjamini-Hochberg. USAR.
holm_sig <- p.adjust(p, "holm") < alpha 
bonferroni_sig
BH_sig 
holm_sig


# ----------------- multicolinealidad, sin ponderador ------------------ #

# Origen
###########
m1b <- lm(zofamilia2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12)

vif(m1b)

# Trabajo duro
######################
m2b <- lm(zhwork2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12)

vif(m2b)

# Indice educacion
#####################
m3b <- lm(zdvE2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12)

vif(m3b)


# -----------------------ATEs con ponderador de ingreso------------------- #
# Origen
###########
m1a <- lm_robust(zofamilia2~factor(tratamiento),data=wide12,weights=weightI)

m1b <- lm_robust(zofamilia2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12,weights=weightI)

screenreg(l=list(m1a,m1b), single.row = TRUE, stars = c(0.01, 0.05))


# Trabajo duro
######################
m2a <- lm_robust(zhwork2~factor(tratamiento),data=wide12,weights=weightI)

m2b <- lm_robust(zhwork2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12,weights=weightI)

screenreg(l=list(m2a,m2b), single.row = TRUE, stars = c(0.01, 0.05))


# Indice educacion
#####################
m3a <- lm_robust(zdvE2~factor(tratamiento),data=wide12,weights=weightI)

m3b <- lm_robust(zdvE2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12,weights=weightI)

screenreg(l=list(m3a,m3b), single.row = TRUE, stars = c(0.01, 0.05))


# Correcion comparaciones multiples
#####################################
summary(m1b)
summary(m2b)
summary(m3b)

tidy(m1b)
pOri <- m1b$p.value[2:3]
pOri

m2 <- tidy(m2b)
pWork <- m2b$p.value[2:3]
pWork

m3 <- tidy(m3b)
pIndex <- m3b$p.value[2:3]
pIndex

p <- c(0.0003754056,0.0007466879,0.40623490,0.02214402,0.1018418,0.6413737)
alpha <- 0.05

# Sin correccion
sig <- p < alpha 
sig

# Con correccion
bonferroni_sig <- p.adjust(p, "bonferroni") < alpha 
BH_sig <- p.adjust(p, "BH") <alpha # Benjamini-Hochberg. USAR.
holm_sig <- p.adjust(p, "holm") < alpha 
bonferroni_sig
BH_sig 
holm_sig
















