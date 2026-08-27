#################
# Clase 6, 2020 #
#################

load("/Users/luismaldonado/Dropbox/Sol3026_3063/2020/Clases/Clase 6/Ejemplo/Chaiten.RData")

# Variable dependiente/resultado
# ZinG: Confianza en grupo de cercanos.

# Tratamiento
# Exposicion: Tratamiento, 1 es expuesto a desastre, 0 es no expuesto

# Covariables
# tch: Tiempo de residencia en Chaiten en anios.
# Terciaria: 1 con educación terciaria y 0 sin educacion terciaria

# ---------------- OLS ----------------- #
m1 <- lm(ZinG~Exposicion,data=Chaiten) # Modelo sin covariables
m2 <- lm(ZinG~Exposicion + tch + Terciaria,data=Chaiten) # Modelo con covariables

library(texreg)
screenreg(l=list(m1,m2), single.row = TRUE, stars = c(0.01, 0.05))	

# ---------------- Estimacion IPWs ----------------- #

# Actividades
	# Propensity score
	# IPW
	# IPW truncado
	# SIPW
	# SIPW truncado

# Propensity score
fit <- glm(Exposicion~tch + Terciaria,family=binomial(),data=Chaiten)
Chaiten$ps <- predict(fit,type="response")

# IPW
p.fit <-ifelse(Chaiten$Exposicion == 0,
               1 - predict(fit, type = "response"),
               predict(fit, type = "response"))

Chaiten$w <- 1 / p.fit

# IPW truncado
quantile(Chaiten$w, c(.01,.99)) 

Chaiten$w_trunc <- Chaiten$w
Chaiten$w_trunc[Chaiten$w_trunc > 28.101261]<- 28.101261   
Chaiten$w_trunc[Chaiten$w_trunc < 1.000494]<- 1.000494 

# SIPW
fit <- glm(Exposicion~tch + Terciaria,family=binomial(),data=Chaiten)
denom.p <- predict(fit,type="response")

numer.fit <- glm(Exposicion~1, family = binomial(link="logit"), data = Chaiten)
numer.p <- predict(numer.fit, type = "response")

Chaiten$sw <- ifelse(Chaiten$Exposicion == 0, ((1-numer.p)/(1-denom.p)),
                     (numer.p/denom.p))

# SIPW truncado
quantile(Chaiten$sw, c(.01,.99)) 

Chaiten$sw_trunc <- Chaiten$sw
Chaiten$sw_trunc[Chaiten$sw_trunc > 7.281298]<- 7.281298    
Chaiten$sw_trunc[Chaiten$sw_trunc < 0.335337]<- 0.335337

# ------------------ Evaluacion de ponderadores ----------------- #
# Medias y SD
#################
summary(Chaiten$w)
summary(Chaiten$w_trunc)
summary(Chaiten$sw)
summary(Chaiten$sw_trunc)

sd(Chaiten$w)
sd(Chaiten$w_trunc)
sd(Chaiten$sw)
sd(Chaiten$sw_trunc)


# Balance
##############
library(Matching)
library(ebal)

# Pre-ponderadores: regresiones
summary(lm(tch~Exposicion,data=Chaiten))
summary(lm(Terciaria~Exposicion,data=Chaiten))
summary(lm(ps~Exposicion,data=Chaiten))

# Pre-ponderadores: Diferencias estandarizadas
bal1 <- MatchBalance(Exposicion~. - (ZinG+ps+w+w_trunc+sw+sw_trunc),
                     data=Chaiten,match.out = NULL,ks=TRUE)
bal1.label  <-c("Tiempo en Chaiten","Terciaria")
bal1.m1  <- baltest.collect(matchbal.out=bal1,var.names=bal1.label,after=FALSE)
round(bal1.m1,2)

# IPW
bal.ipw <- MatchBalance(Exposicion~. - (ZinG+ps+w+w_trunc+sw+sw_trunc),
                        data=Chaiten,weights=Chaiten$w,ks=TRUE)
bal.ipw1  <- baltest.collect(matchbal.out=bal.ipw,var.names=bal1.label,after=FALSE)
round(bal.ipw1,2)

# IPW truncado
bal.ipwt <- MatchBalance(Exposicion~. - (ZinG+ps+w+w_trunc+sw+sw_trunc),
                        data=Chaiten,weights=Chaiten$w_trunc,ks=TRUE)
bal.ipwt1  <- baltest.collect(matchbal.out=bal.ipwt,var.names=bal1.label,after=FALSE)
round(bal.ipwt1,2)

# SIPW 
bal.sipw <- MatchBalance(Exposicion~. - (ZinG+ps+w+w_trunc+sw+sw_trunc),
                        data=Chaiten,weights=Chaiten$sw,ks=TRUE)
bal.sipw1  <- baltest.collect(matchbal.out=bal.sipw,var.names=bal1.label,after=FALSE)
round(bal.sipw1,2)

# SIPW truncado 
bal.sipwt <- MatchBalance(Exposicion~. - (ZinG+ps+w+w_trunc+sw+sw_trunc),
                        data=Chaiten,weights=Chaiten$sw_trunc,ks=TRUE)
bal.sipwt1  <- baltest.collect(matchbal.out=bal.sipwt,var.names=bal1.label,after=FALSE)
round(bal.sipwt1,2)

# SIPW truncado: regresiones
summary(lm(tch~Exposicion,data=Chaiten,weights=sw_trunc))
summary(lm(Terciaria~Exposicion,data=Chaiten,weights=sw_trunc))
summary(lm(ps~Exposicion,data=Chaiten,weights=sw_trunc))

# ----------------------- ATE -------------------- #
library(estimatr)

# ipw
##############
# Diferencias de medias 1
ht.est <- function(y,d,w){
  n <- length(y)
  (1/n)*sum((y*d*w) - (y*(1-d)*w))
}

ht.est(Chaiten$ZinG,Chaiten$Exposicion,Chaiten$w)

# Diferencias de medias 2
with(Chaiten,
(sum(ZinG[Exposicion==1]*w[Exposicion==1]) - 
 sum(ZinG[Exposicion==0]*w[Exposicion==0]))/length(ZinG)
)

# Regresion
summary(lm_robust(ZinG~Exposicion,data=Chaiten,se_type="HC2",weights=w))

# Explicacion diferencia: normalizacion (Hajek estimator)
with(Chaiten,
sum(ZinG[Exposicion==1]*w[Exposicion==1])/sum(w[Exposicion==1]) -
sum(ZinG[Exposicion==0]*w[Exposicion==0])/sum(w[Exposicion==0])
)

# sipw
##############
summary(lm_robust(ZinG~Exposicion,data=Chaiten,se_type="HC2",weights=sw))

with(Chaiten,
sum(ZinG[Exposicion==1]*sw[Exposicion==1])/sum(sw[Exposicion==1]) -
sum(ZinG[Exposicion==0]*sw[Exposicion==0])/sum(sw[Exposicion==0])
)

# Ponderadores truncados
##############################
# ipw truncados
summary(lm_robust(ZinG~Exposicion,data=Chaiten,se_type="HC2",weights=w_trunc))
# sipw truncados
summary(lm_robust(ZinG~Exposicion,data=Chaiten,se_type="HC2",weights=sw_trunc))

# Ponderadores truncados con controles
#########################################
# ipw truncados
summary(lm_robust(ZinG~Exposicion +
	                   tch+Terciaria,
					   data=Chaiten,se_type="HC2",weights=w_trunc))
# sipw truncados
summary(lm_robust(ZinG~Exposicion +
	                   tch+Terciaria,
					   data=Chaiten,se_type="HC2",weights=sw_trunc))
					   

# ----------------- Extrapolacion ---------------- #
# Graficos
##############
library(ggplot2)
library(gridExtra) # Varias graficos en 1 imagen

# Box plots
# tch: Pre-ajuste
g1 <- ggplot(Chaiten,aes(x=factor(Exposicion),y=tch)) + geom_boxplot() + ggtitle("Pre") 

# tch: Post-ajuste
g2 <- ggplot(Chaiten,aes(x=factor(Exposicion),y=tch,weight=sw_trunc)) + geom_boxplot() + ggtitle("Post") 

# Imagen con graficos pre y post
grid.arrange(g1,g2,ncol=2)


# ps: Pre-ajuste
g5 <- ggplot(Chaiten,aes(x=factor(Exposicion),y=ps)) + geom_boxplot() + ggtitle("Pre")

# ps: Post-ajuste
g6 <- ggplot(Chaiten,aes(x=factor(Exposicion),y=ps,weight=sw_trunc)) + geom_boxplot() + ggtitle("Post")

# Imagen con graficos pre y post
grid.arrange(g5,g6,ncol=2)





 