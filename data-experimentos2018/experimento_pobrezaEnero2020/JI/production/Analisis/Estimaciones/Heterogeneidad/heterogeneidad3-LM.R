###################################
# Fecha: Septiembre 2020          #
# Topico: Heterogeneidad, trees   #
###################################

library(dbarts)
library(dplyr)
library(foreign)
library(tidyverse)
library(ggpubr)
library(margins)
library(bartCause)
library(ggplot2)

library(grf) # Causal forest

# ---------- Preparacion de datos -------------- #

# Variables
# zofamilia2: numerica
# zhwork2: numerica
# zdvE2: numerica
# Tpob: dummy
# Tdes: dummy
# sexoWb: dummy
# edadW: continua
# media: dummy
# suptec: dummy
# supun: dummy
# estlabB: dummy
# ingresosQ: continua
# comuna1s: dummy
# comuna1na: dummy
# get_ah_1_w1a: continua
# get_ah_2_w1a: continua
# ideoC: dummy
# ideoD: dummy
# ideoNa: dummy
# irt_z1: continua
# igualpov: dummy
# mayorpov: dummy

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

# Sexo
table(wide12$sexoWb,exclude=NULL) # 1 es mujer

# Edad
table(wide12$edadW,exclude=NULL)

# Estatus laboral 
table(wide12$estlabB,exclude=NULL) # 1 trabaja

# Ingresos
table(wide12$ingresosL,exclude=NULL)

# Comunas
table(wide12$comuna_w1C,exclude=NULL)

wide12$comuna1s <- 0
wide12$comuna1s[wide12$comuna_w1C==1]=1 # Santiago
table(wide12$comuna1s,wide12$comuna_w1C,exclude=NULL)

wide12$comuna1na <- 0
wide12$comuna1na[wide12$comuna_w1C==2]=1 # Santiago
table(wide12$comuna1na,wide12$comuna_w1C,exclude=NULL)

# Get ahead
table(wide12$get_ah_1_w1,exclude=NULL)
table(wide12$get_ah_2_w1,exclude=NULL)

# Igualitarismo
summary(wide12$irt_z1)

# Ideologia politica
table(wide12$ideo)

wide12$ideoC <- 0
wide12$ideoC[wide12$ideo==1]=1 # Centro
table(wide12$ideoC,wide12$ideo,exclude=NULL)  

wide12$ideoD <- 0
wide12$ideoD[wide12$ideo==2]=1 # Derecha
table(wide12$ideoD,wide12$ideo,exclude=NULL)  

wide12$ideoNa <- 0
wide12$ideoNa[wide12$ideo==3]=1 # Derecha
table(wide12$ideoNa,wide12$ideo,exclude=NULL)  


# Full dataset: para usar sin as.factor
wide12b <- dplyr::select(wide12,zofamilia2,zhwork2,zdvE2,tratamiento,
                         sexoWb,edadW,media,suptec,supun,estlabB,
                         comuna1s,comuna1na,get_ah_1_w1a,get_ah_2_w1a,
                         ideoC,ideoD,ideoNa,irt_z1,igualpov,mayorpov,ingresosL)


# -------------------- Dataset ------------------- #

# Pobreza: wide12bP 
summary(wide12b)
wide12bP <- filter(wide12b,tratamiento!="desigual") %>%  na.omit() 
dim(wide12bP)

wide12bP$tratamientoP <- 0
wide12bP$tratamientoP[wide12bP$tratamiento=="pobreza"]=1
table(wide12bP$tratamientoP,wide12bP$tratamiento,exclude=NULL)
wide12bP$tratamiento <- NULL
names(wide12bP)       
summary(wide12bP)

# Desigualdad: wide12bD 
summary(wide12b)
wide12bD <- filter(wide12b,tratamiento!="pobreza") %>%  na.omit() 
dim(wide12bD)

wide12bD$tratamientoD <- 0
wide12bD$tratamientoD[wide12bD$tratamiento=="desigual"]=1
table(wide12bD$tratamientoD,wide12bD$tratamiento,exclude=NULL)
wide12bD$tratamiento <- NULL
names(wide12bD)       
summary(wide12bD)

# ---------------- Overall heterogeneity for poverty treatment -------------- #
names(wide12bP)

W = wide12bP$tratamientoP # Treatment
Yor = wide12bP$zofamilia2 # Outcome, social origin

X = wide12bP[,-(1:3)] # Delete outcomes
names(X)
X = X[,-(18:18)] # Elimina tratamientoP
names(X)
summary(X)

# Social Origin
#######################
Y.forest = regression_forest(X, Yor)
Y.hat = predict(Y.forest)$predictions
W.forest = regression_forest(X, W)
W.hat = predict(W.forest)$predictions

cf.raw = causal_forest(X, Yor, W,
                       Y.hat = Y.hat, W.hat = W.hat)

varimp = variable_importance(cf.raw) # Variables mas explicativas de heterogeneidad
varimp
names(X)

selected.idx = which(varimp > mean(varimp))

cf = causal_forest(X[,selected.idx], Yor, W,
                   Y.hat = Y.hat, W.hat = W.hat,
                   tune.parameters = "all")

wide12bP$tau.hatPo = predict(cf)$predictions

boxplot(wide12bP$tau.hatPo)
hist(wide12bP$tau.hatPo)
mean(wide12bP$tau.hatPo)

# ATE
ATE = average_treatment_effect(cf)
paste("95% CI for the ATE:", round(ATE[1], 3),"+/-", round(qnorm(0.975) * ATE[2], 3))

# Omnibus tests for heterogeneity
test_calibration(cf)

# Graph of overall heterogeneity
tau_hat_forest <- predict(cf, estimate.variance = TRUE)
sigma_hat <- sqrt(tau_hat_forest$variance.estimates)

grf_df <- 
  cbind(data.frame(
    tau_hat = tau_hat_forest$predictions,
    ci_upper = tau_hat_forest$predictions + 1.96 * sigma_hat,
    ci_lower = tau_hat_forest$predictions - 1.96 * sigma_hat),
    X
  )

grf_df$order <- with(grf_df, rank(tau_hat, ties.method = "first"))


g_basePor <-
  ggplot(data = grf_df, aes(tau_hat, order)) +
  geom_point(alpha=.08) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.08,
                 height = 0) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Poverty: social origin") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 	

g_basePor

# How many CIs include zero? 
grf_df %>% 
  summarise(mean(ci_lower <= 0 & ci_upper >= 0))

# What proportion of the CIs that don't include zero are positive? 
these <- with(grf_df, which(ci_lower <= 0 & ci_upper >= 0))

grf_df[-these,] %>% 
  filter(tau_hat > 0) %>% 
  nrow()/nrow(grf_df)


# Work effort
#######################
names(wide12b)

rm(W,Yor,X,Y.forest,Y.hat)
rm(W.forest,W.hat,cf.raw,varimp,selected.idx,cf,tau.hat)
rm(ATE,tau_hat_forest,sigma_hat,grf_df,g_base)

W = wide12bP$tratamientoP # Treatment
Yw = wide12bP$zhwork2 # Outcome, work effort

X = wide12bP[,-(1:3)] # Delete outcomes
names(X)
X = X[,-(18:18)] # Elimina tratamientoP
names(X)
summary(X)

Y.forest = regression_forest(X, Yw)
Y.hat = predict(Y.forest)$predictions
W.forest = regression_forest(X, W)
W.hat = predict(W.forest)$predictions

cf.raw = causal_forest(X, Yw, W,
                       Y.hat = Y.hat, W.hat = W.hat)

varimp = variable_importance(cf.raw) # Variables mas explicativas de heterogeneidad
varimp
names(X)

selected.idx = which(varimp > mean(varimp))

cf = causal_forest(X[,selected.idx], Yw, W,
                   Y.hat = Y.hat, W.hat = W.hat,
                   tune.parameters = "all")

tau.hat = predict(cf)$predictions

boxplot(tau.hat)
hist(tau.hat)
mean(tau.hat)

# ATE
ATE = average_treatment_effect(cf)
paste("95% CI for the ATE:", round(ATE[1], 3),"+/-", round(qnorm(0.975) * ATE[2], 3))

# Omnibus tests for heterogeneity
test_calibration(cf)

# Graph of overall heterogeneity
tau_hat_forest <- predict(cf, estimate.variance = TRUE)
sigma_hat <- sqrt(tau_hat_forest$variance.estimates)

grf_df <- 
  cbind(data.frame(
    tau_hat = tau_hat_forest$predictions,
    ci_upper = tau_hat_forest$predictions + 1.96 * sigma_hat,
    ci_lower = tau_hat_forest$predictions - 1.96 * sigma_hat),
    X
  )

grf_df$order <- with(grf_df, rank(tau_hat, ties.method = "first"))


g_base <-
  ggplot(data = grf_df, aes(tau_hat, order)) +
  geom_point(alpha=.08) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.08,
                 height = 0) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Estimated Treatment Effect") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 	

g_base

# Index of education
#######################
rm(W,Yw,X,Y.forest,Y.hat)
rm(W.forest,W.hat,cf.raw,varimp,selected.idx,cf,tau.hat)
rm(ATE,tau_hat_forest,sigma_hat,grf_df,g_base)

W = wide12bP$tratamientoP # Treatment
Ye = wide12bP$zdvE2 # Outcome, social origin

X = wide12bP[,-(1:3)] # Delete outcomes
names(X)
X = X[,-(18:18)] # Elimina tratamientoP
names(X)
summary(X)


Y.forest = regression_forest(X, Ye)
Y.hat = predict(Y.forest)$predictions
W.forest = regression_forest(X, W)
W.hat = predict(W.forest)$predictions

cf.raw = causal_forest(X, Ye, W,
                       Y.hat = Y.hat, W.hat = W.hat)

varimp = variable_importance(cf.raw) # Variables mas explicativas de heterogeneidad
varimp
names(X)

selected.idx = which(varimp > mean(varimp))

cf = causal_forest(X[,selected.idx], Ye, W,
                   Y.hat = Y.hat, W.hat = W.hat,
                   tune.parameters = "all")

tau.hat = predict(cf)$predictions

boxplot(tau.hat)
hist(tau.hat)
mean(tau.hat)

# ATE
ATE = average_treatment_effect(cf)
paste("95% CI for the ATE:", round(ATE[1], 3),"+/-", round(qnorm(0.975) * ATE[2], 3))

# Omnibus tests for heterogeneity
test_calibration(cf)

# Graph of overall heterogeneity
tau_hat_forest <- predict(cf, estimate.variance = TRUE)
sigma_hat <- sqrt(tau_hat_forest$variance.estimates)

grf_df <- 
  cbind(data.frame(
    tau_hat = tau_hat_forest$predictions,
    ci_upper = tau_hat_forest$predictions + 1.96 * sigma_hat,
    ci_lower = tau_hat_forest$predictions - 1.96 * sigma_hat),
    X
  )

grf_df$order <- with(grf_df, rank(tau_hat, ties.method = "first"))


g_base <-
  ggplot(data = grf_df, aes(tau_hat, order)) +
  geom_point(alpha=.08) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.08,
                 height = 0) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Estimated Treatment Effect") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 	

g_base

# ---------------- Overall heterogeneity for inequality treatment -------------- #

# Social Origin
#######################
names(wide12bD)

rm(W,Ye,X,Y.forest,Y.hat)
rm(W.forest,W.hat,cf.raw,varimp,selected.idx,cf,tau.hat)
rm(ATE,tau_hat_forest,sigma_hat,grf_df,g_base)

W = wide12bD$tratamientoD # Treatment
Yor = wide12bD$zofamilia2 # Outcome, social origin

X = wide12bD[,-(1:3)] # Delete outcomes
names(X)
X = X[,-(18:18)] # Elimina tratamientoP
names(X)
summary(X)


Y.forest = regression_forest(X, Yor)
Y.hat = predict(Y.forest)$predictions
W.forest = regression_forest(X, W)
W.hat = predict(W.forest)$predictions

cf.raw = causal_forest(X, Yor, W,
                       Y.hat = Y.hat, W.hat = W.hat)

varimp = variable_importance(cf.raw) # Variables mas explicativas de heterogeneidad
varimp
names(X)

selected.idx = which(varimp > mean(varimp))

cf = causal_forest(X[,selected.idx], Yor, W,
                   Y.hat = Y.hat, W.hat = W.hat,
                   tune.parameters = "all")

wide12bD$tau.hatDo = predict(cf)$predictions

boxplot(tau.hat)
hist(tau.hat)
mean(tau.hat)

# ATE
ATE = average_treatment_effect(cf)
paste("95% CI for the ATE:", round(ATE[1], 3),"+/-", round(qnorm(0.975) * ATE[2], 3))

# Omnibus tests for heterogeneity
test_calibration(cf)

# Graph of overall heterogeneity
tau_hat_forest <- predict(cf, estimate.variance = TRUE)
sigma_hat <- sqrt(tau_hat_forest$variance.estimates)

grf_df <- 
  cbind(data.frame(
    tau_hat = tau_hat_forest$predictions,
    ci_upper = tau_hat_forest$predictions + 1.96 * sigma_hat,
    ci_lower = tau_hat_forest$predictions - 1.96 * sigma_hat),
    X
  )

grf_df$order <- with(grf_df, rank(tau_hat, ties.method = "first"))

g_baseDor <-
  ggplot(data = grf_df, aes(tau_hat, order)) +
  geom_point(alpha=.08) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.08,
                 height = 0) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Inequality: social origin") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 	

g_baseDor

# How many CIs include zero? 
grf_df %>% 
  summarise(mean(ci_lower <= 0 & ci_upper >= 0))

# What proportion of the CIs that don't include zero are positive? 
these <- with(grf_df, which(ci_lower <= 0 & ci_upper >= 0))

grf_df[-these,] %>% 
  filter(tau_hat > 0) %>% 
  nrow()/nrow(grf_df)


# Work effort
#######################
names(wide12bD)

rm(W,Yor,X,Y.forest,Y.hat)
rm(W.forest,W.hat,cf.raw,varimp,selected.idx,cf,tau.hat)
rm(ATE,tau_hat_forest,sigma_hat,grf_df,g_base)

W = wide12bD$tratamientoD # Treatment
Yw = wide12bD$zhwork2 # Outcome, work effort

X = wide12bD[,-(1:3)] # Delete outcomes
names(X)
X = X[,-(18:18)] # Elimina tratamientoP
names(X)
summary(X)

Y.forest = regression_forest(X, Yw)
Y.hat = predict(Y.forest)$predictions
W.forest = regression_forest(X, W)
W.hat = predict(W.forest)$predictions

cf.raw = causal_forest(X, Yw, W,
                       Y.hat = Y.hat, W.hat = W.hat)

varimp = variable_importance(cf.raw) # Variables mas explicativas de heterogeneidad
varimp*100
names(X)

selected.idx = which(varimp > mean(varimp))

cf = causal_forest(X[,selected.idx], Yw, W,
                   Y.hat = Y.hat, W.hat = W.hat,
                   tune.parameters = "all")

wide12bD$tau.hatDw = predict(cf)$predictions

boxplot(tau.hat)
hist(tau.hat)
mean(tau.hat)

# ATE
ATE = average_treatment_effect(cf)
paste("95% CI for the ATE:", round(ATE[1], 3),"+/-", round(qnorm(0.975) * ATE[2], 3))

# Omnibus tests for heterogeneity
test_calibration(cf)

# Graph of overall heterogeneity
tau_hat_forest <- predict(cf, estimate.variance = TRUE)
sigma_hat <- sqrt(tau_hat_forest$variance.estimates)

grf_df <- 
  cbind(data.frame(
    tau_hat = tau_hat_forest$predictions,
    ci_upper = tau_hat_forest$predictions + 1.96 * sigma_hat,
    ci_lower = tau_hat_forest$predictions - 1.96 * sigma_hat),
    X
  )

grf_df$order <- with(grf_df, rank(tau_hat, ties.method = "first"))


g_baseDwork <-
  ggplot(data = grf_df, aes(tau_hat, order)) +
  geom_point(alpha=.08) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.08,
                 height = 0) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Inequality: work effort") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 	

g_baseDwork

# Index of education
#######################
rm(W,Yw,X,Y.forest,Y.hat)
rm(W.forest,W.hat,cf.raw,varimp,selected.idx,cf,tau.hat)
rm(ATE,tau_hat_forest,sigma_hat,grf_df,g_base)

W = wide12bD$tratamientoD # Treatment
Ye = wide12bD$zdvE2 # Outcome, social origin

X = wide12bD[,-(1:3)] # Delete outcomes
names(X)
X = X[,-(18:18)] # Elimina tratamientoP
names(X)
summary(X)


Y.forest = regression_forest(X, Ye)
Y.hat = predict(Y.forest)$predictions
W.forest = regression_forest(X, W)
W.hat = predict(W.forest)$predictions

cf.raw = causal_forest(X, Ye, W,
                       Y.hat = Y.hat, W.hat = W.hat)

varimp = variable_importance(cf.raw) # Variables mas explicativas de heterogeneidad
varimp
names(X)

selected.idx = which(varimp > mean(varimp))

cf = causal_forest(X[,selected.idx], Ye, W,
                   Y.hat = Y.hat, W.hat = W.hat,
                   tune.parameters = "all")

tau.hat = predict(cf)$predictions

boxplot(tau.hat)
hist(tau.hat)
mean(tau.hat)

# ATE
ATE = average_treatment_effect(cf)
paste("95% CI for the ATE:", round(ATE[1], 3),"+/-", round(qnorm(0.975) * ATE[2], 3))

# Omnibus tests for heterogeneity
test_calibration(cf)

# Graph of overall heterogeneity
tau_hat_forest <- predict(cf, estimate.variance = TRUE)
sigma_hat <- sqrt(tau_hat_forest$variance.estimates)

grf_df <- 
  cbind(data.frame(
    tau_hat = tau_hat_forest$predictions,
    ci_upper = tau_hat_forest$predictions + 1.96 * sigma_hat,
    ci_lower = tau_hat_forest$predictions - 1.96 * sigma_hat),
    X
  )

grf_df$order <- with(grf_df, rank(tau_hat, ties.method = "first"))


g_base <-
  ggplot(data = grf_df, aes(tau_hat, order)) +
  geom_point(alpha=.08) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.08,
                 height = 0) +
  geom_vline(xintercept = 0, lty = 2) +
  xlab("Estimated Treatment Effect") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 	

g_base


# ----------------- Imagen con los tres graficos y base Stata ------------- #
library(grid)
library(gridExtra)

grid.arrange(g_basePor,g_baseDor,g_baseDwork,ncol=3)

# Base Stata
library(foreign)
names(wide12bP) 
names(wide12bD)

setwd('/Users/luismaldonado/Desktop')
write.dta(wide12bP,"wide12bP.dta")
write.dta(wide12bD,"wide12bD.dta")


# ------------- Heterogeneidad tratamiento pobreza: moderadores --------------- #
library(mgcv)

names(wide12bP)
names(wide12bD)

# Origen social
#####################
oriP <- gam(tau.hatPo~sexoWb+edadW+media+suptec+supun+estlabB+
                     comuna1s+comuna1na+get_ah_1_w1a+get_ah_2_w1a+
                     ideoC+ideoD+ideoNa+s(irt_z1)+igualpov+mayorpov+s(ingresosL),data=wide12bP)

summary(oriP)
plot(oriP)

oriD <- gam(tau.hatDo~sexoWb+edadW+media+suptec+supun+estlabB+
              comuna1s+comuna1na+get_ah_1_w1a+get_ah_2_w1a+
              ideoC+ideoD+ideoNa+s(irt_z1)+igualpov+mayorpov+s(ingresosL),data=wide12bD)

summary(oriD)
plot(oriD)

# Work effort
#####################
workD <- gam(tau.hatDw~sexoWb+edadW+media+suptec+supun+estlabB+
              comuna1s+comuna1na+get_ah_1_w1a+get_ah_2_w1a+
              ideoC+ideoD+ideoNa+s(irt_z1)+igualpov+mayorpov+s(ingresosL),data=wide12bD)

summary(workD)
plot(workD)








