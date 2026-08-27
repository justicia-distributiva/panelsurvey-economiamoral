###################################
# Fecha: Agosto 2020              #
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


# ---------- Preparacion de datos -------------- #

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
table(wide12$ingresosQ,exclude=NULL)

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


# --------------- Igualitarismo ---------------- #

# Full dataset: para usar con as.factor
wide12b <- dplyr::select(wide12,zofamilia2,zdvE2,tratamiento,
                         sexoW,edadW,edcepWB,estlabB,ingresosQ,comuna_w1C,
                         get_ah_1_w1a,get_ah_2_w1a,ideo,irt_z1,percep_pov2,irt_z1b,ingresos_w1)

# Full dataset: para usar sin as.factor
wide12b <- dplyr::select(wide12,zofamilia2,tratamiento,
                         sexoWb,edadW,media,suptec,supun,estlabB,
                         ingresosQ,comuna1s,comuna1na,get_ah_1_w1a,get_ah_2_w1a,
                         ideoC,ideoD,ideoNa,irt_z1,igualpov,mayorpov,irt_z1b,ingresos_w1)

# Dataset pobreza
wide12bP <- filter(wide12b,tratamiento!="desigual") %>%  na.omit() 
dim(wide12bP)

wide12bP$tratamientoP <- 0
wide12bP$tratamientoP[wide12bP$tratamiento=="pobreza"]=1
table(wide12bP$tratamientoP,wide12bP$tratamiento,exclude=NULL)
wide12bP$tratamiento <- NULL
names(wide12bP)       
summary(wide12bP)

# BART para condicion pobreza
###################################
# Origen
fit <- bartc(zofamilia2,tratamientoP,
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   factor(ingresosQ)+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12bP,n.samples = 100L,p.scoreAsCovariate = TRUE)

# Indice educacion
fit <- bartc(zdvE2,tratamientoP,
               factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
               factor(ingresosQ)+factor(comuna_w1C)+
               get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
               data=wide12bP,n.samples = 100L,p.scoreAsCovariate = TRUE)


summary(fit)
summary(fit,target=c("pate"))
summary(fit,target=c("sate"))
summary(fit,target=c("cate"))

# Grafico efectos individuales 
ites <- extract(fit, type = "icate")

gg_pob <-
  data_frame(
    tau_hat = apply(ites, 2, mean),
    ci_upper = apply(ites, 2, quantile, 0.975),
    ci_lower = apply(ites, 2, quantile, 0.025),
    ordering = rank(tau_hat, ties.method = "first"), 
    treatment = "Poverty"
  )	

mean(gg_pob$tau_hat) # ATE
boxplot(gg_pob$tau_hat)

g_base <-
  ggplot(data = gg_pob, aes(tau_hat, ordering)) +
  geom_point(alpha=.05) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.05,
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

# Graficos de sub-grupos
ites <- extract(fit, type = "icate")
ite.m <- apply(ites, 2, mean)

ite.sd <- apply(ites, 2, sd) 
ite.lb <- ite.m - 2 * ite.sd 
ite.ub <- ite.m + 2 * ite.sd

ite.m <- as.data.frame(ite.m)
ite.m$id <- 1:1180
wide12bP$id <- 1:1180
wide12bP <- merge(wide12bP,ite.m,by="id")
names(wide12bP)

p <- ggplot(data=wide12bP,
            mapping= aes(x=irt_z1, y=ite.m)) + 
            geom_point(alpha=0.1)


boxplot(ite.m~edcepWB,data=wide12bP)
boxplot(ite.m~percep_pov2,data=wide12bP)
boxplot(ite.m~irt_z1b,data=wide12bP)

# Causal forest for poverty
##############################
library(grf)

names(wide12bP)

W = wide12bP$tratamientoP
Y = wide12bP$zofamilia2

X = wide12bP[,-(1:1)] # Elimina zofamilia2
names(X)
X = X[,-(18:18)] # Elimina irt_z1b
names(X)
X = X[,-(19:19)] # Elimina tratamientoP
names(X)
X = X[,-(7:7)] # Elimina ingresosQ
names(X)
summary(X)

Y.forest = regression_forest(X, Y)
Y.hat = predict(Y.forest)$predictions
W.forest = regression_forest(X, W)
W.hat = predict(W.forest)$predictions

cf.raw = causal_forest(X, Y, W,
                       Y.hat = Y.hat, W.hat = W.hat)
                       
varimp = variable_importance(cf.raw)
selected.idx = which(varimp > mean(varimp))

cf = causal_forest(X[,selected.idx], Y, W,
                   Y.hat = Y.hat, W.hat = W.hat,
                   tune.parameters = "all")

tau.hat = predict(cf)$predictions

boxplot(tau.hat)
hist(tau.hat)
mean(tau.hat)

# ATE
ATE = average_treatment_effect(cf)
paste("95% CI for the ATE:", round(ATE[1], 3),"+/-", round(qnorm(0.975) * ATE[2], 3))

#
# Omnibus tests for heterogeneity
#

test_calibration(cf)

#
# Regresion
#



#
# Graphs
#

# Overall heterogeneity
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

# How many CIs include zero? 
grf_df %>% 
  summarise(mean(ci_lower <= 0 & ci_upper >= 0))

# What proportion of the CIs that don't include zero are positive? 
these <- with(grf_df, which(ci_lower <= 0 & ci_upper >= 0))

grf_df[-these,] %>% 
  filter(tau_hat > 0) %>% 
  nrow()/nrow(grf_df)



# Others graphs
pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
boxplot(tau.hat ~  wide12bP$edcepWB, xlab = "Education", ylab = "Estimated CATE")
lines(smooth.spline(1 + wide12bP$edcepWB, tau.hat, df = 4), lwd = 2, col = 4)

plot(X$irt_z1,tau.hat) # Igualitarismo
boxplot(tau.hat ~  wide12bP$irt_z1b, xlab = "Egalitarianism", ylab = "Estimated CATE")

boxplot(tau.hat ~  wide12bP$ingresosQ, xlab = "Income groups", ylab = "Estimated CATE")


plot(X$ingresos_w1,tau.hat) # Ingreso

boxplot(tau.hat ~  wide12bP$ingresos_w1, xlab = "Household income", ylab = "Estimated CATE")
lines(smooth.spline(wide12bP$ingresos_w1, tau.hat, df = 4), lwd = 2, col = 4)

################################################################### 
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
                     
df1 <- wide12b %>% mutate(tratamiento = "control")
df2 <- wide12b %>% mutate(tratamiento = "pobreza")
df3 <- wide12b %>% mutate(tratamiento = "desigual")  

bart_df <- bind_rows(df1, df2, df3)
xp=as.data.frame(bart_df[,-2])

#bart_df$tratamiento <- as.factor(bart_df$tratamiento)

bart_model <- dbarts(formula = zofamilia2 ~ as.factor(tratamiento) + 
                               sexoWb + edadW + media + suptec + supun + estlabB +
                               as.factor(ingresosQ) + comuna1s + comuna1na + get_ah_1_w1a + get_ah_2_w1a +
                               ideoC + ideoD + ideoNa + irt_z1 + igualpov + mayorpov,
                     test=xp,data=wide12b)

# Opcion 1
bart_fit_1_1 <- bart_model$run()
str(bart_fit_1_1)

# check convergence
plot(bart_fit_1_1$sigma)

n <- nrow(wide12b)
bart_test <- plyr::alply(bart_fit_1_1$test, 3)
str(bart_test)                       
                       
bart_test <- do.call(cbind, bart_test)
pob_mat <- bart_test[(n + 1):(2 * n),] - bart_test[1:n,] # Efectos causales individuales pobreza

des_mat <- bart_test[((2 * n) + 1):(3 * n),] - bart_test[1:n,] # Efectos causales individuales desigualdad                     
                       
# Analisis pobreza I: global
gg_pob <-
  data_frame(
    tau_hat = apply(pob_mat, 1, mean),
    ci_upper = apply(pob_mat, 1, quantile, 0.975),
    ci_lower = apply(pob_mat, 1, quantile, 0.025),
    ordering = rank(tau_hat, ties.method = "first"), 
    treatment = "Poverty"
  )	

mean(gg_pob$tau_hat) # ATE: 0.1448988
sd(gg_pob$tau_hat) # SD: 0.1202192 

mean(gg_pob$tau_hat) - 1.96*sd(gg_pob$tau_hat) # -0.09128435
mean(gg_pob$tau_hat) + 1.96*sd(gg_pob$tau_hat) # 0.3799749

g_base <-
  ggplot(data = gg_pob, aes(tau_hat, ordering)) +
  geom_point(alpha=.05) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.05,
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

# Analisis pobreza II: subgrupos
gg_pob2 <-
  data_frame(
    tau_hat = apply(pob_mat, 1, mean),
    ci_upper = apply(pob_mat, 1, quantile, 0.975),
    ci_lower = apply(pob_mat, 1, quantile, 0.025)
  )	

# Pegar a base con covariables
wide12b$tau_hat <- gg_pob2$tau_hat 
wide12b$ci_upper <- gg_pob2$ci_upper 
wide12b$ci_lower <- gg_pob2$ci_lower 

p <- ggplot(data=wide12b,
            mapping= aes(x=irt_z1, y=tau_hat))

p + geom_point(alpha=0.1)

mean(wide12b$tau_hat[wide12b$irt_z1>0.60])

# Analisis con Kernel
library(foreign)

setwd('/Users/luismaldonado/Desktop')
write.dta(wide12b,"wide12b.dta")


# Dataset: solo placebo y condicion de pobreza
#################################################

wide12b <- dplyr::select(wide12,tratamiento,zofamilia2,
                         sexoWb,edadW,media,suptec,supun,estlabB,
                         ingresosQ,comuna1s,comuna1na,get_ah_1_w1a,get_ah_2_w1a,
                         ideoC,ideoD,ideoNa,irt_z1,igualpov,mayorpov) %>%
                  filter(tratamiento!="desigual") %>%
                  na.omit()

wide12b$tratamiento2 <- 0
wide12b$tratamiento2[wide12b$tratamiento=="pobreza"]=1
table(wide12b$tratamiento2,wide12b$tratamiento,exclude=NULL)
wide12b$tratamiento <- NULL
summary(wide12b)     

df1 <- wide12b %>% mutate(tratamiento2 = 0)
df2 <- wide12b %>% mutate(tratamiento2 = 1)

bart_df <- bind_rows(df1, df2)
xp=as.data.frame(bart_df[,-1])
names(xp)

bart_model <- dbarts(formula = zofamilia2 ~ as.factor(tratamiento2) + 
                       sexoWb + edadW + media + suptec + supun + estlabB +
                       as.factor(ingresosQ) + comuna1s + comuna1na + get_ah_1_w1a + get_ah_2_w1a +
                       ideoC + ideoD + ideoNa + irt_z1 + igualpov + mayorpov,
                     test=xp,data=wide12b)

# Opcion 1
bart_fit_1_1 <- bart_model$run()
str(bart_fit_1_1)

# check convergence
plot(bart_fit_1_1$sigma)

n <- nrow(wide12b)
bart_test <- plyr::alply(bart_fit_1_1$test, 3)
str(bart_test)                       

bart_test <- do.call(cbind, bart_test)
pob_mat <- bart_test[(n + 1):(2 * n),] - bart_test[1:n,] # Efectos causales individuales pobreza

# Analisis pobreza I: global
gg_pob <-
  data_frame(
    tau_hat = apply(pob_mat, 1, mean),
    ci_upper = apply(pob_mat, 1, quantile, 0.975),
    ci_lower = apply(pob_mat, 1, quantile, 0.025),
    ordering = rank(tau_hat, ties.method = "first"), 
    treatment = "Poverty"
  )	

mean(gg_pob$tau_hat) # ATE: 0.1448988
sd(gg_pob$tau_hat) # SD: 0.1202192 

mean(gg_pob$tau_hat) - 1.96*sd(gg_pob$tau_hat) # -0.09128435
mean(gg_pob$tau_hat) + 1.96*sd(gg_pob$tau_hat) # 0.3799749

g_base <-
  ggplot(data = gg_pob, aes(tau_hat, ordering)) +
  geom_point(alpha=.05) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 alpha = 0.05,
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

# Analisis pobreza II: subgrupos
gg_pob2 <-
  data_frame(
    tau_hat = apply(pob_mat, 1, mean),
    ci_upper = apply(pob_mat, 1, quantile, 0.975),
    ci_lower = apply(pob_mat, 1, quantile, 0.025)
  )	

# Pegar a base con covariables
wide12b$tau_hat <- gg_pob2$tau_hat 
wide12b$ci_upper <- gg_pob2$ci_upper 
wide12b$ci_lower <- gg_pob2$ci_lower 

p <- ggplot(data=wide12b,
            mapping= aes(x=irt_z1, y=tau_hat))

p + geom_point(alpha=0.1)
                       
# bartCause
library(bartCause)

names(wide12b)
wide12b=wide12b[,-20]
wide12b=wide12b[,-21]
wide12b=wide12b[,-22]


cov <- dplyr::select(wide12b,
                         sexoWb,edadW,media,suptec,supun,estlabB,
                         ingresosQ,comuna1s,comuna1na,get_ah_1_w1a,get_ah_2_w1a,
                         ideoC,ideoD,ideoNa,irt_z1,igualpov,mayorpov)
  
dim(wide12b)
dim(cov)

fit <- bartc(zofamilia2, tratamiento2,  
             sexoWb+edadW+media+suptec+supun+estlabB+
             ingresosQ+comuna1s+comuna1na+get_ah_1_w1a+get_ah_2_w1a+
             ideoC+ideoD+ideoNa+irt_z1+igualpov+mayorpov, 
             data=wide12b,n.samples = 100L,p.scoreAsCovariate = TRUE)

summary(fit)
summary(fit,target=c("pate"))
summary(fit,target=c("sate"))
summary(fit,target=c("cate"))

plot_sigma(fit)
plot_est(fit)

plot_indiv(fit)
plot_indiv(fit,type=c("icate"))


# bartCause:  
ites <- extract(fit, type = "icate")

gg_pob <-
  data_frame(
    tau_hat = apply(ites, 2, mean),
    ci_upper = apply(ites, 2, quantile, 0.975),
    ci_lower = apply(ites, 2, quantile, 0.025),
    ordering = rank(tau_hat, ties.method = "first"), 
    treatment = "Poverty"
  )	

ites <- extract(fit, type = "icate")
ite.m <- apply(ites, 2, mean)

ite.sd <- apply(ites, 2, sd) 
ite.lb <- ite.m - 2 * ite.sd 
ite.ub <- ite.m + 2 * ite.sd

ite.o <- order(ite.m)

mean(gg_pob$tau_hat) 
sd(gg_pob$tau_hat) 

mean(gg_pob$tau_hat) - 1.96*sd(gg_pob$tau_hat) 
mean(gg_pob$tau_hat) + 1.96*sd(gg_pob$tau_hat) 

# Graph
plot(NULL, type = "n",
     xlim = c(1, length(ite.m)), ylim = range(ite.lb, ite.ub), 
     xlab = "effect order", ylab = "individual treatment effect")
 
lines(rbind(seq_along(ite.m), seq_along(ite.m), NA), rbind(ite.lb[ite.o], ite.ub[ite.o], NA), lwd = 0.5)

points(seq_along(ite.m), ite.m[ite.o], pch = 20)

# Pegar
ite.m <- as.data.frame(ite.m)
ite.m$id <- 1:1180
wide12b$id <- 1:1180
wide12b <- merge(wide12b,ite.m,by="id")
names(wide12b)

p <- ggplot(data=wide12b,
            mapping= aes(x=irt_z1, y=ite.m))

p + geom_point(alpha=0.1)

summary(lm(ite.m ~ sexoWb+edadW+media+suptec+supun+estlabB+
             ingresosQ+comuna1s+comuna1na+get_ah_1_w1a+get_ah_2_w1a+
             ideoC+ideoD+ideoNa+irt_z1+igualpov+mayorpov,data=wide12b))










