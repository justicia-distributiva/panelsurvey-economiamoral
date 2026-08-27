###################################
# Fecha: Agosto 2020              #
# Topico: escala igualitarismo    #
###################################

# Ejecutar prep-covariables-LM.R

# Igualitarismo 1: principal component
###########################################
table(wide12$egal_1_w1,exclude=NULL) # 7 NAs
wide12$egal_1_w1a <- wide12$egal_1_w1
wide12$egal_1_w1a[is.na(wide12$egal_1_w1)]=mean(wide12$egal_1_w1,na.rm=T)
table(wide12$egal_1_w1a,exclude=NULL)

table(wide12$egal_2_w1,exclude=NULL) # 7 NAs
wide12$egal_2_w1a <- wide12$egal_2_w1
wide12$egal_2_w1a[is.na(wide12$egal_2_w1)]=mean(wide12$egal_2_w1,na.rm=T)
table(wide12$egal_2_w1a,exclude=NULL)

table(wide12$egal_5_w1,exclude=NULL) # 8 NAs
wide12$egal_5_w1a <- wide12$egal_5_w1
wide12$egal_5_w1a[is.na(wide12$egal_5_w1)]=mean(wide12$egal_5_w1,na.rm=T)
table(wide12$egal_5_w1a,exclude=NULL)

table(wide12$egal_6_w1,exclude=NULL) # 8 NAs
wide12$egal_6_w1a <- wide12$egal_6_w1
wide12$egal_6_w1a[is.na(wide12$egal_6_w1)]=mean(wide12$egal_6_w1,na.rm=T)
table(wide12$egal_6_w1a,exclude=NULL)

corr <- wide12 %>% dplyr:: select(egal_1_w1a,egal_2_w1a,egal_5_w1a,egal_6_w1a)
cor(corr, use="complete.obs", method="kendall") 
polychoric(corr)

wide12$igual <- summary(princomp(~egal_1_w1a+egal_2_w1a+egal_5_w1a+egal_6_w1a,
                                 data=wide12,na.action=na.exclude))$scores[,1]
summary(wide12$igual)
table(wide12$igual)

hist(wide12$igual)
table(wide12$igual)
boxplot(wide12$igual)

corr <- wide12 %>% dplyr:: select(egal_1_w1a,egal_2_w1a,egal_5_w1a,egal_6_w1a,igual)
cor(corr, use="complete.obs", method="kendall") 

table(wide12$ideo,exclude=NULL)
mean(wide12$igual[wide12$ideo==0])
mean(wide12$igual[wide12$ideo==1])
mean(wide12$igual[wide12$ideo==2])
mean(wide12$igual[wide12$ideo==3])

quantile(wide12$igual, c(.33, .66, 1.0)) # Terciles
summary(wide12$igual)

wide12$igual2 <- 0
wide12$igual2[wide12$igual>0.918495335532438]=1
table(wide12$igual,wide12$igual2)
table(wide12$igual2)

# Igualitarismo 2: IRT binario
#################################
library(psych)

table(wide12$egal_1_w1,exclude=NULL) # 7 NAs
wide12$egal_1_w1b <- 0
wide12$egal_1_w1b[wide12$egal_1_w1>3]=1
table(wide12$egal_1_w1b,exclude=NULL)
table(wide12$egal_1_w1,wide12$egal_1_w1b,exclude=NULL)

wide12$egal_2_w1b <- 0
wide12$egal_2_w1b[wide12$egal_2_w1>3]=1
table(wide12$egal_2_w1b,exclude=NULL)
table(wide12$egal_2_w1,wide12$egal_2_w1b,exclude=NULL)

wide12$egal_5_w1b <- 0
wide12$egal_5_w1b[wide12$egal_5_w1>3]=1
table(wide12$egal_5_w1b,exclude=NULL)
table(wide12$egal_5_w1,wide12$egal_5_w1b,exclude=NULL)

wide12$egal_6_w1b <- 0
wide12$egal_6_w1b[wide12$egal_6_w1>3]=1
table(wide12$egal_6_w1b,exclude=NULL)
table(wide12$egal_6_w1,wide12$egal_6_w1b,exclude=NULL)

test1  <- wide12 %>% 
  dplyr::select(egal_1_w1b,egal_2_w1b,egal_5_w1b,egal_6_w1b)

summary(test1)
tetrachoric(test1)

library(MCMCpack)
test2 <- as.matrix(test1)
head(test2)

posterior1 <- MCMCirt1d(test2, theta.constraints=list("1"="+"),burnin=500, mcmc=100000, thin=20, verbose=500,store.item=TRUE)
str(posterior1)
summary(posterior1)

library(ltm)
model_irt<-ltm(test1~z1)
summary(model_irt)

plot(model_irt, legend=TRUE)
plot(model_irt, type="IIC")
plot(model_irt, type="IIC", items=0)
information(model_irt,c(0,4))
information(model_irt,c(-4,0))

estimates_irt<-factor.scores(model_irt,resp.patterns = test1) 
temp<-estimates_irt[1]$score.dat
estimates_irt2<-dplyr::select(temp,z1)
estimates_irt2<-dplyr::rename(estimates_irt2,irt_z1=z1)

mean(estimates_irt2$irt_z1[wide12$ideo==0])
mean(estimates_irt2$irt_z1[wide12$ideo==1])
mean(estimates_irt2$irt_z1[wide12$ideo==2])
mean(estimates_irt2$irt_z1[wide12$ideo==3])

quantile(estimates_irt2$irt_z1, c(.33, .66, 1.0)) # Terciles
summary(estimates_irt2$irt_z1)
table(estimates_irt2$irt_z1)

estimates_irt2$irt_z2 <- 0
estimates_irt2$irt_z2[estimates_irt2$irt_z1>-0.636447177691233]=1
table(estimates_irt2$irt_z2,estimates_irt2$irt_z1)
table(estimates_irt2$irt_z2)

# Comparacion irt vs. principal component
hist(estimates_irt2$irt_z1)
table(estimates_irt2$irt_z1)
boxplot(estimates_irt2$irt_z1)

hist(wide12$igual)
table(wide12$igual)
boxplot(wide12$igual)

# Merge
estimates_irt2$id<-1:1781
dim(estimates_irt2)
names(estimates_irt2)

wide12$id <- 1:1781
wide12 <- merge(wide12,estimates_irt2,by="id")
dim(wide12)
names(wide12)

with(wide12,cor(igual,irt_z1)) # 0.8846099

# Igualitarismo 3: IRT ordinal, Graded Response Model - Polytomous IRT
##################################
library(ltm)

# Seleccion datos
test1  <- wide12 %>% 
  dplyr::select(egal_1_w1,egal_2_w1,egal_5_w1,egal_6_w1)

test1 <- na.omit(test1)
summary(test1)

rcor.test(test1, method = "kendall")

# Estimacion modelo
model_irta<-grm(data=test1,constrained=TRUE) # Rasch model
model_irtb<-grm(data=test1)
anova(model_irta,model_irtb) # unscontrained GRM is preferable.

model_irtb

information(model_irtb,c(-4,4))
information(model_irtb,c(-4,4),items=c(1)) # 100*4.88/33.03 = 14.77445
information(model_irtb,c(-4,4),items=c(2)) # 100*8.17/33.03 = 24.73509
information(model_irtb,c(-4,4),items=c(3)) # 100*12.97/33.03 = 39.26733
information(model_irtb,c(-4,4),items=c(4)) # 100*7.01/33.03 = 21.22313

# Extraccion de variable latente
estimates_irt<-factor.scores(model_irtb,resp.patterns = test1) 
temp<-estimates_irt[1]$score.dat
estimates_irt2<-dplyr::select(temp,z1)
estimates_irt2<-dplyr::rename(estimates_irt2,irt_z1=z1)

mean(estimates_irt2$irt_z1[wide12$ideo==0])
mean(estimates_irt2$irt_z1[wide12$ideo==1])
mean(estimates_irt2$irt_z1[wide12$ideo==2])
mean(estimates_irt2$irt_z1[wide12$ideo==3])

hist(estimates_irt2$irt_z1)
table(estimates_irt2$irt_z1)
boxplot(estimates_irt2$irt_z1)

ideo  <- wide12 %>% 
  dplyr::select(egal_1_w1,egal_2_w1,egal_5_w1,egal_6_w1,ideo)

ideo$ideo <- as.factor(ideo$ideo)
ideo <- na.omit(ideo)
dim(ideo)

mean(estimates_irt2$irt_z1[ideo$ideo==0])
mean(estimates_irt2$irt_z1[ideo$ideo==1])
mean(estimates_irt2$irt_z1[ideo$ideo==2])
mean(estimates_irt2$irt_z1[ideo$ideo==3])

quantile(estimates_irt2$irt_z1, c(.33, .66, 1.0)) # Terciles

# Igualitarismo 4: Hierarchical graded response models
############################################################
library(hIRT)

y <- nes_econ2008[, -(1:3)]
x <- model.matrix( ~ party * educ, nes_econ2008)
z <- model.matrix( ~ party, nes_econ2008)
nes_m1 <- hgrm(y, x, z)
nes_m1

pref <- latent_scores(nes_m1)
require(ggplot2)
ggplot(data = nes_econ2008) +
geom_density(aes(x = pref$post_mean, col = party))

y <- ideo[,-(5)] # Sin ideo
mirt <- hgrm(y) # Modelo sin covariables
pref <- latent_scores(mirt)

summary(pref$post_mean)
sd(pref$post_mean)
hist(pref$post_mean)
table(pref$post_mean)
boxplot(pref$post_mean)

mean(pref$post_mean[ideo$ideo==0])
mean(pref$post_mean[ideo$ideo==1])
mean(pref$post_mean[ideo$ideo==2])
mean(pref$post_mean[ideo$ideo==3])

ggplot(data = ideo) +
  geom_density(aes(x = pref$post_mean, col = ideo))

quantile(pref$post_mean, c(.33, .66, 1.0)) # Terciles


