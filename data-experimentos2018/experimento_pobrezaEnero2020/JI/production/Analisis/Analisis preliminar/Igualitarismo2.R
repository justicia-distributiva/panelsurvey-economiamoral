###################################
# Fecha: Agosto 2020              #
# Topico: escala igualitarismo    #
###################################

# Ejecutar prep-covariables-LM.R

# Igualitarismo 1: principal component y correlaciones
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

# Correlaciones
corr <- wide12 %>% dplyr:: select(egal_1_w1a,egal_2_w1a,egal_5_w1a,egal_6_w1a)
cor(corr, use="complete.obs", method="kendall") 
polychoric(corr)

# PC
wide12$igualpc <- summary(princomp(~egal_1_w1a+egal_2_w1a+egal_5_w1a+egal_6_w1a,
                                 data=wide12,na.action=na.exclude))$scores[,1]

# Igualitarismo 2: IRT ordinal, Graded Response Model - Polytomous IRT
##################################
library(ltm)

# Seleccion datos
test1  <- wide12 %>% 
  dplyr::select(egal_1_w1,egal_2_w1,egal_5_w1,egal_6_w1)

test1b <- na.omit(test1)
rcor.test(test1b, method = "kendall")

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

# Igualitarismo 3: Hierarchical graded response models
############################################################
library(hIRT)

# Seleccion datos
test1  <- wide12 %>% 
  dplyr::select(egal_1_w1,egal_2_w1,egal_5_w1,egal_6_w1)

summary(test1)

mirt <- hgrm(test1) # Modelo sin covariables
pref <- latent_scores(mirt)


# Comparacion
########################
summary(wide12$igualpc)
summary(estimates_irt2$irt_z1)
summary(pref$post_mean)

sd(wide12$igualpc)
sd(estimates_irt2$irt_z1)
sd(pref$post_mean)

hist(wide12$igualpc)
hist(estimates_irt2$irt_z1)
hist(pref$post_mean)

boxplot(wide12$igualpc)
boxplot(estimates_irt2$irt_z1)
boxplot(pref$post_mean)

table(wide12$igualpc)
table(estimates_irt2$irt_z1)
table(pref$post_mean)

# Correlacion y merge
estimates_irt2$id<-1:1781
dim(estimates_irt2)
names(estimates_irt2)

wide12$id <- 1:1781
wide12 <- merge(wide12,estimates_irt2,by="id")

pref$id <- 1:1781
wide12 <- merge(wide12,pref,by="id")
names(wide12)

with(wide12,cor(igualpc,irt_z1)) # 0.9577267
with(wide12,cor(igualpc,post_mean)) # 0.9459677
with(wide12,cor(irt_z1,post_mean)) # 0.9986613

# Ideologia politica
mean(wide12$igualpc[wide12$ideo==0])
mean(wide12$igualpc[wide12$ideo==1])
mean(wide12$igualpc[wide12$ideo==2])
mean(wide12$igualpc[wide12$ideo==3])

mean(wide12$irt_z1[wide12$ideo==0])
mean(wide12$irt_z1[wide12$ideo==1])
mean(wide12$irt_z1[wide12$ideo==2])
mean(wide12$irt_z1[wide12$ideo==3])

mean(wide12$post_mean[ideo$ideo==0])
mean(wide12$post_mean[ideo$ideo==1])
mean(wide12$post_mean[ideo$ideo==2])
mean(wide12$post_mean[ideo$ideo==3])

# Percentiles
quantile(wide12$igualpc, c(.33, .66, 1.0)) # Terciles
quantile(wide12$igualpc, c(.10,.20,.30,.40,.50,.60,.70,.80,.90,1.0)) 

quantile(wide12$irt_z1, c(.33, .66, 1.0)) # Terciles
quantile(wide12$irt_z1, c(.10,.20,.30,.40,.50,.60,.70,.80,.90,1.0)) 

quantile(wide12$post_mean, c(.33, .66, 1.0)) # Terciles
quantile(wide12$post_mean, c(.10,.20,.30,.40,.50,.60,.70,.80,.90,1.0)) 


# Graphs
#############
library(ggplot2)

ggplot(data = wide12) +
  geom_density(aes(x = post_mean))  

ggplot(data = wide12) +
  geom_density(aes(x = irt_z1))                

ggplot(data = wide12) +
  geom_density(aes(x = igualpc))

# Recodificacion
#####################
wide12$igualpc2 <- 0
wide12$igualpc2[wide12$igualpc>0.918495335532438]=1
table(wide12$igualpc,wide12$igualpc2)
table(wide12$igualpc2,exclude=NULL)

wide12$irt_z1b <- 0
wide12$irt_z1b[wide12$irt_z1>0.585472253552466]=1
table(wide12$irt_z1,wide12$irt_z1b)
table(wide12$irt_z1b,exclude=NULL)
table(wide12$irt_z1b,wide12$igualpc2)

wide12$post_mean2 <- 0
wide12$post_mean2[wide12$post_mean>0.79]=1
table(wide12$post_mean,wide12$post_mean2)
table(wide12$post_mean2,exclude=NULL)

table(wide12$irt_z1b,wide12$igualpc2)
table(wide12$irt_z1b,wide12$post_mean2)

# 50%
wide12$irt_z1b <- 0
wide12$irt_z1b[wide12$irt_z1>-0.02410968]=1
table(wide12$irt_z1,wide12$irt_z1b)
table(wide12$irt_z1b,exclude=NULL)






