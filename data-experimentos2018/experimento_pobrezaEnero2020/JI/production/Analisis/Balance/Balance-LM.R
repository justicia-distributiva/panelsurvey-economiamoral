######################
# Fecha: Marzo 2020  #
# Topico: balance    #
######################

# Tipos de balance
  # Balance por variable
  # Balance de todas juntas

library(dplyr)
library(survey)

# --------------- Covariables por separado ----------------- #
table(wide12$tratamiento,exclude=NULL) # 2 NAs

# Sexo
# Educacion: media
# Ingreso: q1
# Comuna: Santiago, regiones

# Sexo: 
#####################
table(wide12$sexoW,exclude=NULL)

# 0 es hombre, 1 es mujer
wide12$sexoW2=0 # Hombre
wide12$sexoW2[wide12$sexoW==2]=1 # Mujer
table(wide12$sexoW2,wide12$sexoW,exclude=NULL)

wide12 %>% 
   group_by(tratamiento) %>%
   summarise(mean=mean(sexoW2),n=n())
   
summary(lm(sexoW2~factor(tratamiento),data=wide12)) # F significativo

surveyobject <- svydesign(ids = ~0, data = wide12, weights = wide12$weight) 
summary(svyglm(sexoW2~factor(tratamiento), design=surveyobject))

# Edad:
####################
# 1:18-24
# 2:25-34
# 3:35-44
# 4:45-44
# 5:55 o mas

table(wide12$edadW,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(edadW),n=n())

summary(lm(edadW~factor(tratamiento),data=wide12)) # F no significativo

surveyobject <- svydesign(ids = ~0, data = wide12, weights = wide12$weight) 
summary(svyglm(edadW~factor(tratamiento), design=surveyobject))

# Educacion: 
#####################
# 0: basica o menos
# 1: media
# 2: superior no universitaria
# 3: superior universitaria

table(wide12$edcepWB,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(edcepWB),n=n())

# Analisis 1
summary(lm(edcepWB~factor(tratamiento),data=wide12))

surveyobject <- svydesign(ids = ~0, data = wide12, weights = wide12$weight) 
summary(svyglm(edcepWB~factor(tratamiento), design=surveyobject))

# Basica
wide12$basica <- 0
wide12$basica[wide12$edcepWB==0]=1
table(wide12$basica,wide12$edcepWB)
summary(lm(basica~factor(tratamiento),data=wide12)) # F no significativo

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(basica),n=n())

# Media
wide12$media <- 0
wide12$media[wide12$edcepWB==1]=1
table(wide12$media,wide12$edcepWB)
summary(lm(media~factor(tratamiento),data=wide12)) # F significativo

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(media),n=n())

# Superior no universitaria
wide12$superiorNo <- 0
wide12$superiorNo[wide12$edcepWB==2]=1
table(wide12$superiorNo,wide12$edcepWB)
summary(lm(superiorNo~factor(tratamiento),data=wide12)) # F no significativo

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(superiorNo),n=n())

# Superior universitaria
wide12$superior <- 0
wide12$superior[wide12$edcepWB==3]=1
table(wide12$superior,wide12$edcepWB)
summary(lm(superior~factor(tratamiento),data=wide12)) # F no significativo

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(superior),n=n())

# Estatus laboral: 
#######################
# 0: no trabaja
# 1: trabaja

table(wide12$estlabB,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(estlabB),n=n())

summary(lm(estlabB~factor(tratamiento),data=wide12)) # F no significativo

surveyobject <- svydesign(ids = ~0, data = wide12, weights = wide12$weight) 
summary(svyglm(estlabB~factor(tratamiento), design=surveyobject))

# Ingresos 2: continua
############################
table(wide12$ingresosL,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(ingresosL),n=n())

summary(lm(ingresosL~factor(tratamiento),data=wide12)) # No significativo, ver Kolgomorov

# Ingresos 1: percentiles
############################
# 1: q1
# 2: q2
# 3: q3
# 4: q4
# 5: NA

table(wide12$ingresosQ,exclude=NULL)

wide12$ingresosQb <- wide12$ingresosQ
wide12$ingresosQb[wide12$ingresosQ==5]=NA
table(wide12$ingresosQ,wide12$ingresosQb,exclude=NULL)

wide12In <- dplyr::select(wide12,ingresosQb,tratamiento)
wide12In <- na.omit(wide12In)
summary(wide12In)

wide12In %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(ingresosQb),n=n())

summary(lm(ingresosQb~factor(tratamiento),data=wide12))

summary(lm(ingresos_w1~factor(tratamiento),data=wide12))

# Q1
wide12$q1 <- 0
wide12$q1[wide12$ingresosQ==1]=1
wide12$q1[wide12$ingresosQ==5]=NA
table(wide12$ingresosQ,wide12$q1,exclude=NULL)

wide12In$q1 <- 0
wide12In$q1[wide12In$ingresosQ==1]=1
wide12In$q1[wide12In$ingresosQ==5]=NA
table(wide12In$ingresosQ,wide12In$q1,exclude=NULL)

wide12In %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(q1),n=n())

summary(lm(q1~factor(tratamiento),data=wide12In)) # F significativo

# Q2
wide12$q2 <- 0
wide12$q2[wide12$ingresosQ==2]=1
wide12$q2[wide12$ingresosQ==5]=NA
table(wide12$ingresosQ,wide12$q2,exclude=NULL)

wide12In$q2 <- 0
wide12In$q2[wide12In$ingresosQ==2]=1
wide12In$q2[wide12In$ingresosQ==5]=NA
table(wide12In$ingresosQ,wide12In$q2,exclude=NULL)

wide12In %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(q2),n=n())

summary(lm(q2~factor(tratamiento),data=wide12In)) # F no significativo

# Q3
wide12$q3 <- 0
wide12$q3[wide12$ingresosQ==3]=1
wide12$q3[wide12$ingresosQ==5]=NA
table(wide12$ingresosQ,wide12$q3,exclude=NULL)

wide12In$q3 <- 0
wide12In$q3[wide12In$ingresosQ==3]=1
wide12In$q3[wide12In$ingresosQ==5]=NA
table(wide12In$ingresosQ,wide12In$q3,exclude=NULL)

wide12In %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(q3),n=n())

summary(lm(q3~factor(tratamiento),data=wide12In)) # F no significativo

# Q4
wide12In$q4 <- 0
wide12In$q4[wide12In$ingresosQ==4]=1
wide12In$q4[wide12In$ingresosQ==5]=NA
table(wide12In$ingresosQ,wide12In$q4,exclude=NULL)

wide12$q4 <- 0
wide12$q4[wide12$ingresosQ==4]=1
wide12$q4[wide12$ingresosQ==5]=NA
table(wide12$ingresosQ,wide12$q4,exclude=NULL)

wide12In %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(q4),n=n())

summary(lm(q4~factor(tratamiento),data=wide12In)) # F no significativo

# Comunas
##############
table(wide12$comuna_w1C,exclude=NULL)

# Santiago
wide12$santiago <- 0
wide12$santiago[wide12$comuna_w1C==1]=1
table(wide12$santiago,wide12$comuna_w1C,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(santiago),n=n())

summary(lm(santiago~factor(tratamiento),data=wide12)) # F significativo

# Regiones
wide12$region <- 0
wide12$region[wide12$comuna_w1C==0]=1
table(wide12$region,wide12$comuna_w1C,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(region),n=n())

summary(lm(region~factor(tratamiento),data=wide12)) # F significativo

# Missings
wide12$regionM <- 0
wide12$regionM[wide12$comuna_w1C==2]=1
table(wide12$regionM,wide12$comuna_w1C,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(regionM),n=n())

summary(lm(regionM~factor(tratamiento),data=wide12)) # F No significativo

# get_ah_1_w1
#################
table(wide12$get_ah_1_w1a,exclude=NULL)

wide12In <- dplyr::select(wide12,get_ah_1_w1a,tratamiento)
wide12In <- na.omit(wide12In)
summary(wide12In)

wide12In %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(get_ah_1_w1a),n=n())

summary(lm(get_ah_1_w1a~factor(tratamiento),data=wide12In)) # F no significativo

# get_ah_2_w1
#################
table(wide12$get_ah_2_w1a,exclude=NULL)

wide12In <- dplyr::select(wide12,get_ah_2_w1a,tratamiento)
wide12In <- na.omit(wide12In)
summary(wide12In)

wide12In %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(get_ah_2_w1a),n=n())

summary(lm(get_ah_2_w1a~factor(tratamiento),data=wide12In)) # F no significativo

# ideo
###########
# 0: izquierda
# 1: centro
# 2: derecha
# 3: NAs

table(wide12$ideo,exclude=NULL)

# Izquierda
wide12$izq <- 0
wide12$izq[wide12$ideo==0]=1
table(wide12$izq,wide12$ideo,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(izq),n=n())

summary(lm(izq~factor(tratamiento),data=wide12)) # F No significativo

# Centro
wide12$cen <- 0
wide12$cen[wide12$ideo==1]=1
table(wide12$cen,wide12$ideo,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(cen),n=n())

summary(lm(cen~factor(tratamiento),data=wide12)) # F No significativo

# derecha
wide12$der <- 0
wide12$der[wide12$ideo==2]=1
table(wide12$der,wide12$ideo,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(der),n=n())

summary(lm(der~factor(tratamiento),data=wide12)) # F No significativo

# NAs
wide12$mis <- 0
wide12$mis[wide12$ideo==3]=1
table(wide12$mis,wide12$ideo,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(mis),n=n())

summary(lm(mis~factor(tratamiento),data=wide12)) # F No significativo


# Igualitarismo
####################
summary(wide12$irt_z1)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(irt_z1),n=n())

summary(lm(irt_z1~factor(tratamiento),data=wide12)) # F No es significativo

# Percepcion pobreza
#######################
table(wide12$percep_pov2,exclude=NULL)

# Menor=0
wide12$menor <- 0
wide12$menor[wide12$percep_pov2==0]=1
table(wide12$menor,wide12$percep_pov2,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(menor),n=n())

summary(lm(menor~factor(tratamiento),data=wide12)) # F No significativo

# Igual
wide12$igual <- 0
wide12$igual[wide12$percep_pov2==1]=1
table(wide12$igual,wide12$percep_pov2,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(igual),n=n())

summary(lm(igual~factor(tratamiento),data=wide12)) # F No significativo

# Mayor
wide12$mayor <- 0
wide12$mayor[wide12$percep_pov2==2]=1
table(wide12$mayor,wide12$percep_pov2,exclude=NULL)

wide12 %>% 
  group_by(tratamiento) %>%
  summarise(mean=mean(mayor),n=n())

summary(lm(mayor~factor(tratamiento),data=wide12)) # F No significativo



# --------------- Diferencias estandarizadas y balance global ----------------- #
library(RItools)
library(Matching)
library(ebal)

# Control vs. pobreza
#########################
wide12$tratamiento2 <- NA
wide12$tratamiento2[wide12$tratamiento=="control"]=0
wide12$tratamiento2[wide12$tratamiento=="pobreza"]=1
wide12$tratamiento2[wide12$tratamiento=="desigual"]=2
table(wide12$tratamiento,wide12$tratamiento2,exclude=NULL)

SDpobreza <- dplyr::select(wide12,tratamiento2,
                           sexoW2,edadW,media,superiorNo,superior,
                           estlabB,region,regionM,
                           get_ah_1_w1a,get_ah_2_w1a,cen,der,mis,ingresosL,irt_z1,igual,mayor)

SDpobreza <- filter(SDpobreza,tratamiento2 < 2)
dim(SDpobreza)
str(SDpobreza)

xBalance(tratamiento2~sexoW2+edadW+media+superiorNo+superior+
                      estlabB+region+regionM+
                      get_ah_1_w1a+get_ah_2_w1a+cen+der+mis+ingresosL+irt_z1+igual+mayor, 
                      report = c("chisquare.test", "std.diffs","adj.means"),
                      data = SDpobreza)

bal1 <- MatchBalance(tratamiento2~sexoW2+edadW+media+superiorNo+superior+
                                  estlabB+region+regionM+
                                  get_ah_1_w1a+get_ah_2_w1a+cen+der+mis+ingresosL+irt_z1+igual+mayor, 
                                  match.out = NULL,ks=TRUE,data = SDpobreza)

bal1 <- MatchBalance(tratamiento2~.,match.out = NULL,ks=TRUE,data = SDpobreza)
bal1.label <-c("sexoW2","edadW","media","superiorNo","superior",
              "estlabB","region","regionM",
              "get_ah_1_w1a","get_ah_2_w1a","cen","der","mis","ingresos_w1","irt_z1","igual","mayor") 
bal1.m1  <- baltest.collect(matchbal.out=bal1,var.names=bal1.label,after=FALSE)
round(bal1.m1,2)


# Control vs. desigual
#########################
SDdesigual <- dplyr::select(wide12,tratamiento2,
                           sexoW2,edadW,media,superiorNo,superior,
                           estlabB,region,regionM,
                           get_ah_1_w1a,get_ah_2_w1a,cen,der,mis,ingresosL,irt_z1,igual,mayor)

SDdesigual <- filter(SDdesigual,tratamiento2==0 | tratamiento2==2)
dim(SDdesigual)
str(SDdesigual)

table(SDdesigual$tratamiento2,exclude=NULL)

SDdesigual$tratamiento3 <- 0
SDdesigual$tratamiento3[SDdesigual$tratamiento2==2]=1
table(SDdesigual$tratamiento3,SDdesigual$tratamiento2,exclude=NULL)

SDdesigual$tratamiento2 <- NULL
names(SDdesigual)

xBalance(tratamiento3~sexoW2+edadW+media+superiorNo+superior+
                      estlabB+region+regionM+
                      get_ah_1_w1a+get_ah_2_w1a+cen+der+mis+ingresosL+irt_z1+igual+mayor, 
                      report = c("chisquare.test", "std.diffs","adj.means"),
                      data = SDdesigual)

bal1 <- MatchBalance(tratamiento3~.,match.out = NULL,ks=TRUE,data = SDdesigual)
bal1.label <-c("sexoW2","edadW","media","superiorNo","superior",
               "estlabB","region","regionM",
               "get_ah_1_w1a","get_ah_2_w1a","cen","der","mis","ingresos_w1","irt_z1","igual","mayor") 
bal1.m1  <- baltest.collect(matchbal.out=bal1,var.names=bal1.label,after=FALSE)
round(bal1.m1,2)










