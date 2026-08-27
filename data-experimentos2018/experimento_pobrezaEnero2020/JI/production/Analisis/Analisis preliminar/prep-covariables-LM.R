###################################
# Fecha: Febrero 2020             #
# Topico: Preparacion covariables #
###################################

library(plyr)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(questionr)

load("~/Dropbox/fondecyt regular 2015/Estudio cuantitativo/data-experimentos2018/experimento_pobrezaEnero2020/JI/input/data/proc/wide12.RData")
names(wide12) # 208 variables (13-09-2020)
sjPlot::view_df(x = wide12,show.values = T,show.frq = T,show.prc = T)

# ---------------- Informe sobre missing ------------------- #

# VD: 22 missings
  # Origen
    # get_ah_1_w2: 22 NAs
  # Trabajo duro
    # get_ah_5_w2: 22 NAs
  # Educacion
    # get_ah_estr_w2: 22 NAs
    # get_ah_3_w2: 22 NAs
    # get_ah_indv_w2: 22 NAs

# Tratamiento: 2 missings

# Covariables
  # Genero: 0 NAs
  # Edad: 0 NAs
  # Educacion: 0 NAs
  # Estatus laboral: 0 NAs
  # Ingreso: ingreso: 0 NAs, categoria
  # Tener padres con altos niveles de educación: 
  # get_ah_1_w1: 5 NAs
  # Tener ambicion
    # get_ah_2_w1: 5 NAs
  # Izquierda y derecha: 0 NAs, categoria
  # Comuna: 0 NAs, categoria
  # Igualitarismo
    # egal_1_w1: 7 NAs
    # egal_2_w1: 7 NAs
    # egal_5_w1: 7 NAs
    # egal_6_w1: 8 NAs
  # Percepcion pobreza
    # percep_pov_w1: 5 NAs, se eliminan


# ----------------- Atencion y variables de atencion --------------- #
# Tratamiento
###################
table(wide12$FL_6_DO_Tratamiento_w2,exclude=NULL) # 592
table(wide12$FL_6_DO_Control_w2,exclude=NULL) # 592
table(wide12$FL_6_DO_Tratamiento_desigualdad_w2,exclude=NULL) # 595

table(wide12$tratamiento,exclude=NULL) 
  # 592 control
  # 592 pobreza
  # 595 desigual
  # 2 NAs

# Check de manipulation
##########################
# Variable generada: manip
table(wide12$mani_poverty_w2,exclude=NULL) # N=586, deberian ser 592
prop.table(table(wide12$mani_poverty_w2)) # 80% correcto

table(wide12$mani_placebo_w2,exclude=NULL) # N=589, deberian ser 592
prop.table(table(wide12$mani_placebo_w2)) # 83% correcto

table(wide12$mani_desigualdad_w2,exclude=NULL) # N=587, deberian ser 595
prop.table(table(wide12$mani_desigualdad_w2)) # 79% correcto

wide12$manip <- 0 # Incorrectos + NAs = Otros
wide12$manip[wide12$mani_poverty_w2==4]=1 # correcto pobreza
wide12$manip[wide12$mani_placebo_w2==3]=2 # correcto placebo
wide12$manip[wide12$mani_desigualdad_w2==3]=3 # correcto desigualdad
table(wide12$manip,exclude=NULL)
prop.table(table(wide12$manip,exclude=NULL))

# Check de atencion
######################
# Variable generada: atencionB
table(wide12$atencion_w2,exclude=NULL) # 1724 ponen atencion, 57 no ponen atencion

wide12$atencionB <- 0
wide12$atencionB[wide12$atencion_w2==5]=1
table(wide12$atencionB,wide12$atencion_w2,exclude=NULL)
prop.table(table(wide12$atencionB))


# Duracion
###############
# W1
summary(wide12$Duration__in_seconds__w1/60)
boxplot(wide12$Duration__in_seconds__w1)

summary(wide12$Duration__in_seconds__w2[wide12$atencionB_w2==1]/60) # Pusieron antencion
boxplot(wide12$Duration__in_seconds__w2[wide12$atencionB_w2==1])

#W2
summary(wide12$Duration__in_seconds__w2/60)
boxplot(wide12$Duration__in_seconds__w2)

summary(wide12$Duration__in_seconds__w2[wide12$atencionB_w2==0]/60) # No pusieron atencion 
boxplot(wide12$Duration__in_seconds__w2[wide12$atencionB_w2==0])

# Quartiles para w2
# Variable generada: durationB
summary(wide12$Duration__in_seconds__w2)
quantile(wide12$Duration__in_seconds__w2)

wide12$durationB <- 1
wide12$durationB[wide12$Duration__in_seconds__w2>375&wide12$Duration__in_seconds__w2<512]=2
wide12$durationB[wide12$Duration__in_seconds__w2>511&wide12$Duration__in_seconds__w2<740]=3
wide12$durationB[wide12$Duration__in_seconds__w2>739]=4

table(wide12$durationB,exclude=NULL)

# --------------- Outcomes -------------- #
# Indicadores estructurales
table(wide12$get_ah_estr_w2,exclude=NULL) # Educacion de calidad
prop.table(table(wide12$get_ah_estr_w2,exclude=NULL)) # 22 NAs
wide12$get_ah_estr_w2a <- wide12$get_ah_estr_w2
wide12$get_ah_estr_w2a[is.na(wide12$get_ah_estr_w2)]=mean(wide12$get_ah_estr_w2,na.rm=T)
table(wide12$get_ah_estr_w2a,exclude=NULL)

table(wide12$get_ah_1_w2,exclude=NULL) # Familia rica
prop.table(table(wide12$get_ah_1_w2,exclude=NULL)) # 22 NAs
wide12$get_ah_1_w2a <- wide12$get_ah_1_w2
wide12$get_ah_1_w2a[is.na(wide12$get_ah_1_w2)]=mean(wide12$get_ah_1_w2,na.rm=T)
table(wide12$get_ah_1_w2a,exclude=NULL)

# Indicadores individuales
table(wide12$get_ah_3_w2,exclude=NULL) # Buen nivel educacional
prop.table(table(wide12$get_ah_3_w2,exclude=NULL)) # 22 NAs
wide12$get_ah_3_w2a <- wide12$get_ah_3_w2
wide12$get_ah_3_w2a[is.na(wide12$get_ah_3_w2)]=mean(wide12$get_ah_3_w2,na.rm=T)
table(wide12$get_ah_3_w2a,exclude=NULL)

table(wide12$get_ah_5_w2,exclude=NULL) # Trabajo duro
prop.table(table(wide12$get_ah_5_w2,exclude=NULL)) # 22 NAs
wide12$get_ah_5_w2a <- wide12$get_ah_5_w2 
wide12$get_ah_5_w2a[is.na(wide12$get_ah_5_w2)]=mean(wide12$get_ah_5_w2,na.rm=T)
table(wide12$get_ah_5_w2a,exclude=NULL)

table(wide12$get_ah_indv_w2,exclude=NULL) # Haber alcanzado un buen nivel de educación
prop.table(table(wide12$get_ah_indv_w2,exclude=NULL)) # 22 NAs
wide12$get_ah_indv_w2a <- wide12$get_ah_indv_w2
wide12$get_ah_indv_w2a[is.na(wide12$get_ah_indv_w2)]=mean(wide12$get_ah_indv_w2,na.rm=T)
table(wide12$get_ah_indv_w2a,exclude=NULL)

# Correlaciones
library(psych)
library(ltm)

corr <- wide12 %>% dplyr:: select(get_ah_estr_w2,get_ah_1_w2,
                                  get_ah_3_w2,get_ah_5_w2,get_ah_indv_w2)

cor(corr, use="complete.obs", method="kendall") 
polychoric(corr)

# Indice de variables de educacion: componentes principales
# Variable generada: dvE, SdvE (estandarizada)
factor.obj <- princomp(~get_ah_estr_w2+get_ah_3_w2+get_ah_indv_w2,
                       data=wide12,na.action=na.exclude)
print(loadings(factor.obj))

wide12$dvE <- summary(princomp(~get_ah_estr_w2a+get_ah_3_w2a+get_ah_indv_w2a,
                      data=wide12,na.action=na.exclude))$scores[,1]

summary(wide12$dvE) # 0 NAs

corr <- wide12 %>% dplyr:: select(dvE,SdvE,get_ah_estr_w2,get_ah_3_w2,get_ah_indv_w2)
corr <- na.omit(corr)
sd(corr$dvE) # 1.204799
sd(corr$SdvE) # 1

cor(corr)

# -----------------Covariables ------------------ #
# Genero
table(wide12$sexoW,exclude=NULL)

wide12$sexoWb <- 0
wide12$sexoWb[wide12$sexoW==2]=1 # Mujer
table(wide12$sexoW,wide12$sexoWb,exclude=NULL)

# Edad
table(wide12$edadW,exclude=NULL)

# Educacion
table(wide12$edcepW,exclude=NULL)

wide12$edcepWB <- 0 # Basica o menos
wide12$edcepWB[wide12$edcepW>3 & wide12$edcepW<6]=1 # Media
wide12$edcepWB[wide12$edcepW>5 & wide12$edcepW<8]=2 # Superior no universitaria
wide12$edcepWB[wide12$edcepW>7]=3 # Superior universitaria
table(wide12$edcepW,wide12$edcepWB,exclude=NULL)

table(wide12$edcepWB,exclude=NULL)
prop.table(table(wide12$edcepWB,exclude=NULL))
prop.table(wtd.table(x = wide12$edcepWB,weights = wide12$weight))

# Estatus laboral
table(wide12$estlab_w1,exclude=NULL)

wide12$estlabB <- 0 # No trabaja
wide12$estlabB[wide12$estlab_w1<3]=1 # trabaja
table(wide12$estlab_w1,wide12$estlabB,exclude=NULL)

# Tener padres con altos niveles de educación
table(wide12$get_ah_1_w1, exclude=NULL) # 5 NAs
wide12$get_ah_1_w1a <- wide12$get_ah_1_w1
wide12$get_ah_1_w1a[is.na(wide12$get_ah_1_w1)]=mean(wide12$get_ah_1_w1,na.rm=T)
table(wide12$get_ah_1_w1a,exclude=NULL)

# Tener ambicion
table(wide12$get_ah_2_w1, exclude=NULL) # 5 NAs
wide12$get_ah_2_w1a <- wide12$get_ah_2_w1
wide12$get_ah_2_w1a[is.na(wide12$get_ah_2_w1)]=mean(wide12$get_ah_2_w1,na.rm=T)
table(wide12$get_ah_2_w1a,exclude=NULL)

# Izquierda y derecha
table(wide12$pospol_1_w1, exclude=NULL) # 54 NAs

wide12$ideo <- 0 # izquierda
wide12$ideo[wide12$pospol_1_w1==5]=1 # centro
wide12$ideo[wide12$pospol_1_w1>5]=2 # derecha
wide12$ideo[is.na(wide12$pospol_1_w1)]=3 # missing
table(wide12$pospol_1_w1,wide12$ideo,exclude=NULL)

# Comuna
# Hay comunas de netquest que no estan en CEP. 
# Ponderadores no considera comunas.

# sjPlot::view_df(x = as.data.frame(wide12$comuna_w1),max.len=100,show.prc = T)
# Regiones: Antofagasta,Valparaíso,Viña del Mar,Concepción,Talcahuano,
#           Arica,Iquique,Calama,Copiapó,La Serena,Coquimbo,Rancagua,
#           Curicó,Talca,Chillán,Temuco,Puerto Montt,Osorno,Coyhaique,Valdivia

# Santiago: Cerrillos,Cerro Navia,Conchalí,El Bosque,Estación Central,Huechuraba,
#           Independencia,La Cisterna,La Florida,La Granja,La Pintana,La Reina,Las Condes,
#           Lo Barnechea,Lo Espejo,Lo Prado,Macul,Maipú,Ñuñoa,Pedro Aguirre Cerda,Peñalolén,
#           Providencia,Pudahuel,Puente Alto,Quilicura,Quinta Normal,Recoleta,Renca,San Bernardo,
#           San Joaquín,San Miguel,San Ramón,Santiago,Vitacura

wide12$comuna_w1B <-NA # Nueva variable de comuna con loops, 1 Santiago, 0 regiones

for (i in 1065:1069){
  wide12$comuna_w1B[wide12$comuna_w1==i]=0 
}

for (i in 1070:1103){
  wide12$comuna_w1B[wide12$comuna_w1==i]=1 
}

for (i in 1104:1158){
  wide12$comuna_w1B[wide12$comuna_w1==i]=0 
}

table(wide12$comuna_w1,exclude=NULL)
table(wide12$comuna_w1B,exclude=NULL) # 1=1282 es Santiago, 0=400 en regiones, 99 NAs

wide12$comuna_w1C <- 0
wide12$comuna_w1C[wide12$comuna_w1B==1]=1
wide12$comuna_w1C[is.na(wide12$comuna_w1B)]=2
table(wide12$comuna_w1B,wide12$comuna_w1C,exclude=NULL)


# Percepcion pobreza
table(wide12$percep_pov_w1,exclude=NULL) # 5 NAs
prop.table(table(wide12$percep_pov_w1))

wide12$percep_pov2 <- 0 # Menor
wide12$percep_pov2[wide12$percep_pov_w1==2]=1 # Igual
wide12$percep_pov2[wide12$percep_pov_w1==1]=2 # Mayor
wide12$percep_pov2[is.na(wide12$percep_pov_w1)]=NA
table(wide12$percep_pov2,wide12$percep_pov_w1,exclude=NULL)

prop.table(table(wide12$percep_pov2)) # 37% menor, 20% Igual, 43% Mayor

# Variables get ahead de control
table(wide12$get_ah_1_w1,exclude=NULL) # get_ah_1_w1
prop.table(table(wide12$get_ah_1_w1,exclude=NULL)) # 22 NAs
wide12$get_ah_1_w1a <- wide12$get_ah_1_w1
wide12$get_ah_1_w1a[is.na(wide12$get_ah_1_w1)]=mean(wide12$get_ah_1_w1,na.rm=T)
table(wide12$get_ah_1_w1a,exclude=NULL)

table(wide12$get_ah_2_w1,exclude=NULL) # get_ah_1_w1
prop.table(table(wide12$get_ah_2_w1,exclude=NULL)) # 22 NAs
wide12$get_ah_2_w1a <- wide12$get_ah_2_w1
wide12$get_ah_2_w1a[is.na(wide12$get_ah_2_w1)]=mean(wide12$get_ah_2_w1,na.rm=T)
table(wide12$get_ah_2_w1a,exclude=NULL)

# -------------------- Analisis de ponderador ------------------ #
sum(wide12$weight) # 1781
summary(wide12$weight)
boxplot(wide12$weight)
table(wide12$weight,exclude=NULL)

library(survey)
surveyobject <- svydesign(ids = ~0, data = wide12, weights = wide12$weight) 

# Sexo
prop.table(table(wide12$sexoW,exclude=NULL))
svymean(~interaction(sexoW),surveyobject)

# Edad
prop.table(table(wide12$edadW,exclude=NULL))
svymean(~interaction(edadW),surveyobject)

# Educacion
table(wide12$edcepWB,exclude=NULL)
prop.table(table(wide12$edcepWB,exclude=NULL))

svymean(~interaction(edcepWB),surveyobject)

wide12 %>%
  group_by(edcepWB) %>%
  summarise(mean=mean(weight),min=min(weight),max=max(weight),n=n())

# Correlation for weights and outcomes
corr <- wide12 %>% dplyr::select(zofamilia,zhwork,zedstr,zed,zednew,SdvE,weight)
corr <- na.omit(corr)
cor(corr)













