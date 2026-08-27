#Recode Tratamientos

library(haven)
library(dplyr)

pob_03 <- read_sav("exp_pob03.sav")

#Precio por respuesta -----

# - Gasto total en facebook = $86.364
# - N de respuestas en Qualtrics 789
 
94364/789 # $109.46 x observación total
94364/502 # $172.03 x observación completa

data <- pob_03 %>% filter(atencion==5) #502 obs
  

data$TRATAMIENTO <- NA
data$TRATAMIENTO[data$FL_4_DO_Control==1]                  <- 1 #control cigarros
data$TRATAMIENTO[data$FL_4_DO_Tratamiento    ==1]          <- 2 #pobreza
data$TRATAMIENTO[data$FL_4_DO_Tratamiento_desigualdad ==1] <- 3 #Desigualdad


data$POBREZA <- NA
data$POBREZA[data$FL_4_DO_Control==1]                      <- 0 #control cigarros
data$POBREZA[data$FL_4_DO_Tratamiento ==1]                 <- 1 #pobreza

data$DESIGUALDAD <- NA
data$DESIGUALDAD[data$FL_4_DO_Control==1]                  <- 0 #control cigarros
data$DESIGUALDAD[data$FL_4_DO_Tratamiento_desigualdad==1]  <- 1 #Desigualdad


data$TRATAMIENTO <- factor(x = data$TRATAMIENTO,levels = c(1,2,3),labels = c("Control", 
                                                                             "Tratamiento pobreza",
                                                                             "Tratamiento desigualdad"))
table(data$TRATAMIENTO)


# -99 y -999 == NA --------------------------------------------------------


for (i in 1:ncol(data)) {
data[,i][data[,i] == c(-99)]  <- NA #Missing 
data[,i][data[,i] == c(-999)] <- NA #Missing  
}

summary(data)

# Oportunidades individuales y estructurales ------------------------------

summary(data$get_ah_1) #WFAMILY
summary(data$get_ah_2) #WEDPARENTS

summary(data$get_ah_4) #ambicion
summary(data$get_ah_5) #trabajo duro

data$op_str <- (data$get_ah_1+data$get_ah_2)/2 #Oportunidad estructural
data$op_ind <- (data$get_ah_4+data$get_ah_5)/2 #Oportunidad individual

summary(data$op_str)
summary(data$op_ind)

#sjPlot::view_df(data[17:129],show.prc = TRUE,encoding = "Windows-1250", 
#                 file = "codebook_pob03.html") 
sjPlot::view_df(data[17:129],show.prc = TRUE,encoding = "UTF-8") 
# Test tratamientos -------------------------------------------------------


# Surgir en la vida -------------------------------------------------------

texreg::screenreg(l = list(
  lm(get_ah_1~TRATAMIENTO,data = data),  # familia rica 
  lm(get_ah_2~TRATAMIENTO,data = data),  # padres educados
  lm(get_ah_3~TRATAMIENTO,data = data),  # usted buen nivel educacion
  lm(get_ah_4~TRATAMIENTO,data = data),  # ambicion
  lm(get_ah_5~TRATAMIENTO,data = data))) # trabajo duro

texreg::screenreg(l=list(
  lm(get_ahB_1~TRATAMIENTO,data = data), # conocer personas adecuadas
  lm(get_ahB_2~TRATAMIENTO,data = data), # Contactos politicos
  lm(get_ahB_3~TRATAMIENTO,data = data), # coimas
  lm(get_ahB_4~TRATAMIENTO,data = data), # raza & etnia
  lm(get_ahB_5~TRATAMIENTO,data = data), # religion
  lm(get_ahB_6~TRATAMIENTO,data = data))) # sexo


# Oportunidades -----------------------------------------------------------

texreg::screenreg(l = list(lm(op_str~TRATAMIENTO,data = data),
                           lm(op_ind~TRATAMIENTO,data = data)))

texreg::screenreg(l = list(lm(op_str~POBREZA,data = data),
                           lm(op_ind~POBREZA,data = data)))

texreg::screenreg(l = list(lm(op_str~DESIGUALDAD,data = data),
                           lm(op_ind~DESIGUALDAD,data = data)))


# Redistribución taxes ----------------------------------------------------

texreg::screenreg(l=list(
  lm(pref_redis_taxB_1~TRATAMIENTO,data = data),
  lm(pref_redis_taxB_2~TRATAMIENTO,data = data),
  lm(pref_redis_taxB_3~TRATAMIENTO,data = data),
  lm(pref_redis_taxB_4~TRATAMIENTO,data = data)))


# Escala de Preferencias / percepción meritocracia ------------------------

texreg::screenreg(l=list(
  lm(merit_perc_pref_1~TRATAMIENTO,data = data), 
  lm(merit_perc_pref_2~TRATAMIENTO,data = data),
  lm(merit_perc_pref_3~TRATAMIENTO,data = data),
  lm(merit_perc_pref_4~TRATAMIENTO,data = data),
  lm(merit_perc_pref_5~TRATAMIENTO,data = data)))

texreg::screenreg(l=list(
  lm(merit_perc_pref_13~TRATAMIENTO,data = data), 
  lm(merit_perc_pref_14~TRATAMIENTO,data = data),
  lm(merit_perc_pref_15~TRATAMIENTO,data = data)
  ))

summary(lm(merit_perc_scale ~TRATAMIENTO, data = data)) # Escalas 1 a 7
summary(lm(merit_pref_scale ~TRATAMIENTO, data = data)) # Escalas 1 a 7
summary(lm(merit_own_scale  ~TRATAMIENTO, data = data)) # Escalas 1 a 7
