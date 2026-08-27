###################################
# Fecha: Septiembre 2020          #
# Topico: Manipulation Checks     #
###################################

# Ejecutar prep-covariables-LM.R, Ingreso1.R, Igualitarismo2.R, analisis2-LM.R

# -----------------------Drop people that fails------------------- #
wide12b <- dplyr::filter(wide12, manip>0)
table(wide12b$manip,exclude=NULL)

# Origen
###########
m1a <- lm_robust(zofamilia2~factor(tratamiento),data=wide12b)

m1b <- lm_robust(zofamilia2~factor(tratamiento),data=wide12b)

m1c <- lm_robust(zofamilia2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12b)

screenreg(l=list(m1a,m1b,m1c), single.row = TRUE, stars = c(0.01, 0.05))

# Trabajo duro
######################
m2a <- lm_robust(zhwork2~factor(tratamiento),data=wide12b)

m2b <- lm_robust(zhwork2~factor(tratamiento),data=wide12b)

m2c <- lm_robust(zhwork2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12b)

screenreg(l=list(m2a,m2b,m2c), single.row = TRUE, stars = c(0.01, 0.05))

# Indice educacion
#####################
m3a <- lm_robust(zdvE2~factor(tratamiento),data=wide12b)

m3b <- lm_robust(zdvE2~factor(tratamiento),data=wide12b)

m3c <- lm_robust(zdvE2~factor(tratamiento)+
                   factor(sexoW)+edadW+factor(edcepWB)+factor(estlabB)+
                   ingresosL+factor(comuna_w1C)+
                   get_ah_1_w1a+get_ah_2_w1a+factor(ideo)+irt_z1+factor(percep_pov2),
                   data=wide12b)

screenreg(l=list(m3a,m3b,m3c), single.row = TRUE, stars = c(0.01, 0.05))

