###################################
# Fecha: Mayo 2020                #
# Topico: postestratificacion     #
###################################

# Base: wide12

# Estandarizacion de Glass
glass_delta <- function(Y, Z, name = "control"){
  Y/sd(Y[Z == name], na.rm = TRUE)
}

wide12$zofamilia2 <- glass_delta(Y=wide12$get_ah_1_w2,wide12$tratamiento)                
wide12$zhwork2 <- glass_delta(Y=wide12$get_ah_5_w2,wide12$tratamiento)
wide12$zedstr2 <- glass_delta(Y=wide12$get_ah_estr_w2,wide12$tratamiento)
wide12$zed2 <- glass_delta(Y=wide12$get_ah_3_w2,wide12$tratamiento)
wide12$zednew2 <- glass_delta(Y=wide12$get_ah_indv_w2,wide12$tratamiento)
wide12$zdvE2 <- glass_delta(Y=wide12$dvE,wide12$tratamiento)


# ---------------- Post-stratification ------------------ #

# Procesamiento de datos
#############################
wide12b <- wide12 %>% dplyr::select(zofamilia2,zhwork2,zedstr2,zed2,zednew2,zdvE2,
                                    tratamiento,sexoW,edadW,edcepWB,
                                    estlabB,ingresosQ,comuna_w1C,
                                    get_ah_1_w1,get_ah_2_w1,ideo,weight)                                  

wide12b <- na.omit(wide12b)
dim(wide12b)

# Generacion de dummies
table(wide12b$sexoW,exclude=NULL)
wide12b$sexoW1 <- 0
wide12b$sexoW1[wide12b$sexoW==2]=1
table(wide12b$sexoW,wide12b$sexoW1,exclude=NULL)

table(wide12b$edcepWB,exclude=NULL)
wide12b$edcepWB1 <- 0
wide12b$edcepWB1[wide12b$edcepWB==1]=1
table(wide12b$edcepWB,wide12b$edcepWB1,exclude=NULL)
wide12b$edcepWB2 <- 0
wide12b$edcepWB2[wide12b$edcepWB==2]=1
table(wide12b$edcepWB,wide12b$edcepWB2,exclude=NULL)
wide12b$edcepWB3 <- 0
wide12b$edcepWB3[wide12b$edcepWB==3]=1
table(wide12b$edcepWB,wide12b$edcepWB3,exclude=NULL)

table(wide12b$estlabB,exclude=NULL)

table(wide12b$ingresosQ,exclude=NULL)
wide12b$ingresosQ2 <- 0
wide12b$ingresosQ2[wide12b$ingresosQ==2]=1
table(wide12b$ingresosQ,wide12b$ingresosQ2,exclude=NULL)
wide12b$ingresosQ3 <- 0
wide12b$ingresosQ3[wide12b$ingresosQ==3]=1
table(wide12b$ingresosQ,wide12b$ingresosQ3,exclude=NULL)
wide12b$ingresosQ4 <- 0
wide12b$ingresosQ4[wide12b$ingresosQ==4]=1
table(wide12b$ingresosQ,wide12b$ingresosQ4,exclude=NULL)
wide12b$ingresosQ5 <- 0
wide12b$ingresosQ5[wide12b$ingresosQ==5]=1
table(wide12b$ingresosQ,wide12b$ingresosQ5,exclude=NULL)

table(wide12b$comuna_w1C,exclude=NULL)
wide12b$comuna_w1C1 <- 0
wide12b$comuna_w1C1[wide12b$comuna_w1C==1]=1
table(wide12b$comuna_w1C,wide12b$comuna_w1C1,exclude=NULL)
wide12b$comuna_w1C2 <- 0
wide12b$comuna_w1C2[wide12b$comuna_w1C==2]=1
table(wide12b$comuna_w1C,wide12b$comuna_w1C2,exclude=NULL)

table(wide12b$ideo,exclude=NULL)
wide12b$ideo1 <- 0
wide12b$ideo1[wide12b$ideo==1]=1
table(wide12b$ideo,wide12b$ideo1,exclude=NULL)
wide12b$ideo2 <- 0
wide12b$ideo2[wide12b$ideo==2]=1
table(wide12b$ideo,wide12b$ideo2,exclude=NULL)
wide12b$ideo3 <- 0
wide12b$ideo3[wide12b$ideo==3]=1
table(wide12b$ideo,wide12b$ideo3,exclude=NULL)

# Funciones
###################

# Funcion para estratificar
stratify = function( wts, K ) {
  cut( wts, breaks=quantile(wts,(0:K)/K), include.lowest=TRUE )
}

wide12b$strats<-stratify(wide12b$weight,K=13)
table(wide12b$strats,exclude=NULL)
table(wide12b$strats,wide12b$edcepWB,exclude=NULL)

wide12b$strats <- as.numeric(wide12b$strats)
wide12b$strats <- NULL


# Funcion 1
ate.ps.hh1 = function(Y,D,strata,w,base,K) {
  
  par.est <- matrix(NA,nrow=K,ncol=1)
  
  for(i in 1:K) {
    m1 <- lm(Y~D,weights=w,data=base,subset=strata==i) 
    par.est[i,1] <- m1$coef[2] # ATE pobreza
  }
  
  par.est <- as.data.frame(par.est)
  
  Z=sum(w) # Z
  
  Zk <- matrix(NA,nrow=K,ncol=1)
  for(i in 1:K) {
    Zk[i,1]  <- sum(w[strata==i])
  }
  Zk <- as.data.frame(Zk)
  
  tau.ps <- sum(par.est$V1*Zk$V1/Z)
  
  tau.ps               
}

ate.ps.hh1(wide12b$zedstr2,wide12b$tratamiento,wide12b$strats,wide12b$weight,wide12b,13)

# Funcion 2: hace estratos automaticamente
ate.ps.hh2 = function(Y,D,w,base,N) {
  
  base$stratsX<-stratify(base$w,K=N)
  base$stratsX <- as.numeric(base$stratsX)
  
  par.est <- matrix(N,nrow=N,ncol=1)
  
  for(i in 1:N) {
    m1 <- lm(Y~D,weights=w,data=base,subset=stratsX==i) 
    par.est[i,1] <- m1$coef[2] # ATE pobreza
  }
  
  par.est <- as.data.frame(par.est)
  
  Z=sum(w) # Z
  
  Zk <- matrix(NA,nrow=N,ncol=1)
  for(i in 1:N) {
    Zk[i,1]  <- sum(w[base$stratsX==i])
  }
  Zk <- as.data.frame(Zk)
  
  tau.ps <- sum(par.est$V1*Zk$V1/Z)
  tau.ps      
}

ate.ps.hh2(wide12b$zedstr2,wide12b$tratamiento,wide12b$weight,wide12b,13)


# Funcion 3: tratamiento pobreza
ate.ps.hh5 = function(Y,D,X=NULL,w,base,N,weight=TRUE) {
  
  base$stratsX<-stratify(base$w,K=N)
  base$stratsX <- as.numeric(base$stratsX)
  
  par.est <- matrix(NA,nrow=N,ncol=1)
  
  # Without covariates but weights
  if (is.null(X) & weight==TRUE) {
    for(i in 1:N) {
      m1 <- lm(Y~D,weights=w,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[2] # ATE pobreza
    }
  }
  
  # Without covariates and weights
  if (is.null(X) & weight==FALSE) {
    for(i in 1:N) {
      m1 <- lm(Y~D,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[2] # ATE pobreza
    }
  }
  
  # With covariates and weights
  if (!is.null(X) & weight==TRUE) {
    for(i in 1:N) {
      m1 <- lm(Y~D+X,weights=w,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[2] # ATE pobreza
    }
  }
  
  # With covariates but without weights
  if (!is.null(X) & weight==FALSE) {
    for(i in 1:N) {
      m1 <- lm(Y~D+X,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[2] # ATE pobreza
    }
  }
  
  par.est <- as.data.frame(par.est)
  
  Z=sum(w) # Z
  
  Zk <- matrix(NA,nrow=N,ncol=1)
  for(i in 1:N) {
    Zk[i,1]  <- sum(w[base$stratsX==i])
  }
  Zk <- as.data.frame(Zk)
  
  ate.ps <- sum(par.est$V1*Zk$V1/Z)
  
  c(ate.ps=ate.ps)      
}

ate.ps.hh5(wide12b$zedstr2,wide12b$tratamiento,X=NULL,wide12b$weight,wide12b,13,weight=T)
ate.ps.hh5(wide12b$zedstr2,wide12b$tratamiento,X=NULL,wide12b$weight,wide12b,13,weight=F)
ate.ps.hh5(wide12b$zedstr2,wide12b$tratamiento,X=covar,wide12b$weight,wide12b,13,weight=T)
ate.ps.hh5(wide12b$zedstr2,wide12b$tratamiento,X=covar,wide12b$weight,wide12b,13,weight=F)

# Funcion 4: tratamiento desigualdad
ate.ps.hh6 = function(Y,D,X=NULL,w,base,N,weight=TRUE) {
  
  base$stratsX<-stratify(base$w,K=N)
  base$stratsX <- as.numeric(base$stratsX)
  
  par.est <- matrix(N,nrow=N,ncol=1)
  
  # Without covariates but weights
  if (is.null(X) & weight==TRUE) {
    for(i in 1:N) {
      m1 <- lm(Y~D,weights=w,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[3] # ATE pobreza
    }
  }
  
  # Without covariates and weights
  if (is.null(X) & weight==FALSE) {
    for(i in 1:N) {
      m1 <- lm(Y~D,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[3] # ATE pobreza
    }
  }
  
  # With covariates and weights
  if (!is.null(X) & weight==TRUE) {
    for(i in 1:N) {
      m1 <- lm(Y~D+X,weights=w,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[3] # ATE pobreza
    }
  }
  
  # With covariates but without weights
  if (!is.null(X) & weight==FALSE) {
    for(i in 1:N) {
      m1 <- lm(Y~D+X,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[3] # ATE pobreza
    }
  }
  
  par.est <- as.data.frame(par.est)
  
  Z=sum(w) # Z
  
  Zk <- matrix(NA,nrow=N,ncol=1)
  for(i in 1:N) {
    Zk[i,1]  <- sum(w[base$stratsX==i])
  }
  Zk <- as.data.frame(Zk)
  
  ate.ps <- sum(par.est$V1*Zk$V1/Z)
  
  c(ate.ps=ate.ps)      
}

ate.ps.hh6(wide12b$zedstr2,wide12b$tratamiento,X=NULL,wide12b$weight,wide12b,13,weight=T)
ate.ps.hh6(wide12b$zedstr2,wide12b$tratamiento,X=NULL,wide12b$weight,wide12b,13,weight=F)
ate.ps.hh6(wide12b$zedstr2,wide12b$tratamiento,X=covar,wide12b$weight,wide12b,13,weight=T)
ate.ps.hh6(wide12b$zedstr2,wide12b$tratamiento,X=covar,wide12b$weight,wide12b,13,weight=F)

# --------------------- Estimaciones --------------------- #

# Covariates
wide12b1 <- dplyr::select(wide12b,sexoW1,edadW,edcepWB1,
                          edcepWB2,edcepWB3,estlabB,
                          ingresosQ2,ingresosQ3,ingresosQ4,ingresosQ5,
                          comuna_w1C1,comuna_w1C2,get_ah_1_w1,get_ah_2_w1,
                          ideo1,ideo2,ideo3) 

covar <- as.matrix(wide12b1)

# Origen familiar: zofamilia2
################################

# Pobreza
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zofamilia2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

library(reshape2)
rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zofamilia2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Desigualdad
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zofamilia2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zofamilia2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Trabajo duro: zhwork2
##########################

# Pobreza
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zhwork2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zhwork2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Desigualdad
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zhwork2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zhwork2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Haber accedido a una educación de calidad : zedstr2
#######################################################

# Pobreza
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zedstr2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zedstr2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Desigualdad
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zedstr2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zedstr2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Haber alcanzado un buen nivel de educación: zed2 
######################################################

# Pobreza
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zed2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zed2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Desigualdad
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zed2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zed2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Que usted tenga un buen nivel de educación: zednew2 
#######################################################

# Pobreza
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zednew2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zednew2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Desigualdad
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zednew2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zednew2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Indice educacion: zdvE2 
#############################

# Pobreza
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zdvE2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh5(edat.star$zdvE2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

# Desigualdad
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zdvE2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=F)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh6(edat.star$zdvE2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9,weight=T)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)











