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


# A mano
#############
# Funcion para estratificar
stratify = function( wts, K ) {
  cut( wts, breaks=quantile(wts,(0:K)/K), include.lowest=TRUE )
}

# Creacion de estratos
wide12b$strats<-stratify(wide12b$weight,K=13)
table(wide12b$strats,exclude=NULL)
table(wide12b$strats,wide12b$edcepWB,exclude=NULL)

wide12b$strats <- as.numeric(wide12b$strats)

baseX <- wide12b %>% group_by(strats) %>% 
  summarise(nb=sum(weight))

# Estimacion
baseX <- wide12b %>% group_by(strats) %>% 
  summarise(Yw=weighted.mean(zedstr2[tratamiento=="pobreza"],weight[tratamiento=="pobreza"]) - weighted.mean(zedstr2[tratamiento=="control"],weight[tratamiento=="control"]),
            Y=mean(zedstr2[tratamiento=="pobreza"]) - mean(zedstr2[tratamiento=="control"]),
            nb=n())

with(baseX, sum(Yw*nb/sum(nb)))
with(baseX, sum(Y*nb/sum(nb)))


# Matching
##############
library(quickmatch)

# Ejemplo
############
# Construct example data
my_data <- data.frame(y = rnorm(100),
                      x1 = runif(100),
                      x2 = runif(100),
                      treatment = factor(sample(rep(c("T1", "T2", "C"), c(25, 25, 50)))))

# Treatment group averages in unmatched sample
covariate_averages(my_data$treatment, my_data[c("x1", "x2")])

# Make distances
my_distances <- distances(my_data, dist_variables = c("x1", "x2"))

# Make matching with one unit from "T1", "T2" and "C" in each matched group
my_data$m1 <- quickmatch(my_distances, my_data$treatment,size_constraint = 6)
table(my_data$treatment,my_data$m1)

# Base wide12b
#####################
covariate_averages(wide12b$tratamiento,wide12b$weight)

my_distances <- distances(wide12b, dist_variables = c("weight"))

match1 <- quickmatch(my_distances, wide12b$tratamiento,size_constraint = 61)
wide12b$match1 <- quickmatch(my_distances, wide12b$tratamiento,size_constraint = 61)

wide12b$match1[wide12b$match1==0]=17
wide12b$match1 <- as.numeric(wide12b$match1)
table(wide12b$match1) # 17 grupos
table(wide12b$tratamiento,wide12b$match1) # 17 grupos

# Balance
covariate_balance(wide12b$tratamiento,wide12b$weight,all_differences=T)
covariate_balance(wide12b$tratamiento,wide12b$weight,all_differences=T,m1)

# ATE 1
lm_match(wide12b$zedstr2,
         wide12b$tratamiento,
         match1)

# ATE 2
m1 <- lm(zedstr2~tratamiento,weights=weight,data=wide12b,subset=match1==1)
m1$coefficients[2:3]  

par.est <- matrix(NA,nrow=17,ncol=2)

for(i in 1:17) {
  m1 <- lm(zedstr2~tratamiento,weights=weight,data=wide12b,subset=match1==i) 
  par.est[i,1] <- m1$coef[2] # ATE pobreza
  par.est[i,2] <- m1$coef[3] # ATE desigualdad
}

par.est   
par.est <- as.data.frame(par.est)
par.est$match1 <- rep(1:17)

baseX <- wide12b %>% group_by(match1) %>% 
  summarise(Yp=weighted.mean(zedstr2[tratamiento=="pobreza"],weight[tratamiento=="pobreza"]) - weighted.mean(zedstr2[tratamiento=="control"],weight[tratamiento=="control"]),
            nb=n())


baseX <- merge(baseX,par.est, by="match1")  

with(baseX, sum(Yp*nb/sum(nb)))

# Solo control y pobreza
wide12c <- filter(wide12b, tratamiento!="desigual") 
dim(wide12c)

baseX <- wide12c %>% group_by(match1) %>% 
  summarise(Yp=weighted.mean(zedstr2[tratamiento=="pobreza"],weight[tratamiento=="pobreza"]) - weighted.mean(zedstr2[tratamiento=="control"],weight[tratamiento=="control"]),
            nb=n())

with(baseX, sum(Yp*nb/sum(nb)))

# Bootstraping
###################

# Funcion 1
ate.ps.hh1 = function(Y,D,strata,w,base,K) {
  
  par.est <- matrix(K,nrow=K,ncol=1)
  
  for(i in 1:K) {
    m1 <- lm(Y~D,weights=w,data=base,subset=strata==i) 
    par.est[i,1] <- m1$coef[2] # ATE pobreza
  }
  
  par.est <- as.data.frame(par.est)
  
  n=length(Y)
  
  tau.ps <- with(par.est, sum(V1*table(strata)/n))
  tau.ps               
}

ate.ps.hh1(wide12b$zedstr2,wide12b$tratamiento,wide12b$match1,wide12b$weight,wide12b,17)
ate.ps.hh1(wide12b$zedstr2,wide12b$tratamiento,wide12b$strats,wide12b$weight,wide12b,13)

# Funcion 2
ate.ps.hh2 = function(Y,D,w,base,N) {
  
  base$stratsX<-stratify(base$w,K=N)
  base$stratsX <- as.numeric(base$stratsX)
  
  par.est <- matrix(N,nrow=N,ncol=1)
  
  for(i in 1:N) {
    m1 <- lm(Y~D,weights=w,data=base,subset=stratsX==i) 
    par.est[i,1] <- m1$coef[2] # ATE pobreza
  }
  
  par.est <- as.data.frame(par.est)
  
  n=length(Y)
  
  tau.ps <- with(par.est, sum(V1*table(base$stratsX)/n))
  tau.ps      
}

ate.ps.hh2(wide12b$zedstr2,wide12b$tratamiento,wide12b$weight,wide12b,13)

# Funcion 3
ate.ps.hh3 = function(Y,D,w,base,N) {
  
  base$stratsX<-stratify(base$w,K=N)
  base$stratsX <- as.numeric(base$stratsX)
  
  par.est <- matrix(N,nrow=N,ncol=1)
  
  for(i in 1:N) {
    m1 <- lm(Y~D+base$sexoW1+base$edadW+
               base$edcepWB1+base$edcepWB2+base$edcepWB3+base$estlabB+
               base$ingresosQ2+base$ingresosQ3+base$ingresosQ4+base$ingresosQ5+
               base$comuna_w1C1+base$comuna_w1C2+base$get_ah_1_w1+base$get_ah_2_w1+
               base$ideo1+base$ideo2+base$ideo3,weights=w,data=base,subset=stratsX==i) 
    par.est[i,1] <- m1$coef[2] # ATE pobreza
  }
  
  par.est <- as.data.frame(par.est)
  
  n=length(Y)
  
  tau.ps <- with(par.est, sum(V1*table(base$stratsX)/n))
  tau.ps      
}

ate.ps.hh3(wide12b$zedstr2,wide12b$tratamiento,wide12b$weight,wide12b,13)
ate.ps.hh3(wide12b$zofamilia2,wide12b$tratamiento,wide12b$weight,wide12b,13)

# Funcion 4
ate.ps.hh4 = function(Y,D,X=NULL,w,base,N) {
  
  base$stratsX<-stratify(base$w,K=N)
  base$stratsX <- as.numeric(base$stratsX)
  
  par.est <- matrix(N,nrow=N,ncol=1)
  
  # Without covariates
  if (is.null(X)) {
    for(i in 1:N) {
      m1 <- lm(Y~D,weights=w,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[2] # ATE pobreza
    }
  }
  
  # With covariates
  if (!is.null(X)) {
    for(i in 1:N) {
      m1 <- lm(Y~D+X,weights=w,data=base,subset=stratsX==i) 
      par.est[i,1] <- m1$coef[2] # ATE pobreza
    }
  }
  
  par.est <- as.data.frame(par.est)
  
  n=length(Y)
  
  ate.ps <- with(par.est, sum(V1*table(base$stratsX)/n))
  
  c(ate.ps=ate.ps)      
}

ate.ps.hh4(wide12b$zedstr2,wide12b$tratamiento,X=NULL,wide12b$weight,wide12b,13)
ate.ps.hh4(wide12b$zedstr2,wide12b$tratamiento,X=covar,wide12b$weight,wide12b,13)
ate.ps.hh4(wide12b$zofamilia2,wide12b$tratamiento,X=covar,wide12b$weight,wide12b,13)

# Funcion 5
ate.ps.hh5 = function(Y,D,X=NULL,w,base,N,weight=TRUE) {
  
  base$stratsX<-stratify(base$w,K=N)
  base$stratsX <- as.numeric(base$stratsX)
  
  par.est <- matrix(N,nrow=N,ncol=1)
  
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
  
  n=length(Y)
  
  ate.ps <- with(par.est, sum(V1*table(base$stratsX)/n))
  
  c(ate.ps=ate.ps)      
}

ate.ps.hh5(wide12b$zedstr2,wide12b$tratamiento,X=NULL,wide12b$weight,wide12b,13,weight=T)
ate.ps.hh5(wide12b$zedstr2,wide12b$tratamiento,X=NULL,wide12b$weight,wide12b,13,weight=F)
ate.ps.hh5(wide12b$zedstr2,wide12b$tratamiento,X=covar,wide12b$weight,wide12b,13,weight=T)
ate.ps.hh5(wide12b$zedstr2,wide12b$tratamiento,X=covar,wide12b$weight,wide12b,13,weight=F)

# Bootstrap SE
###########################
# Covariates
wide12b1 <- dplyr::select(wide12b,sexoW1,edadW,edcepWB1,
                          edcepWB2,edcepWB3,estlabB,
                          ingresosQ2,ingresosQ3,ingresosQ4,ingresosQ5,
                          comuna_w1C1,comuna_w1C2,get_ah_1_w1,get_ah_2_w1,
                          ideo1,ideo2,ideo3) 

covar <- as.matrix(wide12b1)

# zofamilia2
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

# zedstr2
SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh4(edat.star$zedstr2,edat.star$tratamiento,X=NULL,edat.star$weight,edat.star,9)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)

SErep = replicate( 1000, {
  
  edat.star = sample(nrow(wide12b), replace=TRUE)
  edat.star = wide12b[edat.star,]
  ate.ps.hh4(edat.star$zedstr2,edat.star$tratamiento,X=covar,edat.star$weight,edat.star,9)
  
} )

rs  = melt( SErep )
mean(rs$value, na.rm=TRUE)
sd(rs$value, na.rm=TRUE )
quantile(rs$value,.025)
quantile(rs$value,.975)
