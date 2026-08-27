
# Dos bloques, probabilidades distintas
ID <-c(1,2,3,4,5,6)
X <- c(0,0,1,1,1,1)
D <- c(0,1,0,1,1,1)
Y <- c(25,50,10,65,70,69)

base <- cbind(ID,X,D,Y)
base <- as.data.frame(base)

base$pr1 <- c(0.5,0.5,0.25,0.75,0.75,0.75)
base$w <- with(base, 1/pr1)

# Dos bloques: pr=50
ID <-c(1,2,3,4,5,6)
D <- c(0,0,0,1,1,1)
Y <- c(25,50,10,65,70,69)

base <- cbind(ID,D,Y)
base <- as.data.frame(base)

base$pr1 <- c(0.5,0.5,0.5,0.5,0.5,0.5)

base$w <- with(base, 1/pr1)


# Parametros
######################
w = base$w
u = Y*w
Z = sum(w)
n = length(Y)
n1 = sum(D)
v = u * n / Z
Tx = D == 1 

# Diferencias de medias
mean( Y[Tx] ) - mean( Y[!Tx] ) 

# HT
(sum(Y[D==1]*w[D==1]) - sum(Y[D==0]*w[D==0]))/length(Y)

# double-hajek
Z1 = sum(w[Tx])
Z0 = sum(w[!Tx])
sum(u[Tx])/Z1 - sum(u[!Tx]) /Z0


# Post-stratification
##############################

## Funcion 1: Calculate the strata given a vector of realized weights
stratify = function( wts, K ) {
	cut( wts, breaks=quantile(wts,(0:K)/K), include.lowest=TRUE )
}

strats = stratify( base$w, K=2 )
base$strats = stratify( base$w, K=2 )

# calc the self-weighted and post-stratified treatment
# effect estimators given outcome variable, Tx, and b
calc.tau.ps = function( Y, Tx, b ) {
	stopifnot( !is.null(b) & !is.null(Tx) & !is.null(Y))
	n = length(Y)

	mns = tapply( Y, list( b, Tx ), mean )
	tx <- sum( (mns[,2] - mns[,1]) * table(b)/n )

	tx
}

tau.ps = calc.tau.ps( u, Tx, strats )

mns= tapply( u, list( strats, Tx ), mean )
sum( (mns[,2] - mns[,1]) * table(strats)/n )





