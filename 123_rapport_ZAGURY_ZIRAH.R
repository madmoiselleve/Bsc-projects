#------------------------------Exercice 1----------------------------------------------------------
rm(list = ls())
set.seed(111)

#Préliminaires

# ---- Estimateur de MC classique
mc_estimator <- function (X, h = identity) {
  y <- h(X)
  s2 <- var(y)
  tol <- 1.96 * sqrt(s2 / length(y))
  return (list(mc_estim = mean(
    y), y = y, sigma2 = s2, mse = s2 / length(y),
               ic_inf = mean(y) - tol, ic_sup = mean(y) + tol, 
               tol = tol))
} 

# ---- Evolution de l'estimateur de MC classique
mc_estim_evol <- function(X, h = identity, level = 0.95) {
  y <- h(X)
  n <- length(y)
  delta_hat <- cumsum(y)/(1:n) 
  sd_hat <- (cumsum(y^2) - (1:n) * (delta_hat)^2) / (0:(n - 1))  # estimateur biaisé de la variance
  sd_2_hat <- c(0, sd_hat[-1]) / (1:n)
  e_delta <- sqrt(sd_2_hat) * 1.96
  
  return (data.frame (n = n, y = y, value = delta_hat, sigma = sd_hat, var = sd_2_hat,
                      ic_inf = delta_hat - e_delta, ic_sup <- delta_hat + e_delta,
                      e_delta = e_delta, level = level))
}

# ---- Fonction psi de l'énoncé
psi <- function(x, y){
  return((abs(sin((2 / pi) * x^2 - pi / 4)) + 4 * (cos(x))^2 + y^4) 
         * exp(-2 * (x + abs(y))) * (abs(x) <= pi/2) * (abs(y) <= 1))
}

# ---- Question 1 ---- 

# ---- Simulation de n réalisations de 2 v.a i.i.d de loi exponentielle translatée(2)
rgen_n_1 <- function(n){   
  x1 <- -pi/2 + rexp(n, 2)
  x2 <- -1 + rexp(n, 2)
  return (cbind(x1, x2))
}

# ---- Définition de la constante 
m1 <- ((10 + sqrt(2))/8) * exp(pi + 2)

# Ratio d'acceptation pour g définie comme ci-dessus
ratio_1 <- function(x){
  psi <- ((abs(sin(2 / pi) * x[,1]^2 - (pi / 4))) + 4 * (cos(x[,1]))^2 + x[,2]^4) * exp(-2 * (abs(x[,2]) - x[,2])) * (x[,1] <= (pi/2)) * (x[,2] <= 1)
  return(psi / ((10 + sqrt(2))/2))
}

# ---- Simulation de n réalisations du vecteur (x1,x2) de loi exp(2) translatée x Laplace(0,1/2)
rgen_n_2 <- function(n){
  u <- runif(n, -0.5, 0.5)
  x1 <- -pi/2 + rexp(n, 2)
  
  # Simulation de n v.a suivant la loi de Laplace(0,1/2) par la méthode de la fonction inverse
  x2 <- -0.5 * sign(u) * log(1 - 2 * abs(u))
  return(cbind(x1, x2))
}

# ---- Définition de la constante 
m2 <- ((10 + sqrt(2))/4) * exp(pi)

# Ratio d'acceptation pour g définie comme ci-dessus
ratio_2 <- function(x) {
  psi <- (abs(sin((2 / pi) * x[,1]^2 - (pi / 4))) + 4 * (cos(x[,1]))^2 + x[,2]^4) * (x[,1] <= pi/2) * (abs(x[,2]) <= 1)
  return(psi / ((10 + sqrt(2))/2))
}

# ---- Simulation de n réalisations du vecteur (x1,x2) de loi exponentielle concentrée x Laplace(0,1/2) concentrée sur (-1,1)
rgen_n_3 <- function(n){
  u1 <- runif(n)
  u2 <-  runif(n)
  x1 <- - log(exp(pi) - 2*sinh(pi)*u1)/2
  cst <- 1 - exp(-2)
  u <- (u2 < 0.5) 
  x2 <- 0.5 * (log(2*cst*u2 + exp(-2)) * u - (1-u) * log(1  - 2*cst*u2 + cst))
  return(cbind(x1, x2))
}

m3 <- (5 + (sqrt(2)/2)) * sinh(pi) * (1 - exp(-2))

ratio_3 <- function(x) {
  psi <- (abs(sin((2 / pi) * x[,1]^2 - pi / 4)) + 4 * (cos(x[,1]))^2 + x[,2]^4) 
  return(psi / (5 + (sqrt(2)/2)))
}

# ---- Simulation de n couples de v.a i.i.d de loi U([-pi/2, pi/2]) x Laplace(0,1/2)
rgen_n_4 <- function(n){
  x1 <- runif(n, -pi / 2, pi / 2)
  u <- runif(n, -0.5, 0.5)
  x2 <- -0.5 * sign(u) * log(1 - 2 * abs(u))
  return(cbind(x1, x2))
}

m4 <- (5 + (sqrt(2)/2)) * pi * exp(pi)

# Ratio d'acceptation pour g définie comme ci-dessus
ratio_4 <- function(x){
  psi <- (abs(sin((2 / pi) * x[,1]^2 - pi / 4)) + 4 * (cos(x[,1]))^2 + x[,2]^4) * exp(-2 * x[,1]) * (abs(x[,2]) <= 1)
  return(psi / ((5 + (sqrt(2)/2)) * exp(pi)))
}


#----- Alogrithme acceptation-rejet 
rgen_f <- function(n, rgen, ratio) { 
  ans <- c()
  rho <- c()
  count <- n
  freq_accept <- 1
  n_l <- 0  # nb moyen de passage ds la boucle while
  n_t <- 0  # nb moyen de simulations suivant g 
  T_a <- c() # temps d'atteinte de succès de l'algorithme 
  
  while (count > 0) {
    n_l <- n_l + 1
    nbsimu <- count %/% (freq_accept + (freq_accept == 0)) + 1 # Nb de simu suivant g nécessaires pour avoir count simulations suivant f
    x <- rgen(nbsimu)  
    n_t <- n_t + nbsimu # Nb de simu suivant g pour avoir nrow(ans) simu suivant f
    ratios <- ratio(x) # Evaluation du ratio d'acceptation
    selected <- which(runif(nbsimu) <= ratios)  # On n'accepte que les v.a tq u < rho(x)
    rho <- append(rho, ratios) 
    ans <- rbind(ans, x[selected, ]) 
    count <- n - nrow(ans) 
    freq_accept <- nrow(ans) / n_t
    T_a <- c(T_a, selected - c(0, selected[1:length(selected) - 1]))
  }
  
  return(list(value = ans[1:n, ], n_t = n_t, n_l = n_l, ratio = rho, T_a = T_a[1:n]))
}

# ---- Question 3 ----
n <- 10000

# Simulation de n va suivant f par la méthode du rejet avec 4 couples (m,g) différents
z1 <- rgen_f(n, rgen_n_1, ratio_1)
z1_value <- z1$value
z1_n_t <- z1$n_t
z1_n_l <- z1$n_l

z2 <- rgen_f(n, rgen_n_2, ratio_2)
z2_value <- z2$value
z2_n_t <- z2$n_t
z2_n_l <- z2$n_l

z3 <- rgen_f(n, rgen_n_3, ratio_3)
z3_value <- z3$value
z3_n_t <- z3$n_t
z3_n_l <- z3$n_l

z4 <- rgen_f(n, rgen_n_4, ratio_4)
z4_value <- z4$value
z4_n_t <- z4$n_t
z4_n_l <- z4$n_l

# ---- Question 4 ----
K = 100

# Calcul de l'estimateur bn_hat de a et de son intervalle de confiance associé 
b_hat <- function(n, m, ratio, level = 0.95) {
  x <- mc_estim_evol(ratio)
  rho_hat <- x$value # valeur de l'estimateur de MC classique de rho(X,Y) 
                      # où (X,Y) est simulé suivant g
  value <- 1 / (m * rho_hat)
  
  # Estimateur de l'écart type et de la variance
  sd_hat <- c(0, x$sigma[-1])
  b_var <- (m^2 * sd_hat * value^4) / n
  
  # Demi-longueur de l'intervalle de confiance
  tol <- 1.96 * sqrt(b_var)
  
  return(list(value = value, ic_inf = value - tol, ic_sup = value + tol, 
              tol = tol, s2 = b_var, rho_hat = x$y))
}

estim <- b_hat(z3_n_t, m3, z3$ratio)
b_hat_ <- estim$value[z3_n_t]
ic_b <- c(estim$ic_inf[z3_n_t], estim$ic_sup[z3_n_t])
b_var <- estim$s2[z3_n_t]

#----Estimation du biais de l'estimateur par bootstrap

bias_estim <- function(K, n, x) {
  # Echantillonage de n * K v.a dans x
  samp <- matrix(sample(x, n * K, TRUE), ncol = K)
  
  # Evaluation de l'estimateur
  ans <- 1 / colMeans(samp)
  return(mean(ans) - 1 / mean(x))
}

bias <-function(l, rho, m) {
  b_boot <-sapply(1:l,function(i) {
    mean(sample(rho, replace = TRUE))
    })
  return(mean(1/(m * b_boot)) - 1/(m *mean(rho)))
}

library(microbenchmark)
microbenchmark(bias_estim(K, n, estim$rho_hat * m3), bias(K, estim$rho_hat , m3))

bias_b_hat <- bias_estim(K, n, estim$rho_hat * m3)
(bias_u <-bias(K, estim$rho_hat , m3))
# ---- Question 5 ----
# Evaluation de l'estimateur sans biais an_hat de a et de l'IC(1-alpha) associé
T_hat <- mc_estim_evol(z3$T_a)
a_hat <- (1 / m3) * T_hat$value[n]
a_var <- a_hat * (a_hat * m3 - 1) / (n * m3)
tol <- 1.96 * sqrt(a_var)
ic_a <- c(a_hat - tol, a_hat + tol)

# Question 6
eps <- 0.01

# Calcul du nb de tirages suivant g nécessaires pour obtenir une précision de b_hat à eps près
# On démarre à 30 simulations pour que le TCL soit valide
tol_g <- estim$tol[30:z3_n_t]
(n_g <- which(tol_g < eps / 2)[1])

# Calcul du nb de tirages suivant f nécessaires pour obtenir une précision de a_hat à eps près
tol_f <- (1 / m3) * T_hat$e_delta[30:n]
(n_f <- which(tol_f < (eps / 2))[1])

# Rapport des coût
C1 <- n_g 
C2 <- n_f * (z3_n_t / n)
cost_ratio <- C1 / C2

# Rapport des variances
cost_var <- b_var / a_var

# Efficacité relative des 2 estimateurs
eff_rel <- n * b_var / (z3_n_t * a_var)

# ---- Question 7 ----

# ---- Fonction marginale de X estimée
f_hat <- function (x, a = a_hat){
  
  # Valeur de l'intégrale de psi par rapport à y 
  psi_x <- ((abs(sin((2 / pi) * x^2 - (pi / 4))) + 4 * (cos(x))^2) * (1 - exp(-2)) 
            + (3 - 21 * exp(-2))/2) * exp(-2 * x) * (abs(x) <= (pi/2))
  
  return(a * psi_x)
}

# Affichage de l'histogramme associée à la densité marginale de x estimée
x <- seq(from = -pi/2, to = pi/2, length.out = n)

hist(z3$value[,1], breaks = 100,
     freq=FALSE, col = "grey60", border = "grey70",
     main = "Distribution marginale de x", xlab= "Valeurs de x", ylab = "Fréquences", ylim = c(0,1)
)
lines(x, f_hat(x), col = "red", type = "l")


# ---- Question 10 ----
#On choisit omega suivant une loi exponentielle concentrée sur (-pi/2,pi/2)
omega = function (x){ exp(-2 * x) * (abs(x) <= (pi/2))/ sinh(pi) }

w = function(x) { mc_estimator( psi(rep(x,n),z3_value[,2]) * omega(z3_value[,1]) /psi(z3_value[,1],z3_value[,2])) }

v <- w(-1)
#On évalue w_hat(-1). La valeur obtenue est assez proche de celle pour f_hat(-1)
(w_hat <- v$mc_estim) 

#l'IC au niveau 95% de w_hat est de [ic_inf_w ; ic_sup_w]
ic_w <- c(v$ic_inf, v$ic_sup)

# ---- Question 11 ----

#L'estimateur de f défini plus haut est celui de a multiplié par une certaine expression que l'on calcule ci-dessous
expression <- f_hat(-1)/a_hat  
var_fx <- (a_var * expression^2) #on utilise le fait que var(fX)= value^2 * var(a)
var_w <- v$sigma2 # variance de w_hat

#on fixe le mse à celui de w_hat
mse <- var_fx/n

#Détermination du nombre de réalisations nécessaires pr obtenir 
n_fx <- floor(var_fx/mse)
n_w <- floor(var_w/mse)

R1 <- (n_w * C1) / (n_fx * C2)

# Affichage graphique de la densité de f théorique 
x <- seq(-(pi/2), pi/2, length.out = 100) 
y <- seq(-1, 1, length.out = 100)

f <- matrix(NA, length(x), length(y)) 

for(i in 1:length(x)){ 
  for(j in 1:length(y)){ 
    psi <- ((abs(sin(2 / pi) * x[i]^2 - (pi / 4))) + 4 * (cos(x[i]))^2 + y[j]^4) * exp(-2 * (x[i] - abs(y[j])))
    f[i,j] <- a_hat * psi  
  } 
} 

library(plot3D)
persp3D(x, y, f, phi = 20, theta = 35, main = "Densité théorique de f")


#--------------------------------------Exercice 2 -------------------------------------------------------------------

rm(list = ls())
#install.packages("microbenchmark") #enlever le hashtag si le package n'est pas installé
library(microbenchmark)  
set.seed(111)

#----- Preliminaire
mc_estimator <- function (X, h = identity) {
  y <- h(X)
  mean <- mean(y)
  s2 <- var(y)
  return (list( y = y, estimator = mean, sigma2 = s2, mse = s2 / length(y)))
}

mc_estim_evol <- function(y, level = 0.95) {
  n <- length(y)
  # Évolution de l'estimateur
  delta <- cumsum(y) / (1:n)
  # Évolution de l'estimateur de la variance
  s2 <- (cumsum(y^2) - (1:n) * delta^2) / (0:(n - 1))
  # Erreur quadratique moyenne
  mse <- c(0, s2[-1]) / (1:n)
  return(list( value = delta, sigma = s2, mse = mse ))
}


# Données de l'exo
n=10000
mu = c(0.1,0,0.1)
sigma <- matrix(0.01*c(4.7, 0, 1.17, 0, 4.7, 0, 1.17, 0, 4.7), nrow = 3)

h <- function (X) {
  n <- length(X[,1])
  return( n*(n<colMeans(exp(-X))) + colMeans(exp(-X))*(n>=colMeans(exp(-X))) )
} #Fonction dont l'intégrale est à estimer

# ---- Question 1 ----

rmvnorm <- function(n,mu,sigma) {
  z <- matrix(rnorm(length(mu) * n), nrow = length(mu))
  L <- t(chol(sigma))
  return (mu + L %*% z)
}

# ---- Question 2.b ----

X <-rmvnorm (n,mu,sigma)
mc <- mc_estimator(X, h)
mc_estim <- mc$estimator
mc_mse <- mc$mse

# ---- Question 3.b ----

ant <- mc_estimator((h(X)+ h(2*mu-X))/2)
ant_estim <- ant$estimator
ant_mse <- ant$mse

#Reduction de variance
R1 <- var(h(X)) / (var((h(X)+ h(2*mu-X))/ 2))  
#La variance de l'EMC est réduite de 97% + avec la méthode de la variable antithétique, et permets donc de diminuer fortement l'erreur (à cout de calcul =)

#Comparaison cout de calcul des 2 methodes
test_mc <- function() {
  x <- rmvnorm (1,mu,sigma)
  return(h(x))
}
test_ant <- function() {
  x <- rmvnorm (1,mu,sigma)
  return((h(X)+ h(2*mu-X))/ 2)
}

microbenchmark(test_mc(),test_ant()) #les 2 méthodes ont un cout de calcul médian équivalent 

# ---- Question 4 ----

h0 <- function (X) {
  return (colMeans(1 - X + X*X/2)) 
} 
m = (3-0.2+0.01+3*0.047/2)/3 #calcul explicite de l'Esperance de h0(X) 
ro <- cor(h0(X),h(X)) #corrélation entre hO(X) et h(X)

delta_hat = function (b) { mc_estimator(h(X)-b*(h0(X)-m)) } #implémentation de l'estimateur par la méthode variable de contrôle simple

# ---- Question 4.b ----

#On regarde d'abord le nombre de réalisations de f nécéssaire pour que la variance de delta_hat(b) converge - burn in method
vardelta_evol = function(b) { mc_estim_evol(h(X)-b*(h0(X)-m))$sigma }
erreur <- abs(vardelta_evol(1) - var(delta_hat(1)$y))
plot(1:n, erreur, type = 'l', main = 'Erreur de la variance "évolutive"', xlab='Nombre de simulations de f utilisées', col = "brown4", lwd = 2)
epsilon = 5*10^(-6)

lines(rep(epsilon, n), lty=2, col='firebrick')
legend("topright", c("Erreur","Seuil de convergence"), box.lty = 0, col = c("brown4","firebrick"), lty = c(1,2), lwd = c(2), inset = 0.05, bg='grey95')
nbsimu = 200 + which(erreur[200:n] < epsilon)[1]

#Détermination graphique du paramètre b qui minimise au mieux la variance de l'estimateur
reg <- seq(0,2,0.01)
var_delta = function(b) {var(delta_hat(b)$y[1:nbsimu])} #On utilise nbsimu simulations de f pour estimer b*
variance <- sapply(reg,var_delta) 
plot(reg, variance, type='l', main = 'Variance de delta_hat en fonction de b', xlab = 'b', col='darkblue') #tracé de la variance en fonction de b
#On remarque clairement que la variance de l'estimateur est minimale pour b autour de 1

#Détermination numérique du b optimal
b_opt = reg[which(variance == min(variance))] #donne le b qui minimise la variance sur [0,2]

#On utilise le reste des simulations de f pour estimer delta_hat
delta_hat_ = mc_estimator(h(X)[(nbsimu+1):n]-b_opt*(h0(X)[(nbsimu+1):n]-m))
delta_hat_opt <- delta_hat_$estimator
delta_hat_mse <- delta_hat_$mse

#--------------------------------------Exercice 3 -------------------------------------------------------------------
rm(list=ls())
set.seed(111)
library(microbenchmark)

# ---- Question 1 ----
# Renvoie l'estimateur de MC classique et l'erreur quadratique moyenne associée
mc_estim <- function (y, level = 0.95) {
  
  delta_hat <- mean(y)              # Estimateur de la moyenne empirique
  sigma_hat <- var(y) / length(y)    # Estimateur sans biais de l'écart-type
  
  return (data.frame(value = delta_hat, q_error = sigma_hat)) 
}

# Simulation de n va de loi Geom(p)
simu_geom <- function(n, p = 0.2){
  u <- runif(n)
  return(ceiling(log(u)/log(1 - p)))
}

S_function <- function(k, m = 2, theta = 2){
  return(sum(log(rgamma(k, m, theta) + 1)))
}

# ---- Déclaration des paramètres 
n <- 10000
p <- 0.2
m <- 2
theta <- 2

# ---- Simulation de la va S par la méthode de MC classique 
estim_mc <- function(n){
  
  # Simulation de n va suivant la loi de S 
  y <- sapply(rep(1, n), simu_geom)
  s <- sapply(y, S_function)
  
  # ---- Estimateur de Monte-Carlo et erreur quadratique moyenne associée
  result <- mc_estim(s)
  
  return(list(delta_hat = result$value, q_error = result$q_error))
}

res <- estim_mc(n)
delta_hat_mc <- res$delta_hat
q_error_mc <- res$q_error

# ---- Question 2 ----
# Nb de strates 
L <- 15

# Simulation de n va de loi S|Y >= L
simu_S_cond <- function(n, L){
  S <- c()
  count <- n
  
  while(count > 0){
    Y <- simu_geom(count, p)
    Y_cond <- Y[which (Y >= L)]
    
    # On teste s'il existe des éléments de Y qui vérifient la condition Y >= L 
    if (identical(Y_cond, numeric(0)) == TRUE){
      Y_cond <- 0 
    }
    
    # Simulations de v.a de loi S|Y >= L
    S <- append(S, sapply(Y_cond, S_function))
    count <- n - length(S)
  }
  
  return(S[1:n])
}

# Estimateur stratifié de delta
estim_strat <- function(n, L){

    #On traite de facon vectorielle les strates allant de 1 à L-1
  ppk <- p * (1 - p)^(0:L-2)
  pk <-ppk[3 : length(ppk)] #vecteur (p1, .. , pL-1)
  nk <- floor(n * pk) #vecteur (n1, .. , nL-1)
  rr<-rep(1:(L-1), nk)   
  y <- sapply(rr, S_function) #Vecteur de (n1+n2..+nL-1) réalisations de S
  d <- split(y, as.factor(rr) ) #On sépare le vecteur y en une liste [n1 réalisations, ..., nL-1 réalisations]
  delta_hat <- sum(pk * sapply(d, mean)) #implémentation de l'estimateur 
  q_error = function(d){var(y) / length(y)}
  est_mc <- sapply(d,q_error)
  sigma_sq <-  sum(pk * pk * est_mc)
  
  #On ajoute la dernière strate
  pL <- (1 - p)^(L-1)
  nL <- floor(n * pL)
  est_mcL <- mc_estim(simu_S_cond(nL, L))
  sigma_sq <- sigma_sq + pL * pL * est_mcL$q_error
  delta_hat <- delta_hat + pL * est_mcL$value
  
  return(list(value = delta_hat, q_error = sigma_sq))
}

result <- estim_strat(n, L)
delta_hat_strat <- result$value
q_error_strat <- result$q_error

# Rapport des variances des 2 estimateurs 
var_ratio <- q_error_mc / q_error_strat

library(microbenchmark)

# Calcul des coût des estimateurs
m <- microbenchmark(Estim_MC = estim_mc(n), Estim_strat = estim_strat(n, L)) 
C1 <- mean((m$expr == "Estim_MC") * m$time)
C2 <- mean((m$expr == "Estim_strat") * m$time)
cost_ratio <- C1 / C2

eff_rel <- cost_ratio * var_ratio 
