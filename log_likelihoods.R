require("stats4")
require("VGAM")
# Log-likelihood functions
# They are implemented according to table 2
# of the statement

# These variables are intended to be
# interpreted as constants on the 
# to-maximize functions
N <- 100
M <- 100
C <- 12939
Mprime <- 123912

deg_pref = read.csv("degseq_pref.csv", header = FALSE)
deg_rand = read.csv("degseq_random.csv", header = FALSE)


# Given a degree distribution, set all the
# variables (constants) to their proper values
set_constants <- function(degree_sequence) {
  N <<- length(degree_sequence)
  M <<- sum(degree_sequence)
  # TODO: This can be computed in nlogn by sorting
  # and binary searching the frequencies of the js
  C <<- sum(
    as.numeric(
      Map(
        function (x) sum(log(2:x)),
        degree_sequence
      )
    )
  )
  Mprime <<- sum(
    log(degree_sequence)
  )
}

# Log-likelihood functions
# They have + sign here
displaced_poisson <- function(lambda) {
  M*log(lambda) - N*(lambda + log(1 - exp(-lambda))) - C
}

displaced_geometric <- function(q) {
  (M - N)*log(1.0 - q) + N*log(q)
}

zeta_function_gamma_two <- function() {
  -3.0*Mprime - N*log(zeta(3.0))
}

zeta_function <- function(gamma) {
  -gamma*Mprime - N*log(zeta(gamma))
}

harmonic <- function(kmax, gamma) {
  sum = 0.0
  for(i in 1:ceiling(kmax)) {
    sum <- sum + i^-gamma
  }
  return(sum)
}

right_truncated_zeta <- function(kmax, gamma) {
  -gamma*Mprime - N*log(harmonic(kmax, gamma))
}

get_AIC <- function(m2logL, K, N, name) {
  c(m2logL + (2.0*K*N)/(N-K-1.0), name)
}

zeta_prob <- function(k) {
  k^-3.0 / zeta(3.0);
}

approximation_function <- function(k) {
  ((2.0 * 25.0 * 99990.0) / (100000.0)) * k^-3.0
}

quadratic_f <- function(x) {
  x*x
}

# A function that given a degree sequence
# computes and returns a list of pairs
# (AIC_1, name_1), (AIC_2, name_2), ..., (AIC_5, name_5)
# with the AIC and the name of the log likelihood function
# that caused this AIC. See above what functions are being
# considered here (or the functions array declaration)
# inside the code
get_aics <- function(degree_sequence) {
  # All these functions have constants, we are going
  # to set them before doing nothing else
  set_constants(degree_sequence)
  # MLE needs the negation of these functions
  functions <- c(
                 function(lambda) -displaced_poisson(lambda),
                 function(q) -displaced_geometric(q),
                 function()  -zeta_function_gamma_two(),
                 function(gamma) -zeta_function(gamma),
                 function(kmax, gamma) -right_truncated_zeta(kmax, gamma)
                 )
  # Start parameters for MLE
  start_parameters <- c(
    list(lambda = M/N),
    list(q = N/M),
    list(a = 1),
    list(gamma = 3.0),
    list(kmax = N, gamma = 3.0)
  )
  # Lower bounds for each function
  lower_bounds <- c(
    c(0.01),
    c(0.01),
    c(-Inf),
    c(3.0),
    c(10.0000001, 2.00)
  )
  # Upper bound for each function
  upper_bounds <- c(
    c(2*mean(degree_sequence)),
    c(0.99999),
    c(+Inf),
    c(3.0000002),
    c(N, +Inf)
  )

  
  names <- c(
    "Displaced Poisson",
    "Displaced Geometric",
    "Zeta Function with Gamma = 2",
    "Zeta Function",
    "Right Truncated Zeta"
  )
    
  # Compute the optimal parameters plus
  # (minus) log likelihoods via calls to MLE
  # As an exception, do nothing when dealing
  # with zeta with gamma = 2 (mle crashes with
  # functions with no parameters, at least in
  # our computers)
  opt_parameters <- NULL
  log_likelihoods <- NULL
  for (i in 1:4) {
    print(i)
    if(names[i] != "Zeta Function with Gamma = 2") {
      to_add <- mle(minuslogl = functions[[i]],
                    start = start_parameters[i],
                    method = "L-BFGS-B",
                    lower = lower_bounds[i],
                    upper = upper_bounds[i]
                    )
      log_likelihood <- attributes(summary(to_add))$m2logL
      log_likelihoods <- c(log_likelihoods, log_likelihood)
    }
    else {
      to_add <- -1
      log_likelihoods <- c(log_likelihoods, 2*functions[[i]]())
    }
    opt_parameters <- c(opt_parameters,
                         to_add
    )
  }
  print('5')
  to_add <- mle(minuslogl = functions[[5]],
                start  = list(kmax = max(degree_sequence), gamma = 3.0),
                method = "L-BFGS-B",
                lower  = c(3.00, 3.00),
                upper  = c(+Inf, 3.0000002)
                # avoid NaNs an log0s...
                #upper  = c(max(degree_sequence), min(10, mean(degree_sequence)))
                  )
  log_likelihoods <- c(log_likelihoods, attributes(summary(to_add))$m2logL)
  opt_parameters <- c(opt_parameters, to_add)
  print(opt_parameters)
  
  # Compute and return the pairs (AIC, name)
  k_values <- c(1, 1, 0, 1, 2)
  return(
    c(opt_parameters,
      Map(get_AIC,
        as.numeric(log_likelihoods),
        k_values,
        rep(N, 5),
        names
      )
    )
  )
}


set_constants(deg_pref[,1])
aic_deg_pref = get_aics(deg_pref[,1])

disp_geom = function(q, k) q*(1 - q)^(k-1)
zeta_distro = function(gam, k) k^(-gam) / zeta(gam)



maxdeg_pref = max(deg_pref[,1])
hist(sort(deg_pref[,1], decreasing = TRUE), probability = T, breaks = 50)
lines(disp_geom(coef(aic_deg_pref[[2]]), seq(1,maxdeg_pref)), type = "l", col = "red")


set_constants(deg_rand[,1])
aic_deg_rand = get_aics(deg_rand[,1])

maxdeg_rand = max(deg_rand[,1])
hist(sort(deg_rand[,1], decreasing = TRUE), probability = T, breaks = 50)
lines(zeta_distro(coef(aic_deg_rand[[4]]), seq(1,maxdeg_rand)), type = "l", col = "red")

