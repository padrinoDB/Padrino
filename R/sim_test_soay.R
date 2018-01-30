library(ggplot2)
library(dplyr)
library(magrittr)

# parameter list
par_fix <- readRDS(file = "par_fix.rds")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## a few utlilty functions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

calculate_P_transitions <- function(a_set) { 
  a_ <- c(tail(a_set, -1), tail(a_set, 1))
  a_ <- as.character(a_)
  a  <- a_set
  a  <- as.character(a)
  data.frame(a_ = a_, a = a)
}

calculate_F1_transitions <- function(a_set) { 
  a_  <- as.character(a_set)
  t_ <- c("s", "t")
  s_ <- c("f", "m")
  out <- expand.grid(s_ = s_, t_ = t_, a_ = a_, stringsAsFactors = FALSE)
  cbind(out, a = out$a_)
}

calculate_F2_transitions <- function(a_set) { 
  a  <- as.character(a_set)
  t <- c("s", "t")
  s <- c("f", "m")
  out <- expand.grid(s = s, t = t, a = a, stringsAsFactors = FALSE)
  cbind(s_ = out$s, a_ = "0", out)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## simulation code
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

source("ipm_implementation.R")

min_a <- 0
max_a <- 15

a_rule     <- categorical_integer(c(min_a, max_a))
x_rule     <- numeric_midpoint(c(5, 46, 80)) # <- don't need 80 mesh points
s_rule     <- categorical_nominal(c("f", "m"))
t_rule     <- categorical_nominal(c("s", "t"))
x_lmb_rule <- numeric_midpoint(c(0.1, 3, 30))

P_transitions  <- calculate_P_transitions(min_a:max_a)
F1_transitions <- calculate_F1_transitions(min_a:max_a)
F2_transitions <- calculate_F2_transitions(min_a:max_a)

##
## 1. define up the survival-growth functions
##

n1 <- new_n_t(x = x_rule, a = a_rule)
n2 <- new_n_t(x = x_rule, a = a_rule)

su_f <- ipm_fun(par_fix$su,
                f_fix = ~ b_0_f + b_z1_f * log(x) + b_a1_f * a + b_a2_f * a^2,
                f_env = ~ yr_ef[i,1] + (gamma + yr_ef[i,2]) * expect_f(n, theta) / x^theta,
                f_dmg = ~ inv_logit(.additive),
                P_transitions, n2, n1)

su_m <- ipm_fun(par_fix$su,
                f_fix = ~ b_0_f + b_z1_f * log(x) + b_a1_f * a + b_a2_f * a^2 +
                  b_0_m + b_z1_m * log(x) + b_a1_m * a + b_a2_m * a^2,
                f_env = ~ yr_ef[i,1] + (gamma + yr_ef[i,2]) * expect_f(n, theta) / x^theta,
                f_dmg = ~ inv_logit(.additive),
                P_transitions, n2, n1)

gr_f <- ipm_fun(par_fix$gr,
                f_fix = ~ b_0_f + b_z1_f * log(x) + b_a1_f * a + b_a2_f * a^2,
                f_env = ~ yr_ef[i,1] + (gamma + yr_ef[i,2]) * expect_f(n, theta) / x^theta,
                f_dmg = ~ dnorm(log(x_), mean = .additive, sd = sigma) / x_,
                P_transitions, n2, n1)

gr_m <- ipm_fun(par_fix$gr,
                f_fix = ~ b_0_f + b_z1_f * log(x) + b_a1_f * a + b_a2_f * a^2 +
                  b_0_m + b_z1_m * log(x) + b_a1_m * a + b_a2_m * a^2,
                f_env = ~ yr_ef[i,1] + (gamma + yr_ef[i,2]) * expect_f(n, theta) / x^theta,
                f_dmg = ~ dnorm(log(x_), mean = .additive, sd = sigma) / x_,
                P_transitions, n2, n1)

##
## 2. define up the fecundity functions
##

n1 <- new_n_t(x = x_rule, a = a_rule)
n2 <- new_n_t(s = s_rule, t = t_rule, x = x_lmb_rule, a = a_rule)

su <- ipm_fun(par_fix$su,
              f_fix = ~ b_0_f + b_z1_f * log(x) + b_a1_f * a + b_a2_f * a^2,
              f_env = ~ yr_ef[i,1] + (gamma + yr_ef[i,2]) * expect_f(n, theta) / x^theta,
              f_dmg = ~ inv_logit(.additive),
              F1_transitions, n2, n1)

rp <- ipm_fun(par_fix$rp,
              f_fix = ~ b_0_f + b_z1_f * log(x) + b_a1_f * a + b_a2_f * a^2,
              f_env = ~ yr_ef[i,1] + (gamma + yr_ef[i,2]) * expect_f(n, theta) / x^theta,
              f_dmg = ~ inv_logit(.additive),
              F1_transitions, n2, n1)

tw <- ipm_fun(par_fix$tw,
              f_fix = ~ b_0_f + b_z1_f * log(x) + b_a1_f * a + b_az_f * a * log(x),
              f_env = ~ yr_ef[i,1] + (gamma + yr_ef[i,2]) * expect_f(n, theta) / x^theta,
              f_dmg = ~ inv_logit(.additive) * (a > 0),
              F1_transitions, n2, n1)

bw <- ipm_fun(par_fix$bw_lmb,
              f_fix = ~ b_0_f + (s_ == "m") * b_0_m + (t_ == "t") * b_0_tw + 
                b_a1_f * a + b_z1_f * log(x) + b_az_f * a * log(x) + b_a2_f * a^2,
              f_env = ~ yr_ef[i,1] + (gamma + yr_ef[i,2]) * expect_f(n, theta) / x^theta,
              f_dmg = ~ dnorm(log(x_), .additive, sigma) / x_,
              F1_transitions, n2, n1)

##
## 3. lamb spring-summer iteration functions
##

n1 <- new_n_t(s = s_rule, t = t_rule, x = x_lmb_rule, a = a_rule)
n2 <- new_n_t(x = x_rule, s = s_rule, a = a_rule)

su_lmb <- ipm_fun(par_fix$su_lmb,
                  f_fix = ~ b_0_f + (s == "m") * b_0_m + (t == "t") * b_0_tw + 
                    b_z1_lm * log(x) + b_a1_f * a + b_a2_f * a^2,
                  f_env = ~ yr_ef[i,1],
                  f_dmg = ~ inv_logit(.additive),
                  F2_transitions, n2, n1)

gr_lmb <- ipm_fun(par_fix$gr_lmb,
                  f_fix = ~ b_0_f + (s == "m") * b_0_m + (t == "t") * b_0_tw + 
                    b_z1_lm * log(x) + b_a1_f * a + b_a2_f * a^2,
                  f_env = ~ yr_ef[i,1],
                  f_dmg = ~ dnorm(log(x_), .additive, sigma) / x_,
                  F2_transitions, n2, n1)

##
## 4. kernel definitions
##

P_kern  <- 
  ~ su(a_, a) * gr(a_, a)

F_s_kern <- 
  ~ su(s_, t_, a_, a) * rp(s_, t_, a_, a) * 
  ( 1 - tw(s_, t_, a_, a) ) * bw(s_, t_, a_, a) / 2

F_t_kern <- 
  ~ su(s_, t_, a_, a) * rp(s_, t_, a_, a) * 
  tw(s_, t_, a_, a) * bw(s_, t_, a_, a) 

F2_kern <- 
  ~ su(s_, a_, s, t, a) * gr(s_, a_, s, t, a)

##
## 5. function to iterate the model. This is model-specific because
## Dylan doesn't have time at the moment to generalise this part
##

iterate_model <- function(n_t_0, env_seq, n_step) {
  
  n_t <- n_t_0
  n_t_seq <- list()
  n_t_seq[[1]] <- n_t
  for (step in 1:n_step) {
    # storage for intermediate state
    n_t_I <- new_n_t(s = s_rule, t = t_rule, x = x_lmb_rule, a = a_rule)
    # storage for final state
    n_t_1 <- new_n_t(s = s_rule, x = x_rule, a = a_rule)
    # size distribution now
    n_z <- marginal_density(n_t, margin = "x")
    # construct male and female survival-growth iteration matrices
    P_iter_mat_f <- 
      ipm_kern(kern = P_kern, funs = list(su = su_f, gr = gr_f), 
               P_transitions, i = env_seq[step], n = n_z)
    P_iter_mat_m <- 
      ipm_kern(kern = P_kern, funs = list(su = su_m, gr = gr_m), 
               P_transitions, i = env_seq[step], n = n_z)
    # growth-survival
    wgt <- attr(n_t ,"details")[["wgt", "x"]]
    is_first_contrib <- !duplicated(P_transitions$a_)
    tr_index <- 1:nrow(P_transitions)
    for (tr in tr_index) {
      a  <- P_transitions$a [tr]
      a_ <- P_transitions$a_[tr]
      if (is_first_contrib[tr]) {
        n_t_1[["f", a_]] <- 
          P_iter_mat_f[[tr]] %*% n_t[["f", a]] * wgt
        n_t_1[["m", a_]] <- 
          P_iter_mat_m[[tr]] %*% n_t[["m", a]] * wgt
      } else {
        n_t_1[["f", a_]] <- 
          n_t_1[["f", a_]] + P_iter_mat_f[[tr]] %*% n_t[["f", a]] * wgt
        n_t_1[["m", a_]] <- 
          n_t_1[["f", a_]] + P_iter_mat_m[[tr]] %*% n_t[["m", a]] * wgt
      }
    }
    
    # fecundity -> spring mother-offspring state
    F_s_iter_mat <- 
      ipm_kern(kern = F_s_kern, funs = list(su = su, rp = rp, tw = tw, bw = bw), 
               F1_transitions, i = env_seq[step], n = n_z)
    F_t_iter_mat <- 
      ipm_kern(kern = F_t_kern, funs = list(su = su, rp = rp, tw = tw, bw = bw), 
               F1_transitions, i = env_seq[step], n = n_z)
    wgt <- attr(n_t ,"details")[["wgt", "x"]]
    tr_index <- 1:nrow(F1_transitions)
    for (tr in tr_index) {
      a  <- F1_transitions$a [tr]; a_ <- F1_transitions$a_[tr]
      s_ <- F1_transitions$s_[tr]; t_ <- F1_transitions$t_[tr]
      if (t_ == "s") {
        n_t_I[[s_, t_, a_]] <- 
          F_s_iter_mat[[tr]] %*% n_t[["f", a]] * wgt
      } else {
        n_t_I[[s_, t_, a_]] <- 
          F_t_iter_mat[[tr]] %*% n_t[["f", a]] * wgt
      }
    }
    
    # fecundity -> summer offspring state
    F2_iter_mat <- 
      ipm_kern(kern = F2_kern, funs = list(su = su_lmb, gr = gr_lmb), 
               F2_transitions, i = env_seq[step], n = n_z)
    wgt <- attr(n_t_I ,"details")[["wgt", "x"]]
    is_first_contrib <- !duplicated(F2_transitions$s_)
    tr_index <- 1:nrow(F2_transitions)
    for (tr in tr_index) {
      s1 <- F2_transitions$s[tr]; t1 <- F2_transitions$t[tr]
      a  <- F2_transitions$a[tr]
      if (is_first_contrib[tr]) {
        n_t_1[[s1, "0"]] <- 
          F2_iter_mat[[tr]] %*% n_t_I[[s1, t1, a]] * wgt
      } else {
        n_t_1[[s1, "0"]] <- 
          n_t_1[[s1, "0"]] + F2_iter_mat[[tr]] %*% n_t_I[[s1, t1, a]] * wgt
      }
    }
    #
    n_t_seq[[step+1]] <- n_t <- n_t_1
  }
  return(n_t_seq)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Testing -- simulation code
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


set_lambs <- function(n_t, size, mean, sd) { 
  val <- attr(n_t, "details")[["val", "x"]]
  val
  n_t[["f", "0"]] <- size * dnorm(val, mean = mean, sd = sd)
  n_t[["m", "0"]] <- size * dnorm(val, mean = mean, sd = sd)
  n_t
}
# initial state 
n_t_0 <- new_n_t(s = s_rule, x = x_rule, a = a_rule)
# initialise initial state
n_t_0 <- set_lambs(n_t_0, 275, 12, 1.5)

n_step <- 20
sim <- iterate_model(n_t_0, rep(1, n_step), n_step)

# time series of total number of sheep
plot(sapply(lapply(sim, unlist), sum) * x_rule$wgt, type = "l",
     xlab = "Time", ylab = "Total number of individuals")

# time series of males and females numbers
sapply(sim, marginal_density, margin = "s") %>%
  t %>% matplot(type = "l")

# time series of size distribution
sapply(sim, marginal_density, margin = "x") %>%
  apply(2, function(x) x/sum(x)) %>% t %>%
  image(x = seq(0, n_step), y = x_rule$val, z = ., col = heat.colors(1000))

# final size distribution
lapply(sim, marginal_density, margin = "x")[[length(sim)]] %>%
  plot(x = x_rule$val, y = .)


