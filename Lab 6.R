dat = read.csv("data_random_attach.txt", sep = " ", header = FALSE)
colnames(dat) = c("node", "t", "k")
dat1 = dat[which(dat[,1] == 1),]
dat10 = dat[which(dat[,1] == 10),]
dat100 = dat[which(dat[,1] == 100),]
dat1000 = dat[which(dat[,1] == 1000),]

plot(dat1$t, dat1$k, type = "l", col = "green", ylim = c(0, 850),
     xlab = "t", ylab = "k")
lines(dat1000$k, col = "blue")
lines(dat10$k, col = "red")
lines(dat100$k, col = "yellow")

# model selection lab3 ----------------------------------------------------
library(stats)
library(lmtest)
library(caret)
library(car)

# manca 1/2 davanti --> così non va bene 
find_starts_1 = function(data){
  linear =  lm(log(k) - (1/2)*log(t) ~ 1, data)
  inter = exp(linear$coefficients[1])
  return(inter)
}

find_starts_2 = function(dat){
  linear =  lm(log(k) ~ log(t), dat)
  slope = linear$coefficients[2]
  inter =  exp(linear$coefficients[1])
  return(c(slope, inter))
}

find_starts_3 = function(dat){
  linear = lm(log(k) ~ t, dat)
  inter = exp(linear$coefficients[1])
  slope = linear$coefficients[2]
  return(c(slope, inter))
}


group_variance = function(table, lang, lmod){
  var_g = c()
  vert = sort(unique(table$t))
  for (v in 1:length(vert)){
    var_g[v] = var(table$k[table$t == vert[v]])
  }
  data = data.frame(cbind(vert, var_g))
  data[is.na(data)] = 0.0
  test = cor.test(data$vert, log(data$var_g), method = "kendall", exact = FALSE)
  
  barplotfile = sprintf("plot_vartest_%s.pdf",lang_name)
  pdf(file = barplotfile)
  plottitle = sprintf("variance vs #t: %s",lang_name)
  plot(vert, log(var_g), main = plottitle)
  grid()
  dev.off()
  
  if (test$p.value > 0.05) return(TRUE)
  else return(FALSE)
}


# Compute the residual standard error
s_func = function(nlm){
  sqrt(deviance(nlm)/df.residual(nlm))
}

# Datasets with all the results
results = data.frame()
deviance = data.frame()
AIC = data.frame()
S = data.frame()


dat = c("rdat1","rdat10", "rdat100", "rdat1000")
time = c(1,10,100,1000)
# Computing all the results for each language
for (lang in 1:length(dat)) {
  print(dat[lang])
  i = 1
  j = 1
  
  # lists of results for each language
  res = c()
  dev = c()
  aic = c()
  s_list = c()
  
  tab = eval(parse(text = dat[lang]))

  lmod1 = lm(k ~ t, tab)
  # 
  # if (group_variance(tab, lang) == FALSE){
  #   tab = aggregate(tab, list(tab$t), mean)
  #   paste(cat("The", dat[lang], "language has not the homoskedasticity\n"))
  # }
  # else {
  #   paste(cat("The", dat[lang], "language HAS the homoskedasticity property\n"))
  # }
  
  
  
  ####### model 0 #######
  # formula0 = (n+1)/3
  
  # RSS <- sum((tab$k-(tab$t+1)/3)^2)
  # n <- length(tab$t)
  # p <- 0
  # s_0 <- sqrt(RSS/(n - p))
  # 
  # aic_0 <- n*log(2*pi) + n*log(RSS/n) + n + 2*(p + 1)
  # aic[j] = aic_0
  # dev[j] = RSS
  # s_list[j] = s_0
  # j = j + 1
  print("     model0")
  
  mod0 = lm(k ~ t - 1, tab)
  a0_opt = coef(mod0)["t"]
  
  res[i] = a0_opt
  i = i + 1
  
  aic[j] = AIC(mod0)
  dev[j] = deviance(mod0)
  s_list[j] = s_func(mod0)
  j = j + 1
  
  ######## model 1 #######
  # formula1 = function(n, b) (n/2)^b 
  # b*log(n) - b*log(2)
  print("     model1")
  
  a1_init = find_starts_1(tab)
  
  mod1 = nls(k ~ a*(t)^0.5, tab, 
             start = list(a = a1_init), trace = FALSE)
  
  a1_opt = coef(mod1)["a"]
  
  res[i] = a1_opt
  i = i + 1
  
  aic[j] = AIC(mod1)
  dev[j] = deviance(mod1)
  s_list[j] = s_func(mod1)
  j = j + 1
  
  ######## model 2 #######
  # b*log(n) + log(a)
  print("     model2")
  
  b2_init = find_starts_2(tab)[1]
  a2_init = find_starts_2(tab)[2]
  
  mod2 = nls(k ~ a*(t^b), tab,
             start = list(a = a2_init, b = b2_init), trace = FALSE)
  
  a2_opt = coef(mod2)["a"]
  b2_opt = coef(mod2)["b"]
  
  res[i] = a2_opt
  i = i + 1
  res[i] = b2_opt
  i = i + 1
  
  dev[j] = deviance(mod2)
  aic[j] = AIC(mod2)
  s_list[j] = s_func(mod2)
  j  = j +1
  
  
  ####### model 3 ######
  print("     model3")
  c3_init = find_starts_3(tab)[1]
  a3_init = find_starts_3(tab)[2]
  
  mod3 = nls(k ~ a * exp(c*t), tab,
             start = list(a = a3_init, c= c3_init), trace = FALSE)
  
  a3_opt = coef(mod3)["a"]
  c3_opt = coef(mod3)["c"]
  
  res[i] = a3_opt
  i = i + 1
  res[i] = c3_opt
  i = i + 1
  
  dev[j] = deviance(mod3)
  aic[j] = AIC(mod3)
  s_list[j] = s_func(mod3)
  j  = j +1
  
  ###### model 0+ #######
  print("     model0+")
  
  mod0p = lm(k ~ t, tab)
  
  a0p_opt = coef(mod0p)["t"]
  d0p_opt = coef(mod0p)[1]
  
  res[i] = a0p_opt
  i = i + 1
  res[i] = d0p_opt
  i = i + 1
  
  dev[j] = deviance(mod0p)
  aic[j] = AIC(mod0p)
  s_list[j] = s_func(mod0p)
  j  = j +1
  
  ###### model 1+ #######
  # f(n) = (n/2)^b + d
  print("     model1+")
  
  a1p_init = a1_opt
  d1p_init = 0

  mod1p = nls(k ~ (a * t^0.5) + d, tab,
              start = list(a = a1p_init, d = d1p_init), trace = FALSE)

  a1p_opt = coef(mod1p)["a"]
  d1p_opt = coef(mod1p)["d"]


  res[i] = a1p_opt
  i = i + 1
  res[i] = d1p_opt
  i = i + 1

  dev[j] = deviance(mod1p)
  aic[j] = AIC(mod1p)
  s_list[j] = s_func(mod1p)
  j  = j +1

  
  ###### model 2+ #######
  # f(n) = a*n^b + d
  print("     model2+")

  a2p_init = a2_opt
  b2p_init = b2_opt
  d2p_init = 0

  mod2p = nls(k ~ (a * t^b) + d, tab,
              start = list(a = a2p_init, b = b2p_init, d = d2p_init), trace = FALSE)

  a2p_opt = coef(mod2p)["a"]
  b2p_opt = coef(mod2p)["b"]
  d2p_opt = coef(mod2p)["d"]

  res[i] = a2p_opt
  i = i + 1
  res[i] = b2p_opt
  i = i + 1
  res[i] = d2p_opt
  i = i + 1

  dev[j] = deviance(mod2p)
  aic[j] = AIC(mod2p)
  s_list[j] = s_func(mod2p)
  j  = j + 1
  
  
  ###### model 3+ #######
  print("     model3+")
  ### this one doesn't work beacuse the starting points are too far form the optimum
  
  # a3p_init = a3_opt
  # c3p_init = c3_opt
  # d3p_init = -1
  # 
  # mod3p = nls(k ~ (a * exp(c * t)) + d, tab,
  #             start = list(a = a3p_init, c = c3p_init, d = d3p_init), trace = FALSE)
  # 
  # a3p_opt = coef(mod2p)["a"]
  # c3p_opt = coef(mod2p)["b"]
  # d3p_opt = coef(mod2p)["d"]
  # 
  # res[i] = a3p_opt
  # i = i + 1
  # res[i] = b3p_opt
  # i = i + 1
  # res[i] = d3p_opt
  # i = i + 1
  # 
  # dev[j] = deviance(mod3p)
  # aic[j] = AIC(mod3p)
  # s_list[j] = s_func(mod3p)
  # j  = j + 1

  
  ##### populate dataframes #####
  
  results = rbind(results, res)
  deviance = rbind(deviance, dev)
  AIC = rbind(AIC, aic)
  S = rbind(S, s_list)
  
  models = c("mod0", "mod1", "mod2", "mod3", "mod0p", "mod1p", "mod2p", "mod3p")
  min_aic = which.min(aic)
  min_s = which.min(s_list)
  if (min_aic == min_s) {
    print(models[min_aic])
    barplotfile = sprintf("plot_%s.pdf",dat[lang])
    pdf(file = barplotfile)    # open pdf file
    plottitle = sprintf("Best fit for %s",dat[lang])
    plot(tab$t, tab$k,
         xlab = "log(t)", ylab = "log(mean dependency length)", main = plottitle, type = "l")
    lines(sort(tab$t), fitted(get(models[min_aic]))[order(tab$t)], col = "green")
    
    k = function(t) {
      m0 = 5
      f = m0*t^0.5
      out = f/sqrt(time[lang])
      return(out)
    }
    
    curve(k, from = 1000, to = 100000 ,col="red", add= T)
    grid()
    dev.off()
  }
  else {
    print(c(min_aic, min_s))
    n = length(aic)
    sec_min_aic = sort(aic, decreasing = TRUE)[n-1]
    if (min_s == which(aic == sec_min_aic)){
      print(models[which(aic == sec_min_aic)])
      print(models[min_s])
      barplotfile = sprintf("plot_%s.pdf",dat[lang])
      pdf(file = barplotfile)    # open pdf file
      plottitle = sprintf("Best fit for %s",dat[lang])
      plot(log(tab$t), log(tab$k),
           xlab = "log(t)", ylab = "log(mean dependency length)", main = plottitle)
      lines(log(tab$t), log(fitted(get(models[min_s]))), col = "blue")
      grid()
      dev.off()
    }
  }
}
colnames(results) = c("0.a", "1.a", "2.a", "2.b", "3.a", "3.c", "0+.a", "0+.d","1+.a", 
                      "1+.d", "2+.a", "2+.b", "2+.d") #, "3+.a", "3+.c", "3+.d")
colnames(deviance) = c("0","1","2", "3", "0+", "1+", "2+") #, "3")
colnames(AIC) = c("0","1","2", "3", "0+", "1+", "2+") #, "3")
colnames(S) = c("0","1","2", "3", "0+", "1+", "2+") #, "3")
rownames(results) = dat
rownames(deviance) = dat
rownames(AIC) = dat
rownames(S) = dat


