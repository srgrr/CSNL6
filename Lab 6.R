dat = read.csv("CSNL6/out2.txt", sep = " ", header = FALSE)
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

# source = read.table("list.txt", 
#                     header = TRUE,               
#                     as.is = c("language","file")
# )
# 
# langs = source$language

# Datasets with all the results
results = data.frame()
deviance = data.frame()
AIC = data.frame()
S = data.frame()


dat = c("dat1","dat10", "dat100", "dat1000")
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
  
  # lang_name = source$language[lang]
  # tab = read.table(source$file[lang], header = FALSE)
  # colnames(tab) = c("t","degree_2nd_moment", "k")
  tab = eval(parse(text = dat[lang]))

  # lmod1 = lm(k ~ t, tab)
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
  
  
  ###### model 1+ #######
  # f(n) = (n/2)^b + d
  
  # b1p_init = b1_opt
  # d1p_init = 0
  # 
  # mod1p = nls(k ~ (t/2)^b + d, tab,
  #             start = list(b = b1p_init, d = d1p_init), trace = FALSE)
  # 
  # b1p_opt = coef(mod1p)["b"]
  # d1p_opt = coef(mod1p)["d"]
  # 
  # 
  # res[i] = b1p_opt
  # i = i + 1
  # res[i] = d1p_opt
  # i = i + 1
  # 
  # dev[j] = deviance(mod1p)
  # aic[j] = AIC(mod1p)
  # s_list[j] = s_func(mod1p)
  # j  = j +1
  # 
  # 
  # ###### model 2+ #######
  # # f(n) = a*n^b + d
  # 
  # a2p_init = a2_opt
  # b2p_init = b2_opt
  # d2p_init = 1.5
  # 
  # mod2p = nls(k ~ a * t^b + d, tab,
  #             start = list(a = a2p_init, b = b2p_init, d = d2p_init), trace = FALSE)
  # 
  # a2p_opt = coef(mod2p)["a"]
  # b2p_opt = coef(mod2p)["b"]
  # d2p_opt = coef(mod2p)["d"]
  # 
  # res[i] = a2p_opt
  # i = i + 1
  # res[i] = b2p_opt
  # i = i + 1
  # res[i] = d2p_opt
  # i = i + 1
  # 
  # dev[j] = deviance(mod2p)
  # aic[j] = AIC(mod2p)
  # s_list[j] = s_func(mod2p)
  # j  = j + 1
  # 
  
  ##### populate dataframes #####
  
  results = rbind(results, res)
  deviance = rbind(deviance, dev)
  AIC = rbind(AIC, aic)
  S = rbind(S, s_list)
  
  models = c("mod1", "mod2", "mod3")
  min_aic = which.min(aic)
  min_s = which.min(s_list)
  if (min_aic == min_s) {
    print(models[min_aic])
    barplotfile = sprintf("plot_%s.pdf",dat[lang])
    pdf(file = barplotfile)    # open pdf file
    plottitle = sprintf("Best fit for %s",dat[lang])
    plot(log(tab$t), log(tab$k),
         xlab = "log(t)", ylab = "log(mean dependency length)", main = plottitle)
    lines(log(sort(tab$t)), log(fitted(get(models[min_aic]))[order(tab$t)]), col = "green")
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
colnames(results) = c("2.a", "2.a", "2.b", "3.a", "3.c")
colnames(deviance) = c("1","2", "3")
colnames(AIC) = c("1","2", "3")
colnames(S) = c("1","2", "3")
rownames(results) = dat
rownames(deviance) = dat
rownames(AIC) = dat
rownames(S) = dat


