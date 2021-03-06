
make.plots <- function(dir, lang, data, fit.model, model) {
  
  make.title.fit.model <- function() {
    a = coef(fit.model)[2]
    d = coef(fit.model)[1]
    
    title = paste0(lang, " (y = ", round(a, 3), "*x", " + ", round(d, 3), ")")
    return (title)
  }
  
  plot_name = paste0(dir, lang, ".png")
  png(filename = plot_name)
  plot(x = data$t,
       y = data$k,
       xlab = "t",
       ylab = "k",
       main = make.title.fit.model()
  )
  lines(data$t, fitted(fit.model), col = "green")
  dev.off()
}

study.fit.model <- function(lang, model) {
  filename = paste0(lang, ".csv")
  
  LANG = read.table(filename, header = TRUE, row.names = 1, sep = ",")
  colnames(LANG) = c("t", "k")
  # LANG = LANG[order(LANG$t), ]
  
  # mean_LANG = aggregate(LANG, list(LANG$t), mean)
  
  fit.model = lm(
    formula = k ~ t,
    LANG
  )
  
  RSS_ <- deviance(fit.model)
  AIC_ <- AIC(fit.model)
  s_ <- sqrt(RSS_/df.residual(fit.model))
  
  if (model == "pref"){
    dir = "figures/model0plus/"
  }
  else if (model == "rand"){
    dir = "figures_rand/model0plus/"
  }
  else if (model == "nogro"){
    dir = "figures_nogro/model0plus/"
  }
  
  make.plots(dir, lang, LANG, fit.model, model)
  
  return (list(
    RSS = RSS_,
    AIC = AIC_,
    s = s_
  ))
}

datasets_1 = c("dat1", "dat10", "dat100", "dat1000")
datasets_2 = c("rdat1", "rdat10", "rdat100", "rdat1000")
datasets_3 = c("ndat1", "ndat10", "ndat100", "ndat1000")

model = function(datasets, model){
  aic = c()
  for (dataset in datasets) {
    message(dataset, ":")
    r = study.fit.model(dataset, model)
    #message("    RSS=", round(r$RSS, 3))
    message("    AIC=", round(r$AIC, 3))
    message("    s=  ", round(r$s, 3))
    aic = c(aic, round(r$AIC, 3))
  }
  return(aic)
}

nogro_AIC0p = model(datasets_3, "nogro")
rand_AIC0p = model(datasets_2, "rand")
pref_AIC0p = model(datasets_1, "pref")
