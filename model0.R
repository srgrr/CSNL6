library(functional)

k_func = function(t, den){
  m0 = 5
  f = m0*t^0.5
  out = f/sqrt(den)
  return(out)
}



make.plots <- function(dir, lang, data, fit.model) {
  
  make.title.fit.model <- function() {
    a = coef(fit.model)[1]
    
    title = paste0(lang, " (y = ", round(a, 3), "*x^",")")
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
  k_f = Curry(k_func, den = eval(parse(text = substr(lang, start = 4, stop = nchar(lang)))))
  curve(k_f, from = 1000, to = 100000 ,col="red", add= T)
  dev.off()
}

study.fit.model0 <- function(lang) {
  filename = paste0(lang, ".csv")
  
  LANG = read.table(filename, header = TRUE, row.names = 1, sep = ",")
  colnames(LANG) = c("t", "k")
  # LANG = LANG[order(LANG$t), ]
  
  # mean_LANG = aggregate(LANG, list(LANG$t), mean)
  
  fit.model = lm(
    formula = k ~ t - 1,
    LANG
  )
  
  RSS_ <- deviance(fit.model)
  AIC_ <- AIC(fit.model)
  s_ <- sqrt(RSS_/df.residual(fit.model))
  
  make.plots("figures/model0/", lang, LANG, fit.model)
  
  return (list(
    RSS = RSS_,
    AIC = AIC_,
    s = s_
  ))
}

datasets = c("dat1", "dat10", "dat100", "dat1000")

for (dataset in datasets) {
  message(dataset, ":")
  r = study.fit.model0(dataset)
  #message("    RSS=", round(r$RSS, 3))
  message("    AIC=", round(r$AIC, 3))
  message("    s=  ", round(r$s, 3))
}
