
# With this script we study the fitness of the model
#     f(n) = a*n^b
# where a,b are real constant values.
#
# To do this, we use the nls function to obtain the parameters
# of a non-linear model that best fits the input data.
# Moreover, since it needs some initial values to work with,
# we use the logarithmic transformation of the model:
#     g(n) = log(f(n)) = log(a) + b*log(n) = A + B*log(n)
# and obtain the values A and B for the linear model g.
#
# Then, the initial values a_initial and b_initial are calculated
# as follows:
#
#   a_inital = exp(A)
#   b_inital = B

make.plots <- function(dir, lang, data, trick.model, fit.model) {
  
  make.title.trick.model <- function() {
    a = coef(trick.model)[1]
    
    title = paste0(lang, " (ln(y) = ", "0.5 * log(t) + log(", a, ")")
    return (title)
  }
  
  make.title.fit.model <- function() {
    a = coef(fit.model)[1]
    
    title = paste0(lang, "a * ", a, " ^ 0.5")
    return (title)
  }
  
  plot_name = paste0(dir, lang, "-loglog-scale.png")
  png(filename = plot_name)
  plot(x = data$t,
       y = data$k,
       xlab = "t",
       ylab = "k",
       log = "xy",
       main = make.title.trick.model()
  )
  lines(data$t, fitted(fit.model), col = "green")
  dev.off()
  
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

study.fit.model <- function(dataset) {
  filename = paste0(dataset, ".csv")
  
  LANG = read.table(filename, header = TRUE, row.names = 1, sep = ",")
  colnames(LANG) = c("t", "k")
  LANG = LANG[order(LANG$t), ]
  
  #mean_LANG = aggregate(LANG, list(LANG$vertices), mean)
  
  trick.model = lm(
    formula = log(k) - 0.5 * log(t) ~ 1,
    LANG
  )
  
  a_initial = exp(coef(trick.model)[1])
  
  fit.model = nls(
    formula = k ~ a * t^0.5,
    data = LANG,
    start = list(a = a_initial),
    trace = FALSE
  )
  
  RSS_ <- deviance(fit.model)
  AIC_ <- AIC(fit.model)
  s_ <- sqrt(RSS_/df.residual(fit.model))
  
  make.plots("figures/model1/", dataset, LANG, trick.model, fit.model)
  
  return (list(
    RSS = RSS_,
    AIC = AIC_,
    s = s_
  ))
}

all_datasets = c("dat1", "dat10",  "dat100", "dat1000")

for (dataset in all_datasets) {
  message(dataset, ":")
  r = study.fit.model(dataset)
  #message("    RSS=", round(r$RSS, 3))
  message("    AIC=", round(r$AIC, 3))
  message("    s=  ", round(r$s, 3))
}
