
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
		b = coef(trick.model)[2]

		title = paste0(lang, " (ln(y) = ", round(a, 3), " + ", round(b, 3), "*ln(x))")
		return (title)
	}

	make.title.fit.model <- function() {
		a = coef(fit.model)[1]
		b = coef(fit.model)[2]

		title = paste0(lang, " (y = ", round(a, 3), "*x^", round(b, 3), ")")
		return (title)
	}

	plot_name = paste0(dir, lang, "-loglog-scale.png")
	png(filename = plot_name)
	plot(x = data$vertices,
		 y = data$degree_2nd_moment,
		 xlab = "log(Vertices)",
		 ylab = "log(Degree 2nd Moment)",
		 log = "xy",
		 main = make.title.trick.model()
	)
	lines(data$vertices, fitted(fit.model), col = "green")
	dev.off()

	plot_name = paste0(dir, lang, ".png")
	png(filename = plot_name)
	plot(x = data$vertices,
		 y = data$degree_2nd_moment,
		 xlab = "Vertices",
		 ylab = "Degree 2nd Moment",
		 main = make.title.fit.model()
	)
	lines(data$vertices, fitted(fit.model), col = "green")
	dev.off()
}

study.fit.model2 <- function(lang) {
	filename = paste0("data/", lang, "_dependency_tree_metrics.txt")

	LANG = read.table(filename, header = FALSE)
	colnames(LANG) = c("vertices","degree_2nd_moment", "mean_length")
	LANG = LANG[order(LANG$vertices), ]

	mean_LANG = aggregate(LANG, list(LANG$vertices), mean)

	trick.model = lm(
		formula = log(degree_2nd_moment) ~ log(vertices),
		mean_LANG
	)

	a_initial = exp(coef(trick.model)[1])
	b_initial = coef(trick.model)[2]

	fit.model = nls(
		formula = degree_2nd_moment ~ a*vertices^b,
		data = mean_LANG,
		start = list(a = a_initial, b = b_initial),
		trace = FALSE
	)

	RSS_ <- deviance(fit.model)
	AIC_ <- AIC(fit.model)
	s_ <- sqrt(RSS_/df.residual(fit.model))

	make.plots("figures/model2/", lang, mean_LANG, trick.model, fit.model)

	return (list(
		RSS = RSS_,
		AIC = AIC_,
		s = s_
	))
}

all_langs = c("Arabic", "Basque", "Catalan",
			  "Chinese", "Czech", "English",
			  "Greek", "Hungarian", "Italian",
			  "Turkish")

for (lang in all_langs) {
	message(lang, ":")
	r = study.fit.model2(lang)
	#message("    RSS=", round(r$RSS, 3))
	message("    AIC=", round(r$AIC, 3))
	message("    s=  ", round(r$s, 3))
}
