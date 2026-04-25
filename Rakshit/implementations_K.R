library(spatstat.Knet)

X <- unmark(chicago)

plot (linearK(X))

fit <- lppm(X ~ polynom(x,y,2))

plot(linearKinhom(X, fit))