##### install.packages("ggplot2")
owiny <- read.csv("owiny.csv",header=T, na.strings="?")
attach(owiny)

######## General properties of data set
dim(owiny)
names(owiny)
str(owiny)
apply(owiny, MARGIN = 2, summary)

########### Point and box plots on data with respect to the web objects
library(ggplot2)
p1 <- ggplot(owiny, aes(x, y, color = factor(i))) + geom_point(aes(group = i)) + labs(title = "Revenue vs. Downloads") + labs(x = "(a)     # Downloads") + labs(y = "Revenue [euro]")
      p1 + geom_blank(aes(group = i))

p2 <- ggplot(owiny, aes(factor(i), x))
      p2 + geom_boxplot(aes(group=i, colour = factor(i))) + labs(title = "Downloads vs. Object") + labs(x = "(b)      Objects") + labs(y = "# Downloads")

p3 <- ggplot(owiny, aes(factor(i), y))
      p3 + geom_boxplot(aes(group=i, colour = factor(i))) + labs(title = "Revenue vs. Object") + labs(x = "(c)      Objects") + labs(y = "Revenue [euro]")


############## Calculates D_i_max for web objects
sapply(tapply(owiny$x, owiny$i, diff), max)

############## Linear fit for the 10 linear models
library(nlme)
lmList.fits <- lmList(y~x -1| i, owiny)
summary(lmList.fits)         # models' coefs, pooled-RSE/df
as.vector(sapply(lmList.fits, coef))    # Slope (⍺_i) for each model
sapply(lmList.fits, confint)   # 95% C.I for each model parameter(⍺_i)

##########  Calculating Confidence Interval for  standard_deviation, σ
SD <- pooledSD(lmList.fits)    # pooled SD value(mean of the 10 RSE values)
cv_L <- qchisq(0.9985, 2399)  # Left critical value-chi_squared
cv_R <- qchisq(0.0015, 2399)  # Right critical value-chi_squared
value_L <- sqrt((2400-1)*SD^2/cv_L)
value_R <- sqrt((2400-1)*SD^2/cv_R)
print(paste("99.7% Confidence interval for the standard deviation, σ, is [", as.integer(value_L), ",", as.integer(value_R), "]"))


################ Interpolation polynomial
library(ggplot2)
x_grid <- seq(0, 150000, length = 2400)
objects <- unique(owiny$i)
preds <- expand.grid(x = x_grid, i = objects)
preds$y <- predict(lmList.fits, preds)
p <- ggplot(owiny, aes(x, y, color = factor(i))) + geom_line(data = preds, size= 1.0) + coord_cartesian(ylim = c(0, 1000000), xlim = c(0, 60000)) + labs(title = " Revenue vs. Downloads")
p + labs(x = "# Downloads") + labs(y = "Revenue [euro]") + theme_bw()


################ Residual vs. Predicted
oplot <- ggplot(owiny, aes(x, y, group = i)) + geom_point(size = 0.4)
owiny$fitted <- predict(lmList.fits)
owiny$resid <- with(owiny, fitted - y)
oplot %+% owiny + aes(x= fitted, y = resid) + geom_smooth(aes(group=1)) + labs(title = "Residual vs. Predicted") + labs(x = "Predicted [euro]") + labs(y = "Residual [euro]") + theme_bw()

################ Residual vs. Time
oplot %+% owiny + aes(x = t, y = resid) + geom_smooth(aes(group=1)) + labs(title = "Residual vs. Time (correlation = -0.00064)") + labs(x = "Time [hr]") + labs(y = "Residual") + theme_bw()

###############  Correlation 
cor(fitted(lmList.fits), resid(lmList.fits))

##############  Clear Environment and exit
rm(list = ls())
q()
