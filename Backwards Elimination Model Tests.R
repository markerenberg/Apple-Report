
library(MASS)
library(car)
library(lmtest)

apple_stock <- read.csv("...market_index_clean.csv", header=TRUE)

apple_return <- apple_stock$Return
date <- apple_stock$Date
days <- apple_stock$Days
price <- apple_stock$AAPL
spx <- apple_stock$SPX
vix <- apple_stock$VIX
spgscitr <- apple_stock$SPGSCITR
bndglb <- apple_stock$BNDGLB
eem <- apple_stock$EEM

# ------------------------
# Backwards Elimination 
# ------------------------

# For Backwards Elimination:
# Fitting the model with all possible explanatory variables:
fit <- lm(apple_return~days+price+spx+vix+spgscitr+bndglb+eem, data=apple_stock)
summary(fit)

# Then to store the p-value results:
pvalues <- summary(fit)[[4]][,4]

# To drop days:
fit <- lm(apple_return~price+spx+vix+spgscitr+bndglb+eem, data=apple_stock)
summary(fit)
pvalues <- summary(fit)[[4]][,4]

# To drop price:
fit <- lm(apple_return~spx+vix+spgscitr+bndglb+eem, data=apple_stock)
summary(fit)
pvalues <- summary(fit)[[4]][,4]

# Coefficients of the finalized model are:
new_model.4 <- fit
coef(new_model.4)

# Hence from the Forward Selection method, the model is:
# Return = 0.02264865-0.000003924237*SPX-0.0002433045*VIX-
#           0.000001215622*SPGSCITR-0.00003104374*BNDGLB+0.0001430637*EEM+??

# ------------------------------------------------
# Linearity Check for Backwards Elimination Model
# ------------------------------------------------

# Create pairwise scatter plots of the covariates to check linearity 
pairs(~spx+vix+spgscitr+bndglb+eem)

# To plot residuals against covariate values:
par(mfrow=c(1,1))
plot(spx, residuals(new_model.4), 
       xlab="S&P 500 Index", ylab="Residuals")
plot(vix, residuals(new_model.4), 
         xlab="CBOE Volatility Index", ylab="Residuals",
     main = "Plot of Residuals vs Volatility Index")
plot(spgscitr, residuals(new_model.4), 
     xlab="S&P Goldman Sachs Commodity Index", ylab="Residuals",
     main = "Plot of Residuals vs Commodity Index")
plot(bndglb, residuals(new_model.4), 
     xlab="Dow Jones Barclays Capital Bond Index", ylab="Residuals")
plot(eem, residuals(new_model.4), 
     xlab="Morgan Stanley Emerging Markets Index", ylab="Residuals")

# To plot studendized residuals against covariate values:
plot(spx, rstudent(new_model.4), 
     xlab="S&P 500 Index", ylab="Studendized Residuals")
abline(2, 0)
abline(-2, 0)
plot(vix, rstudent(new_model.4), 
     xlab="CBOE Volatility Index", ylab="Studendized Residuals",
     main = "Plot of Residuals vs Volatility Index")
abline(2, 0)
abline(-2, 0)
plot(spgscitr, rstudent(new_model.4), 
     xlab="S&P Goldman Sachs Commodity Index", ylab="Studendized Residuals",
     main = "Plot of Residuals vs Commodity Index")
abline(2, 0)
abline(-2, 0)
plot(bndglb, rstudent(new_model.4), 
     xlab="Dow Jones Barclays Capital Bond Index", ylab="Studendized Residuals")
abline(2, 0)
abline(-2, 0)
plot(eem, rstudent(new_model.4), 
     xlab="Morgan Stanley Emerging Markets Index", ylab="Studendized Residuals")
abline(2, 0)
abline(-2, 0)


# To plot residuals against fitted values:
par(mfrow=c(1,1))
plot(fitted(new_model.4), residuals(new_model.4), main="Raw Residual Plots",
     xlab="Fitted", ylab="Residuals")
abline(0,0)

# To plot studendized residuals against fitted values:
plot(fitted(new_model.4), rstudent(new_model.4), ylim=c(-5,5), 
     main="Studentized Residaul Plots", xlab="Fitted", ylab="Studentized Residuals")
abline(2,0)
abline(-2,0)


# To consider the linear model with quadratic VIX covariate term:
new_model.5 <- lm(apple_return~spx+poly(vix,2)+spgscitr+bndglb
                  +eem, 
                  data=apple_stock)
summary(new_model.5)

# To plot new residuals against fitted values:
par(mfrow=c(1,1))
plot(fitted(new_model.5), residuals(new_model.5), main="Raw Residual Plots",
     xlab="Fitted", ylab="Residuals")
abline(0,0)

# To plot new studendized residuals against fitted values:
plot(fitted(new_model.5), rstudent(new_model.5), ylim=c(-5,5), 
     main="Studentized Residaul Plots", xlab="Fitted", ylab="Studentized Residuals")
abline(2,0)
abline(-2,0)


# To plot studentized residuals of the new model against the original 
#   model to determine if any improvement exists:
par(mfrow=c(1,1))
plot(fitted(new_model.4), rstudent(new_model.4), ylim=c(-5,5), 
     main="Stud. Residauls v. Fitted Values for 
     Backwards Elimination Model", 
     xlab="Fitted", ylab="Studentized Residuals")
abline(2,0)
abline(-2,0)
plot(fitted(new_model.5), rstudent(new_model.5), ylim=c(-5,5), 
     main="Stud. Residuals v. Fitted Values for Backwards 
     Elimination Model With Quadratic VIX", 
     xlab="Fitted", ylab="Studentized Residuals")
abline(2,0)
abline(-2,0)

# ------------------------
# Homoscedasticity Check
# ------------------------
par(mfrow=c(1,1))
plot(fitted(new_model.5), rstudent(new_model.5), ylim=c(-5,5), 
     main="Quadratic Studentized Residual Plots", xlab="Fitted", ylab="Studentized Residuals")
abline(2,0)
abline(-2,0)

# ------------------------
# Error Normality Check
# ------------------------
par(mfrow=c(1,1))
qqnorm(residuals(new_model.5),main="QQ plot for Backwards Elimination Model 
       With Quadratic VIX")
qqline(residuals(new_model.5), datax=FALSE, distribution=qnorm, 
       probs=c(0.25, 0.75), qtype=7)
## The Normal Q-Q Plot graph indicates the normality of errors assumption holds as the
##   sample quantiles closely resemble the theoretical quantiles of a normal distribution.

# ------------------------
# Independency Check
# ------------------------

# Check Autocorrelation of covariates using ACF graphs
par(mfrow=c(1,1))
acf(residuals(new_model.5), main="Residual ACF for Backwards Elimination Model 
       With Quadratic VIX", lag.max=10)
## The ACF in the response variable has been explained away by the covariates and there
##   is no evidence of remaining autocorrelation in the residuals. 

# Use the Durbin-Watson Test for further check of autocorrelation
dwtest(new_model.5, alternative=c("two.sided"), data=apple_stock)
## DW = 1.989 and p-value = 0.7069. Since the DW value is close to 2 and p-value >> 0.05,
##   there is strong evidence that there is no autocorrelation in the residuals. 

# ------------------------
# Summary of the Model
# ------------------------
summary(full_model)
summary(new_model.5)
## The full model R squared value = 0.01077. 
## The new model R squared value = 0.01138. 