apple_stock <- read.csv("...market_index_clean.csv", header = T)
apple_stock <- as.data.frame(apple_stock)

days <- apple_stock$DAYS
price <- apple_stock$AAPL
spx <- apple_stock$SPX
vix <- apple_stock$VIX
spgscitr <- apple_stock$SPGSCITR
bndglb <- apple_stock$BNDGLB
eem <- apple_stock$EEM
apple_return <- apple_stock$RETURN

library(leaps)
library(MASS)
library(car)
library(lmtest)


MPSE <- function(model){
  dat <- model.frame(model)
  n <- nrow(dat)
  apple_return <- dat$RETURN
  (1/n)*sum((apple_return-fitted(model)^2))
}

AllSubsetsReg <- function(stockData){
  bestSubset <- regsubsets(RETURN~., data=stockData, nbest=1)
  sum <- summary(bestSubset)
  
  par(mfrow=c(1,4))
  plot(1:length(sum$rsq), sum$rsq, xlab="p", ylab="R2", main="", pch=19)
  plot(1:length(sum$rsq), sum$adjr2, xlab="p", ylab="adj R2", main="", pch=19)
  plot(1:length(sum$rsq), sum$rss, xlab="p", ylab="RSS", main="", pch=19)
  plot(1:length(sum$rsq), sum$bic, xlab="p", ylab="BIC", main="", pch=19)
  par(mfrow=c(1,1))
  
  sum
}

GetModelSummary <- function(model){
  out <- c(MPSE(model), summary(model)$adj.r.squared, AIC(model), BIC(model))
  names(out) <- c("MSPE", "Adj. R2", "AIC", "BIC")
  out
}


initial_model.untransformed <- lm(RETURN~., data=apple_stock)

par(mfrow=c(2,4))

hist(price, breaks=30, xlab="PRICE", main="Histogram of PRICE")
hist(spx, breaks=30, xlab="SPX", main="Histogram of SPX")
hist(vix, breaks=30, xlab="VIX", main="Histogram of VIX")
hist(spgscitr, breaks=30, xlab="SPGSCITR", main="Histogram of SPGSCITR")

# Apply power transformation to price
apple_stock$AAPL <- price^0.5
price <- apple_stock$AAPL
hist(price, breaks=30, xlab=expression(paste("PRICE"^"1/2")), main="Histogram of Transformed PRICE")

# Apply power transformation to spx
apple_stock$SPX <- spx^0.5
spx <- apple_stock$SPX
hist(spx, breaks=30, xlab=expression(paste("SPX"^"1/2")), main="Histogram of Transformed SPX")

# Apply power transformation to vix
apple_stock$VIX <- vix^-1
vix <- apple_stock$VIX
hist(vix, breaks=30, xlab=expression(paste("VIX"^"-1")), main="Histogram of Transformed VIX")

# Apply power transformation to spgscitr
apple_stock$SPGSCITR <- log(spgscitr)
spgscitr <- apple_stock$SPGSCITR
hist(spgscitr, breaks=30, xlab="log(SPGSCITR)", main="Histogram of Transformed SPGSCITR")

###################################################################################
#INITIAL MODEL

initial_data <- apple_stock

initial_model <- lm(RETURN~., data=initial_data)
MPSE.initial_model <-MPSE(initial_model)

# ------------------------
# MODEL ASSUMPTIONS Check
# ------------------------

mod <- initial_model
dat <- initial_data

# Pairwise scatter plots
pairs(dat)

# Raw residuals and studentized residual plots
par(mfrow=c(1,2))
plot(fitted(mod), residuals(mod), main="Raw Residual Plots",
     xlab="Fitted", ylab="Residuals")
abline(0,0)
plot(fitted(mod), rstudent(mod), ylim=c(-5,5), 
     main="Studentized Residaul Plots", xlab="Fitted", ylab="Studentized Residuals")
abline(2,0)
abline(-2,0)

# Partial residual plots
crPlots(m<-mod, main="Partial Residual Plots", 
        ylab="Partial Residuals", smooth=T)


par(mfrow=c(1,1))
qqnorm(residuals(mod))
qqline(residuals(mod), datax=FALSE, distribution=qnorm, 
       probs=c(0.25, 0.75), qtype=7)


# Check Autocorrelation of covariates using ACF graphs
par(mfrow=c(1,1))
acf(residuals(mod), main="Residual ACF", lag.max=10)
## The ACF in the response variable has been explained away by the covariates and there
##   is no evidence of remaining autocorrelation in the residuals. 

# Use the Durbin-Watson Test for further check of autocorrelation
dwtest(mod, alternative=c("two.sided"), data=dat)


###################################################################################
#RETURN HISTORIC + ORIGINAL 7

prev1 <- apple_return[-length(apple_return)]

return_autoreg_data <- cbind(apple_stock[-1,], prev1)

days <- return_autoreg_data$DAYS
price <- return_autoreg_data$AAPL
spx <- return_autoreg_data$SPX
vix <- return_autoreg_data$VIX
spgscitr <- return_autoreg_data$SPGSCITR
bndglb <- return_autoreg_data$BNDGLB
eem <- return_autoreg_data$EEM
apple_return <- return_autoreg_data$RETURN

return_autoreg_model <- lm(RETURN~., data=return_autoreg_data)

MPSE.return_autoreg_model <- MPSE(return_autoreg_model)

crPlots(m<-return_autoreg_model, main="Partial Residual Plots", 
        ylab="Partial Residuals", smooth=T)

###################################################################################
#Simple price autoreg model
days <- apple_stock$DAYS
price <- apple_stock$AAPL
spx <- apple_stock$SPX
vix <- apple_stock$VIX
spgscitr <- apple_stock$SPGSCITR
bndglb <- apple_stock$BNDGLB
eem <- apple_stock$EEM
apple_return <- apple_stock$RETURN

prev1 <- price[-length(price)]

simple_price_autoreg_data <- cbind(apple_stock[-1,], prev1)

days <- simple_price_autoreg_data$DAYS
price <- simple_price_autoreg_data$AAPL
spx <- simple_price_autoreg_data$SPX
vix <- simple_price_autoreg_data$VIX
spgscitr <- simple_price_autoreg_data$SPGSCITR
bndglb <- simple_price_autoreg_data$BNDGLB
eem <- simple_price_autoreg_data$EEM
apple_return <- simple_price_autoreg_data$RETURN

simple_price_autoreg_model <- lm(apple_return~price+prev1, data=simple_price_autoreg_data)
simple_price_autoreg_data <- model.frame(simple_price_autoreg_model)

MPSE.simple_price_autoreg_model <- MPSE(simple_price_autoreg_model)

summary(simple_price_autoreg_model)

# ------------------------
# MODEL ASSUMPTIONS Check
# ------------------------

mod <- simple_price_autoreg_model
dat <- simple_price_autoreg_data

# Pairwise scatter plots
pairs(dat)

# Raw residuals and studentized residual plots
par(mfrow=c(1,2))
plot(fitted(mod), residuals(mod), main="Raw Residual Plots",
     xlab="Fitted", ylab="Residuals")
abline(0,0)
plot(fitted(mod), rstudent(mod), ylim=c(-5,5), 
     main="Studentized Residaul Plots", xlab="Fitted", ylab="Studentized Residuals")
abline(2,0)
abline(-2,0)

# Partial residual plots
crPlots(m<-mod, main="Partial Residual Plots", 
        ylab="Partial Residuals", smooth=T)


par(mfrow=c(1,1))
qqnorm(residuals(mod))
qqline(residuals(mod), datax=FALSE, distribution=qnorm, 
       probs=c(0.25, 0.75), qtype=7)

# Check Autocorrelation of covariates using ACF graphs
par(mfrow=c(1,1))
acf(residuals(mod), main="Residual ACF", lag.max=10)
## The ACF in the response variable has been explained away by the covariates and there
##   is no evidence of remaining autocorrelation in the residuals. 

# Use the Durbin-Watson Test for further check of autocorrelation
dwtest(mod, alternative=c("two.sided"), data=dat)

####################################################################################
#FULL AUTOREGRESSIVE MODEL
days <- apple_stock$DAYS
price <- apple_stock$AAPL
spx <- apple_stock$SPX
vix <- apple_stock$VIX
spgscitr <- apple_stock$SPGSCITR
bndglb <- apple_stock$BNDGLB
eem <- apple_stock$EEM
apple_return <- apple_stock$RETURN

# Creating matrix of historic returns looking back 21 days
t = 21
histPrices <- matrix(ncol=t, nrow=(length(price)-t))
for(i in 1:t){
  if(i == t){
    histPrices[,i] <- head(price, -i)
  }
  else{
    histPrices[,i] <- head(tail(price, i-t), -i)
  }
}

# Programmatically create variables used when fitting model
names <- Map(function(i){paste("prev", i, sep="")}, 1:t)
for(i in 1:t){
  assign(unlist(names[i]), histPrices[,i])
}

# Create new covariate matrix with historic data.  Note the first t days are no longer complete and thus are discarded
colnames(histPrices) <- names
full_autoreg_data <- cbind(apple_stock[-seq(1,t),], histPrices)

# Redefine explanatory variables with updated data
days <- full_autoreg_data$DAYS
price <- full_autoreg_data$AAPL
spx <- full_autoreg_data$SPX
vix <- full_autoreg_data$VIX
spgscitr <- full_autoreg_data$SPGSCITR
bndglb <- full_autoreg_data$BNDGLB
eem <- full_autoreg_data$EEM
apple_return <- full_autoreg_data$RETURN

full_autoreg_model <- lm(RETURN~., data=full_autoreg_data)
MPSE.full_autoreg_model <- MPSE(full_autoreg_model)

AllSubsetsReg(full_autoreg_data)

##################################################################################


###############################################################################################
#PRICE HISTORIC + ORIGINAL 7

days <- apple_stock$DAYS
price <- apple_stock$AAPL
spx <- apple_stock$SPX
vix <- apple_stock$VIX
spgscitr <- apple_stock$SPGSCITR
bndglb <- apple_stock$BNDGLB
eem <- apple_stock$EEM
apple_return <- apple_stock$RETURN

prev1 <- price[-length(price)]
price_autoreg_data <- cbind(apple_stock[-1,], prev1)

days <- price_autoreg_data$DAYS
price <- price_autoreg_data$AAPL
spx <- price_autoreg_data$SPX
vix <- price_autoreg_data$VIX
spgscitr <- price_autoreg_data$SPGSCITR
bndglb <- price_autoreg_data$BNDGLB
eem <- price_autoreg_data$EEM
apple_return <- price_autoreg_data$RETURN

price_autoreg_model <- lm(RETURN~., data=price_autoreg_data)

MPSE.price_autoreg_model <- MPSE(price_autoreg_model)

# ------------------------
# MODEL ASSUMPTIONS Check
# ------------------------

mod <- price_autoreg_model
dat <- price_autoreg_data

# Pairwise scatter plots
pairs(dat)

# Raw residuals and studentized residual plots
par(mfrow=c(1,2))
plot(fitted(mod), residuals(mod), main="Raw Residual Plots",
     xlab="Fitted", ylab="Residuals")
abline(0,0)
plot(fitted(mod), rstudent(mod), ylim=c(-5,5), 
     main="Studentized Residaul Plots", xlab="Fitted", ylab="Studentized Residuals")
abline(2,0)
abline(-2,0)

# Partial residual plots
crPlots(m<-mod, main="Partial Residual Plots", 
        ylab="Partial Residuals", smooth=T)


par(mfrow=c(1,1))
qqnorm(residuals(mod))
qqline(residuals(mod), datax=FALSE, distribution=qnorm, 
       probs=c(0.25, 0.75), qtype=7)

# Check Autocorrelation of covariates using ACF graphs
par(mfrow=c(1,1))
acf(residuals(mod), main="Residual ACF", lag.max=10)
## The ACF in the response variable has been explained away by the covariates and there
##   is no evidence of remaining autocorrelation in the residuals. 

# Use the Durbin-Watson Test for further check of autocorrelation
dwtest(mod, alternative=c("two.sided"), data=dat)

##################################################################################
#Automated Model Selection

# Using the Forward Selection method; alpha to add is the 0.15 significant level

alpha <- 0.15
mod <- price_autoreg_model
dat <- price_autoreg_data

forward_model <- lm(apple_return~1, data=dat)
addMatrix <- addterm(forward_model, scope=mod, test="F")
pvalues <- addMatrix[,"Pr(F)"]
while(min(pvalues[-1]) < alpha){
  addition <- rownames(addMatrix[-1,])[which.min(pvalues[-1])]
  print(data.frame(rbind(rownames(addMatrix[-1,]),pvalues[-1])))
  print(addition)
  form <- paste(".~.+", addition, sep="")
  forward_model <- update(forward_model, form)
  addMatrix <- addterm(forward_model, scope=mod, test="F")
  pvalues <- addMatrix[,"Pr(F)"]
}
print(data.frame(rbind(rownames(addMatrix[-1,]),pvalues[-1])))
summary(forward_model)

# Using the Stepwise Regression method ; using the indicator of least AIC
step_model <- step(lm(apple_return~1, data=dat), scope=list(upper=mod), direction="both")

summary(step_model)


# Using the Backward Eliminatiosn method; alpha to add is the 0.15 significant level
alpha <- 0.15
backward_model <- mod
pvalues <- summary(mod)[[4]][,4]
while(max(pvalues[-1]) > alpha){
  elim <- names(which.max(pvalues[-1]))
  print(pvalues[-1])
  print(elim)
  form <- paste(".~.-", elim)
  backward_model <- update(backward_model, form)
  pvalues <- summary(backward_model)[[4]][,4]
}
print(pvalues[-1])

summary(backward_model)

###############################################################################################


summary_table <- rbind(GetModelSummary(initial_model),
                       GetModelSummary(return_autoreg_model),
                       GetModelSummary(simple_price_autoreg_model),
                       GetModelSummary(full_autoreg_model),
                       GetModelSummary(forward_model),
                       GetModelSummary(step_model),
                       GetModelSummary(backward_model))

rownames(summary_table) <- c("Initial Model", "Return AutoReg. Model", "Simple Price AutoReg. Model", "Full AutoReg. Model",
                             "Forward Selection Model", "Stepwise Regression Model", "Backward Elimination Model")

print(summary_table)
