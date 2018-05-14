apple_stock <- read.csv("...market_index_clean.csv", 
                        header=TRUE)
apple_stock <- as.data.frame(apple_stock)

days <- apple_stock$DAYS
price <- apple_stock$AAPL
spx <- apple_stock$SPX
vix <- apple_stock$VIX
spgscitr <- apple_stock$SPGSCITR
bndglb <- apple_stock$BNDGLB
eem <- apple_stock$EEM
apple_return <- apple_stock$RETURN

# Apply power transformation to price
apple_stock$AAPL <- price^0.5
price <- apple_stock$AAPL

# Apply power transformation to spx
apple_stock$SPX <- spx^0.5
spx <- apple_stock$SPX

# Apply power transformation to vix
apple_stock$VIX <- vix^-1
vix <- apple_stock$VIX

# Apply power transformation to spgscitr
apple_stock$SPGSCITR <- log(spgscitr)
spgscitr <- apple_stock$SPGSCITR

# Model 1) Simple Price Autoregressive Model:

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

# Model 2) Backward Elimination:
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

mod <- price_autoreg_model
dat <- price_autoreg_data

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


# To label the models:
model1 <-  simple_price_autoreg_model
model2 <- backward_model

# To calculate leverage of model one's response values:

mod1.diag <- ls.diag(model1)
mod1.lev_measure <- 2*(2+1)/length(apple_return)

# The number of high-leverage cases for model1 was calculated using:

leverage1 <- mod1.diag$hat > mod1.lev_measure
highlev1 <- which(leverage1 == TRUE)
high1 <- mod1.diag$hat[seq(1,length(leverage1)) %in% as.character(highlev1)]
sumhighlev1 <- length(highlev1)

# The proportion of high leverage cases in model one was calculated using:
sumhighlev1/length(mod1.diag$hat)

# The maximum leverage for model1 was calculated using:
max(high1)

# To plot Cook's statistics for model one, and find the maximum statistic:
#plot(model1,which=4)
max(mod1.diag$cooks)

# To fit model one without observations 1359, 2442, and 3088:
remove <- c(1359,2442,3088)

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

days <- simple_price_autoreg_data$DAYS[!seq(2,3102) %in% remove]
price <- simple_price_autoreg_data$AAPL[!seq(2,3102) %in% remove]
spx <- simple_price_autoreg_data$SPX[!seq(2,3102) %in% remove]
vix <- simple_price_autoreg_data$VIX[!seq(2,3102) %in% remove]
spgscitr <- simple_price_autoreg_data$SPGSCITR[!seq(2,3102) %in% remove]
bndglb <- simple_price_autoreg_data$BNDGLB[!seq(2,3102) %in% remove]
eem <- simple_price_autoreg_data$EEM[!seq(2,3102) %in% remove]
apple_return <- simple_price_autoreg_data$RETURN[!seq(2,3102) %in% remove]
prev1 <- simple_price_autoreg_data$prev1[!seq(2,3102) %in% remove]

simple_price_autoreg_model2 <- lm(apple_return~price+prev1)
simple_price_autoreg_data2 <- model.frame(simple_price_autoreg_model)

summary(simple_price_autoreg_model2)

# To calculate the MSPE of the new model one:
MSPE1 <- (1/length(days))*sum((apple_return-fitted(simple_price_autoreg_model2))^2)

# To calculate leverage of model two's response values:

mod2.diag <- ls.diag(model2)
mod2.lev_measure <- 2*(4+1)/length(apple_return)

# The number of high-leverage cases for model two was calculated using:

leverage2 <- mod2.diag$hat > mod2.lev_measure
highlev2 <- which(leverage2 == TRUE)
high2 <- mod2.diag$hat[seq(1,length(leverage2)) %in% as.character(highlev2)]
sumhighlev2 <- length(highlev2)

# The proportion of high leverage cases in model two was calculated using:
sumhighlev2/length(mod2.diag$hat)

# The maximum leverage for model two was calculated using:
max(high2)


# To plot Cook's statistics for model two, and find the maximum statistic:
#plot(model2,which=4)
max(mod2.diag$cooks)

# To fit model two without observations 1359, 2442, and 3088:

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

days <- price_autoreg_data$DAYS[!seq(2,3102) %in% remove]
price <- price_autoreg_data$AAPL[!seq(2,3102) %in% remove]
spx <- price_autoreg_data$SPX[!seq(2,3102) %in% remove]
vix <- price_autoreg_data$VIX[!seq(2,3102) %in% remove]
spgscitr <- price_autoreg_data$SPGSCITR[!seq(2,3102) %in% remove]
bndglb <- price_autoreg_data$BNDGLB[!seq(2,3102) %in% remove]
eem <- price_autoreg_data$EEM[!seq(2,3102) %in% remove]
apple_return <- price_autoreg_data$RETURN[!seq(2,3102) %in% remove]
prev1 <- price_autoreg_data$prev1[!seq(2,3102) %in% remove]

price_autoreg_data2 <- as.data.frame(cbind(days,price,spx,vix,spgscitr,bndglb,
                                          eem,apple_return,prev1))

price_autoreg_model2 <- lm(apple_return~.,data=price_autoreg_data2)

mod2 <- price_autoreg_model2
dat2 <- price_autoreg_data2

alpha <- 0.15
backward_model2 <- mod2
pvalues <- summary(mod2)[[4]][,4]
while(max(pvalues[-1]) > alpha){
  elim <- names(which.max(pvalues[-1]))
  print(pvalues[-1])
  print(elim)
  form <- paste(".~.-", elim)
  backward_model2 <- update(backward_model2, form)
  pvalues <- summary(mod2)[[4]][,4]
}

summary(backward_model2)

# To calculate the MSPE of the new model two:
MSPE2 <- (1/length(days))*sum((apple_return-fitted(backward_model2))^2)

# To perform cross-validation for model one:

#library("lattice")
#library("DAAG")

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

# For simplicity, the following R code was left as a comment, because the
# result exceeded an unnecessary number of pages:

# cv.lm(data=simple_price_autoreg_data, form.lm=simple_price_autoreg_model, m= 10, plotit = F)

# However the calculated value of cross-validation over 310 folds is ms=0.000127.

# To perform cross-validation for model two:
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

mod <- price_autoreg_model
dat <- price_autoreg_data

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

# For simplicity, the following R code was left as a comment, because the
# result exceeded an unnecessary number of pages:

#cv.lm(data=price_autoreg_data, form.lm=backward_model, m= 10, plotit = F)

# However the calculated value of cross-validation over 310 folds is also ms=0.000127.

