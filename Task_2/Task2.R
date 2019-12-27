

getwd()
#setwd("")

rm(list=ls())
install.packages("quadprog")
library(quadprog)
library(Matrix)
load("FIE450-Assignment-2.RData") 

sapply(list(rf,sp500,stocks), class) # indentify the classes 

summary(stocks) # summary statistics 
head(rf)
tail(rf)

head(sp500)
tail(sp500)

head(stocks)
tail(stocks)

plot(rf)
plot(sp500)
plot(stocks)

#********************************************************************TASK_1
# reshape the Data  frame stocks given 
stocks<-as.data.frame(reshape(stocks, v.names = "R", idvar = "date", timevar = "id", direction = "wide"))
summary(stocks)

#forward-looking notation. A return at month t refers to the period t to t + 1. We use the same setup for Risk Free.
df<-rf[rf$date>="2009-01-31" &rf$date<="2018-05-31",]
# compute excess return for index SP500
df$Ex.R.Mrkt<-sp500[sp500$date>="2009-01-31" &sp500$date<="2018-05-31",2]-df$rf

dim(df)
# compute excess return of single constituents
ex.stocks<-as.data.frame(stocks[,2:565]-df$rf)
# combine data frames
df<-cbind(df,ex.stocks)
fix(df)

#********************************************************************TASK_2

df2<- df[order(df$date, decreasing=T ),]
df2<-df2[1:60,]
df2<-df2[order(df2$date, decreasing=F),]

df2<-df2[,colSums(is.na(df2))==0]  #if any NA in column drop the column

# 1. Estimate the expected excess return of the stocks (mu2)

mu2<-apply(df2[, -(1:3)], 2, mean) * 12  #annualized
mu2[mu2 < 0] <- 0  #no expected return negative

# 2. Estimate the covariance matrix (Sigma2)

Sigma2 <- cov(df2[, -(1:3)], use = "pairwise.complete.obs") * 12

Sigma2 <- nearPD(Sigma2)$mat
Sigma2 <- as.matrix(Sigma2)

# 3. Estimate the optimal portfolio (omega2)
n<-length(mu2)
A<-t(rbind(1, mu2, diag(1, n), diag(-1, n)))
b0<-c(1,0.05, rep(0,n), rep(-0.1,n))
d<-rep(0, n)
res<-solve.QP(Dmat=Sigma2, dvec=d, Amat = A, bvec = b0, meq = 2) #meq= 2 strong equality first 2 constraints (1 and 0.05)

omega2<-res$solution
summary(omega2)
sum(omega2)
hist(omega2) # distribution 
# check if 5% contrain is respected
t(omega2)%*%as.matrix(mu2)#matrix multiplication

# 4. Compute the minimum and maximum weight of the optimal portfolio (min.weight2, max.weight2)
min.weight2<-omega2[which.min(omega2)]
min.weight2
max.weight2<-omega2[which.max(omega2)]
max.weight2

# 5. Compute the annualized standard deviation of the portfolio (sigma.p2)
sigma.p2<-sqrt(t(omega2)%*%Sigma2%*%as.matrix(omega2) )
sigma.p2
# 6. Compute the annualized Sharpe ratio (sr.p2)
sr.p2<-0.05/sigma.p2
sr.p2


#********************************************************************TASK_3
j<-length(df$date)-60
R.p3<-c()
n.p3<-c()

R<-df[,-(2:3)]
Ri<-df[,1:3]
Ri$rf<-NULL
save(R, file="stock-R.RData" )
save(Ri, file="index-R.RData")
for (i in 1:j){
  rm(R,Ri)
  load("stock-R.RData")
  load("index-R.RData")
  # Select datapoints 1 to 61 (i to 60+i), where we only use datapoints 1 to 60 for determining the
  # optimal portfolio weights, and the 61st (60+i) observation to compute the realized return.
  
  # The stocks need to be traded at the 61st (60+i) point in time
  not.NA<-!is.na(R[R$date==R$date[60+i],])
  R<-R[,not.NA]
  R<-R[(i):(60+i),]
  
  # Index excess return is called 'Ri'.
  # We only need observation 1:60 (i to 59+i) to run the regression
  Ri<-Ri[(i):(59+i),]
  # For each round consider only stocks in your estimation that had at least 20 return observations.
  liq<-apply(R,2, function(v) (sum(!is.na(v))-1))
  R<-R[, liq>=20]
  
  # Estimating the single-factor model
  reg<-apply(R[1:60,-1], 2, function(v){
    res<-lm(v~Ri$Ex.R.Mrkt)
    c(coefficients(res), var(residuals(res)))
  })
  rownames(reg)<-c("alpha", "beta", "var.eps")
  
  alpha <- reg[1, ] #*12
  beta <- reg[2, ]
  var.eps <- reg[3, ] #*12
  mu.index <- mean(Ri$Ex.R.Mrkt) #*12
  var.index <- var(Ri$Ex.R.Mrkt) #*12
  mu <- alpha + beta * mu.index
  Sigma <- var.index * (as.matrix(beta) %*% beta)
  diag(Sigma) <- diag(Sigma) + var.eps
  summary(mu)
  
  # We are not adjusting the expected return estimates, 
  # if they are negative we don't reset them to zero. As we so in the lecture 
  # during the explanation of the single index model 
  
  
  # optimization problem Minimum-Variance Portfolio 
  
  A  <- as.matrix(rep(1,length(mu)))
  b0 <- c(1)
  d  <- c(rep(0,length(mu)))
  
  res <- solve.QP(Dmat = Sigma, dvec = d, Amat = A, bvec = b0, meq = 1)
  omega <- res$solution
  
  # Return we generate when holding the portfolio for one month with the given weights:
  R.p3 = c(R.p3, sum(omega*R[61,-1]))
  
  # amount of firms we are going to hold in our portfolio in a vector.
  n.p3 = c(n.p3, length(R)-sum(omega==0))
}


plot(R.p3, type = "l")# plot return series that the optimal minimum variance strategy generates


# 2. Compute the annualized mean of this strategy. (mu.p3)

mu.p3 = mean(R.p3)*12
mu.p3


# 3. Compute the annualized standard deviation of this strategy. (sigma.p3)

sigma.p3 = sd(R.p3)*sqrt(12)
sigma.p3


# 4. Compute the annualized Sharpe ratio of this strategy. (sr.p3)

sr.p3 = mu.p3/sigma.p3
sr.p3


# 5. Compute the time-series of the value of the portfolio if you invested NOK 1 at the
#    beginning of the sample period. (V.p3)

V.p3 = 1*cumprod((1+R.p))
plot(V.p3, type = "l")

# 6. Compute the time-series of the number of frms within you portfolio. (n.p3)
plot(n.p3, type="l") #plot time-series of the number of frms within you portfolio.
n.p3

#********************************************************************TASK_4
performance <- ex.stocks[-113,]  ## copy of original and drop the last 2 observation due to time window  t-12 to t-2 + hold 1 period
price <- performance         ## we will need 2 dfs, one for computing sum another to add values
for(i in 13: nrow(price)){
  for (j in 1 : ncol(price)){
    ## compute cum returns of stocks in range t-12 to t-2 (hold for one period)
    
    performance[i,j] = prod(1+price[(i-12):(i-2),j]) # prod (1+ Return (TIME WINDOW))
    
  }
}


price <- price[-(1:12),]  ## removing observations prior to t0
performance <- performance[-(1:12), ]         ## removing observations prior to t0
colnames(price) <- c(1:length(price[1,]))   ## defining col names
colnames(performance) <- c(1:length(performance[1,]))     ## col names equal in both matrices
## and now we have one matrix with returns for last 12 days for each periode
## and a matrix with stock returns.
## We will now create the loop which sorts values in periode returns while we will pull out 
## single return for holding periode and add them together in a vector 
## we will also adjust returns so 50% will go on top stocks and 50% on bot

all.returns <- c()        ## vector with period returns of the portfolio
for (i in 1:nrow(performance)) {
  
  perform <- performance[1,]
  perform <- perform[,!is.na(perform)]
  perform <- perform[, order(perform[1, ], decreasing = T)]   ## sorting matrix by 1st row
  
  buffer <- price[,colnames(perform)] ## switching to single returns with sorted col
  buffer <- as.numeric(buffer[1, ])   ## pull out vector with returns
  
  ## weight proportionally the single stocks 
  ## and removing values between top 50 and bot 50
  buffer <- buffer[-(51:(length(buffer)-50))]  
  long <- buffer[1:(length(buffer)*0.5)]    ## stocks go long
  short <- buffer[(length(long) + 1): length(buffer)] ## stocks go short
  ## we are dropping NAs for the company we have previosly selected 
  ## which dont have any return observations for that specific period! see below:
  long <- long[!is.na(long)]        ## drop NAs
  short <- short[!is.na(short)]     ## drop NAs
  
  all.returns <- c(all.returns, (0.5*mean(long)-0.5*mean(short)))    ## adding sum to an empty vector
  performance <- performance[-1, ]    ## remove rows of returns
  price <- price[-1, ] ## remove rows of returns
}

R.p4 <- all.returns    ## exclude last return given reversal period of 1 month

# 2. Compute the annualized mean of this strategy (mu.p4)
mu.p4<-mean(R.p4)*12

# 3. Compute the annualized standard deviation of this strategy (sigma.p4)
sigma.p4<-sd(all.returns)*sqrt(12)

# 4. Compute the annualized Sharpe ratio of this strategy (sr.p4)
sr.p4<-(mu.p4)/(sigma.p4)

# 5. Compute the time-series of the value of the portfolio if you invested 1 kronor at the
#beginning of the sample period (V.p4)
V.p4<-1*cumprod(1+all.returns)
plot(V.p4, type="l")



