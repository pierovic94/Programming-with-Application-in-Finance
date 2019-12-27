#Programming with Application in Finance

# data manipulation
rm(list=ls())
getwd()
#setwd("")
NHY_daily <- read.csv("NHY.OL.csv", header = T, dec=".")
NHY_monthly <- read.csv("NHY.OL_Monthly.csv", header = T, dec = ".")
View(NHY_daily)
View(NHY_monthly)

# check the class of $Date
class(NHY_daily$Date)
class(NHY_monthly$Date)
# change the class from factor to Date
NHY_daily$Date <- as.Date.factor(NHY_daily$Date) 
NHY_monthly$Date <- as.Date.factor(NHY_monthly$Date)

# convert the data type from factor to numeric
NHY_daily$Adj.Close <- as.numeric(as.character(NHY_daily$Adj.Close)) 
#eliminate the NAs value from the Data.frame
NHY_daily <- na.omit(NHY_daily)
summary(NHY_daily)

class(NHY_monthly$Adj.Close)
summary(NHY_monthly)

head(NHY_daily)
head(NHY_monthly)

# Part 1. For each sampling frequency:
# calculate simple returns (r.daily, r.monthly);
# compute the annualized means (mu.daily, mu.monthly); 
# annualized standard errors (se.daily, se.monthly);
# 95% confidence intervals (ci.daily, ci.monthly).

# compute the daily/monthly simple return and attach the vector to the data frame
NHY_daily$r <- c(NA,diff(NHY_daily$Adj.Close)/NHY_daily$Adj.Close[-length(NHY_daily$Adj.Close)]) 
NHY_monthly$r <- c(NA,diff(NHY_monthly$Adj.Close)/NHY_monthly$Adj.Close[-length(NHY_monthly$Adj.Close)])

# compute the average daily simple return
r.daily <- mean(NHY_daily$r, na.rm = T)    
r.daily

# compute the average monthly simple return
r.monthly <- mean(NHY_monthly$r, na.rm = T)  
r.monthly

# calculate annualized daily return
mu.daily <- r.daily*250                     
mu.daily
# calculate annualized monthly return
mu.monthly <- r.monthly*12                  
mu.monthly

# plot the time series
plot(NHY_daily$Date, NHY_daily$Adj.Close, type="l")       
plot(NHY_monthly$Date, NHY_monthly$Adj.Close, type="l")

# compute the standard deviation of the daily return
sd.r <- sd(NHY_daily$r, na.rm = T)
# compute the standard deviation of the monthly return
sd.r1 <- sd(NHY_monthly$r, na.rm = T)

# omit NA values in the daily return
r <- na.omit(NHY_daily$r)
# n number of observations
n <- length(r)
#compute the daily Standard Error
se.daily <- sd.r*sqrt(250)/sqrt(n)          
se.daily

# omit NA values in the monthly return
r1 <- na.omit(NHY_monthly$r)
# number of observations
n1 <- length(r1)    
# compute the monthly Standard Error
se.monthly <- sd.r1*sqrt(12)/sqrt(n1)       
se.monthly

# 95% confidence probability
p <- 0.95
# Equation (6)
z <- -qnorm((1-p)/2) 

ci.daily <- c(mu.daily-z*se.daily, mu.daily+z*se.daily)
ci.daily
ci.monthly <- c(mu.monthly-z*se.monthly, mu.monthly+z*se.monthly)
ci.monthly

# Empirically, according to the Fiancial Theories monthly return has less noise, 
#therefore it is closer to normal distribution than daily return. However, by looking at the chart below the Monthly data
#has slight more picked observation than Daily, as well the SD is lower for daily than monthly. The daily CI is narrowed than monthly.
plot(NHY_daily$Date, NHY_daily$r)
plot(NHY_monthly$Date, NHY_monthly$r)
# Part 2. Estimate the implied volatility of a put option on Norsk Hydro (Ticker: NHY9R38) from the bid and ask quotes on January 22, 2019.
# plot the return 


# estimate implied volatility of a put option
# get ADJ Close Price(in column 6) on 2019-01-22
S0 <- as.numeric(NHY_daily[NHY_daily$Date=="2019-01-22",6]) 
S0
# midquote
P.mrkt <- mean(c(3.05,3.70))
P.mrkt
# strike price of the put option
K <- 38
# maturity date
T <- - as.numeric((as.Date("2019-01-22")- as.Date("2019-06-21"))/365)
# Since the The Maturity is closer to the 0.4 Maturity of the Z4, we use as risk free the 0.4 discounted rate of interest, compute as follow (FV/PV)^(1/T)-1
rf <- (1/0.9920)^(1/0.4)-1

# Equation 32 Black Scholes
Put <- function(S0,sigma,K,rf,T){
  d1=(log(S0/K)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
  d2=d1-sigma*sqrt(T)
  p=K*exp(-rf*T)*pnorm(-d2)-S0*pnorm(-d1)
  return(p)
}
Put(S0,sigma = 0.2,K,rf,T)
Put(S0,sigma = 0.185,K,rf,T)

# Equation 33 [P.market-P.model]^2
obj.f <- function(sigma,P.mrkt,S0,K,rf,T){
  P.model <- Put(S0,sigma,K,rf,T)
  eps=(P.model-P.mrkt)^2 #error term
  return(eps)
}

obj.f(0.2,P.mrkt,S0,K,rf,T)
obj.f(0.19,P.mrkt,S0,K,rf,T)

# minimize obj.function based on Equation 34
# find the option volatility
# 0.2 starting value
res <- nlm(obj.f,0.2, P.mrkt=P.mrkt, S0=S0, K = K, rf = rf, T = T) 
sigma <- res$estimate #implied option volatility
sigma

# Part 3. price a down-and-in barrier call option on Norsk Hydro based on market information obtained for January 22, 2019.
# The barrier and strike price, respectively, shall be b = K = 32.

# time step
dt<-0.1
# maturity
T_expire<-0.5
t<-seq(dt, T_expire , by=dt)
# number of simulations
n <- 10000
#Present value Zero Cupon Bonds
ZCB_T0 <-c(1, 0.9995,0.9980, 0.9955,0.9920, 0.9876)  

#strike price
K<-32  
#barrier
b<-32

# Using regular Monte-Carlo simulation compute:
# Simulate paths of stock prices. (Assign this result to a variable called S)
# create an empty vector
ZCB.empty <- c()
# simple for loop to create Zi/Zi+1 values
for(i in (1:5)){                            
  ZCB.empty[i] <- as.numeric(ZCB_T0[i]/ZCB_T0[i+1])
}
ZCB <- ZCB.empty     
ZCB

simulate.paths.fast <- function(S0, ZCB, sigma, dt, T_expire, n, t){
  m <- length(t)
  # create matrix and "scaling" it up with ZCB base on equation 3 in the assignment pdf
  e <- matrix(exp(-0.5*(sigma^2)*(dt) + sqrt(dt)*sigma*rnorm(n*m)), m, n)
  e <- e * ZCB
  # Compute comulative product of each column
  S <- apply(e, 2, cumprod)
  S <- S * S0
  S <- rbind(S0, S)
  return(S)
}
set.seed(1)
S <- simulate.paths.fast(S0, ZCB, sigma, dt, T_expire, n, t)

plot(S[,1],type = "l")
abline(h=b)

# Compute the discounted payoffs of each scenario. (P)
# discount factor
disc.factor<-0.9876
# b. Payoff function:
payoffs <- function(S, K, b, rf, t){
  # IF any find price under the barrier -> option activates(I = 0, 1)
  I <- apply(S<=b, 2, any)       
  S <- I * pmax(S[nrow(S),] - K, 0)  
  # Discounting by the 0.5 ZCB price
  S <- S * disc.factor             
  return(S)
}

P <- payoffs(S, K, b, rf, t)
P[1:30]

# Compute the Monte Carlo estimator of the option price. (V)
# c.
V <- mean(P)
V

# Compute the standard error of the Monte Carlo estimator. (se)
# d.
se <- sd(P)/sqrt(length(P))
se

# Compute a 95% confidence interval of the Monte Carlo estimator. (ci)
# e.
alpha <- 0.95
z <- -qnorm((1-alpha)/2)
ci <- c(V - z * se, V + z * se)
ci

# Using Monte-Carlo simulation and antithetic variates compute:
# Simulate paths of stock prices. (Assign this result to a variable called S.as)
simulate.paths.fast.as<-function(S0,ZCB,sigma,dt,T_expire,n){
  t<-seq(dt, T_expire, by=dt)
  m<-length(t)
  z<-rnorm(n*m)
  z.as<- -z
  Z<-matrix(c(z,z.as), m, n*2)
  e<-ZCB*exp((-0.5*sigma^2)*dt+sigma*sqrt(dt)*Z)
  S<-S0*apply(e,2,cumprod)
  S<-rbind(S0,S)
  return(S)
}
n<-10000
set.seed(1)
# number of paired path
n<-n/2
S.as<-simulate.paths.fast.as(S0,ZCB,sigma,dt,T_expire,n)

plot(S.as[,1], type = "l")
abline(h=b)


# Compute the discounted payoffs of each scenario. (P.as)
# b.
P.as <- payoffs(S=S.as, K, b, rf, t)
head(P.as)

# Compute the Monte Carlo estimator of the option price. (V.as)
# c.
V <- mean(P.as)
V

# Compute the standard error of the Monte Carlo estimator. (se.as)
# d.
se.as <- sd(P)/sqrt(length(P.as))
se.as

# Compute a 95% confidence interval of the Monte Carlo estimator. (ci.as)
# e.
ci.as <- c(V - z * se.as, V + z * se.as)
ci.as

# check if antithetic sampling improve the accuracy of the estimate
# f.
se>se.as
se-se.as
ci
ci.as
ci>ci.as
cor(P.as[1:n], P.as[(n + 1):(2 * n)])

#Since the Standard ERROR serves as a measure of variation for random variables, providing a measurement for the spread. 
#The smaller the spread, the more accurate the dataset. 
#However, the "se.as" is not so much smaller than the "se" - the improvement is not much, which is also evident by comparing confidence intervals.
#Moreover, when we check the correlation of payoff function, we get the values to be negative but close to zero.Thus the antithetic model doesn't improve the sampling accuracy significantly in this case.