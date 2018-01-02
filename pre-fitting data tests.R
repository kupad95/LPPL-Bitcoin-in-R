library('MASS')
library('vcd')
library('tseries')

#TEST FOR RANDOM WALK, use estimation window only

adf.test(rTicker$Close, "explosive")

#TEST FOR EXPONENTIAL GROWTH

# estimate the parameters
fit1 <- fitdistr(rTicker$Close, "exponential") 

# goodness of fit test
ks.test(rTicker$Close, "pexp", fit1$estimate) # p-value > 0.05 -> distribution not refused

#JARQUE-BERA TEST
jarque.bera.test(ticker$Close)