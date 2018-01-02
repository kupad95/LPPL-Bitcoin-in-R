library('MASS')
library('vcd')
library('tseries')
library('readxl')

residuals <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker,m, omega, tc)
  return((FittedLPPL(rTicker, lm.result, m, omega, tc) - rTicker$Close))
}

residvalue <- residuals(m, omega, tc)
partresid <- head(residvalue, n=120)

adf.test(partresid)
pp.test(partresid)
kpss.test(residvalue)

plot(rTicker$t, residvalue, xlab="Date", ylab="Residual", typ='l')

#Tc Residual Check

rm(list=ls())

CLIP <- read_excel("C:/Users/K Upadhyay/Dropbox/Bitcoin/CLIP testing.xlsx")

plot(CLIP$t, CLIP$tc, typ='l')

adf.test(CLIP$tc)
pp.test(CLIP$tc)
kpss.test(CLIP$tc)
