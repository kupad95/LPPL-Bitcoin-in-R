# Define function for sum of squared residuals, to evaluate the fitness of parameters m and omega
residuals <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  return(sum((FittedLPPL(rTicker, lm.result, m, omega, tc) - rTicker$Close) ** 2))
}

#To return individual value of SSR
residvalue <- residuals(m, omega,tc)

#Repeat
mspread <- sapply(seq(0,1,0.01),function(m) residuals(m,omega,tc))
omegaspread <- sapply(seq(0,20,0.1),function(omega) residuals(m,omega,tc))
tcspread <- sapply(seq(2017.011,2017.033,0.0001),function(tc) residuals(m,omega,tc))

#Printing results
mspreadresults <- NULL
cbind(mspreadresults, c(m, mspread)) -> mspreadresults

omegaspreadresults <- NULL
cbind(omegaspreadresults, c(omega, omegaspread)) -> omegaspreadresults

tcspreadresults <- NULL
cbind(tcspreadresults, c(tc, tcspread)) -> tcspreadresults

#DISCARD:
#For finding RMSE and Sensitivity Analysis - Work in Progress
generate_vector = seq(min(rTicker$t), tc-0.002, 0.002)
lm.result <- LPPL(rTicker, m, omega, tc)
final <- FittedLPPLwithexpected(rTicker, lm.result, generate_vector, m, omega, tc)
rooms <- rmse(rTicker$Close,final)

final2 <- FittedLPPLwithexpected(rTicker, lm.result, generate_vector, m, omega+3, tc)
rooms2 <- rmse(rTicker$Close,final2)
rooms2