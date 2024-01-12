library (dplyr)
library(tidyr)
library(rmgarch)
library(anytime)
library(timeSeries)
library (fBasics)
library (fExtremes)
library(FRAPO)
library(QRM)
library(copula)
library(moments)
library(tseries)
library(forecast)
library(mvnormalTest)
library(MVN)
library(spd)
library(lmtest)
library(copula)
library(QRM)
library(fitdistrplus)
library(logspline)
library(ggplot2)
library(cowplot)

setwd("yourpath")
at <- read.csv("portfolio_data.csv")
at <- at[,-1]
########Parameters
#tau <- 0.5
#nu <- 3
#nu. <- 3.5
#d <- 4
#th <- iTau(ellipCopula("t", df = nu), tau)
######

# Create a new data frame with the returns of Amazon and Tesla stock prices added as new columns
rt <- at %>% 
  mutate(neg_log_return_AMZN = -1*returnseries(AMZN),
         neg_log_return_TSLA = -1*returnseries(TSLA), neg_log_return_MO = -1*returnseries(MO), neg_log_return_NEM = -1*returnseries(NEM))


rt <- rt[,-c(1, 2, 3, 4, 5)]
rt <- rt[-1,]
Portfolio_historical_returns <- data.frame(historical_portfolio_neglog_returns = rt[,1] + rt[,2] + rt[,3] + rt[,4])
#Plot returns
plot(rt, ylim=NULL)

#Kurtosis----Normality tests
kt <-kurtosis(rt)

densAMZNbase <- plot(density(rt[,"neg_log_return_AMZN"]))
mu <- mean(rt[,1])
sigma <- sd(rt[,1])
x <- seq(min(rt[,1]), max(rt[,1]), length = 100)
lines(x, dnorm(x, mean = mu, sd = sigma), col = "red")

densTESLAbase <- plot(density(rt[,"neg_log_return_TSLA"]))
mu <- mean(rt[,2])
sigma <- sd(rt[,2])
x <- seq(min(rt[,2]), max(rt[,2]), length = 100)
lines(x, dnorm(x, mean = mu, sd = sigma), col = "red")

densMObase <- plot(density(rt[,"neg_log_return_MO"]))
mu <- mean(rt[,3])
sigma <- sd(rt[,3])
x <- seq(min(rt[,3]), max(rt[,3]), length = 100)
lines(x, dnorm(x, mean = mu, sd = sigma), col = "red")

densNEMbase <- plot(density(rt[,"neg_log_return_NEM"]))
mu <- mean(rt[,4])
sigma <- sd(rt[,4])
x <- seq(min(rt[,4]), max(rt[,4]), length = 100)
lines(x, dnorm(x, mean = mu, sd = sigma), col = "red")

mvn_ret = MVN::mvn(rt)

CullenFreyAMZN <- descdist(rt[,1], discrete = FALSE)
CullenFreyTSLA <- descdist(rt[,2], discrete = FALSE)
CullenFreyMO <- descdist(rt[,3], discrete = FALSE)
CullenFreyNEM <- descdist(rt[,4], discrete = FALSE)

# Load the 'moments' package
library(moments)

#CGARCH Model

#########################################################################################################################
spec = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), 
                                        submodel = "GARCH", external.regressors = NULL, variance.targeting = FALSE), 
                  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
                                    archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
                  distribution.model = "std", start.pars = list(), fixed.pars = list())



###Fiting -The higher the adj pearson goodness of fit p-value the better (HO=theoretical distribution matches), here distri is likely to follow a t-distribution

Roll_AMZN <- ugarchroll(spec = spec, data = rt[,"neg_log_return_AMZN"], solver = "hybrid", n.start = 100, refit.every = 50, refit.window = "moving",
                        solver.control = list() )

Roll_TSLA <- ugarchroll(spec = spec, data = rt[,"neg_log_return_TSLA"], solver = "hybrid", n.start = 100, refit.every = 50, refit.window = "moving",
                        solver.control = list() )
Roll_NEM <- ugarchroll(spec = spec, data = rt[,"neg_log_return_NEM"], solver = "hybrid", n.start = 100, refit.every = 50, refit.window = "moving",
                       solver.control = list() )

Roll_MO <-ugarchroll(spec = spec, data = rt[,"neg_log_return_MO"], solver = "hybrid", n.start = 100, refit.every = 50, refit.window = "moving",
                     solver.control = list() )






#Taking the residuals
AMZNres <- (Roll_AMZN@forecast[["density"]][["Realized"]] - Roll_AMZN@forecast[["density"]][["Mu"]])/Roll_AMZN@forecast[["density"]][["Sigma"]]
TSLAres <- (Roll_TSLA@forecast[["density"]][["Realized"]] - Roll_TSLA@forecast[["density"]][["Mu"]])/Roll_TSLA@forecast[["density"]][["Sigma"]]
MOres <- (Roll_MO@forecast[["density"]][["Realized"]] - Roll_MO@forecast[["density"]][["Mu"]])/Roll_MO@forecast[["density"]][["Sigma"]]
NEMres <- (Roll_NEM@forecast[["density"]][["Realized"]] - Roll_NEM@forecast[["density"]][["Mu"]])/Roll_NEM@forecast[["density"]][["Sigma"]]
#New dataframe of simulated returns residuals
dataframe_res <- data.frame(AMZN_res = AMZNres, TSLA_res= TSLAres, NEM_res = NEMres, MO_res = MOres)

###Normality tests
normality_residuals <- MVN::mvn(dataframe_res)

###Density plots
densAMZN <- plot(density(dataframe_res[,"AMZN_res"]))
mu <- mean(dataframe_res[,1])
sigma <- sd(dataframe_res[,1])
x <- seq(min(dataframe_res[,1]), max(dataframe_res[,1]), length = 100)
lines(x, dnorm(x, mean = mu, sd = sigma), col = "red")

densTSLA <- plot(density(dataframe_res[,"TSLA_res"]))
mu <- mean(dataframe_res[,2])
sigma <- sd(dataframe_res[,2])
x <- seq(min(dataframe_res[,2]), max(dataframe_res[,2]), length = 100)
lines(x, dnorm(x, mean = mu, sd = sigma), col = "red")


densMO <- plot(density(dataframe_res[,"MO_res"]))
mu <- mean(dataframe_res[,3])
sigma <- sd(dataframe_res[,3])
x <- seq(min(dataframe_res[,3]), max(dataframe_res[,3]), length = 100)
lines(x, dnorm(x, mean = mu, sd = sigma), col = "red")

densNEM <- plot(density(dataframe_res[,"NEM_res"]))
mu <- mean(dataframe_res[,2])
sigma <- sd(dataframe_res[,2])
x <- seq(min(dataframe_res[,2]), max(dataframe_res[,2]), length = 100)
lines(x, dnorm(x, mean = mu, sd = sigma), col = "red")

qqplot_AMZN = qqnormPlot(dataframe_res[,1], main = "AMZN", title = FALSE)
qqplot_TESLA = qqnormPlot(dataframe_res[,2], main = "TESLA", title = FALSE)
qqplot_MO = qqnormPlot(dataframe_res[,3], main = "MO", title = FALSE)
qqplot_NEM = qqnormPlot(dataframe_res[,4], main = "NEM", title = FALSE)

############Copula Model
pobs <- pobs(dataframe_res)#Copula models based on pseudo-observation (normalized ranked data), copula is a function of uniform margins. 

fitcop <- fitCopula(ellipCopula("t", dim = 4, df.fixed = TRUE, df = 413.5134244966), data = pobs, method = "mpl")


############Simulated residuals




SimulatedCopula <- function(u) {
  length(SimulatedPseudos[SimulatedPseudos[,1]<u[1] & SimulatedPseudos[,2]<u[2],])/(dim(SimulatedPseudos)[2]*dim(SimulatedPseudos)[1])   
}


PseudoScenarios = rCopula(10000,tCopula(param = fitcop@estimate, dispstr = "ex", dim = 4)) # the simulated residuals

library(cramer)
#cramer <- cramer.test(x = PseudoScenarios, y = pobs)
#############Simulated returns
library(gamlss)
library(gamlss.dist)
library(gamlss.add)
fit1=fitDist(rt[,1], k = 4, type = "realline", trace = FALSE, try.gamlss = TRUE)#####Identify the suitable distribution
fit2=fitDist(rt[,2], k = 4, type = "realline", trace = FALSE, try.gamlss = TRUE)
fit3=fitDist(rt[,3], k = 4, type = "realline", trace = FALSE, try.gamlss = TRUE)
fit4=fitDist(rt[,4], k = 4, type = "realline", trace = FALSE, try.gamlss = TRUE)

SimReturnAMZN = qLO(PseudoScenarios[,1],mu=fit1$mu, sigma=fit1$sigma)####Simulated returns of AMAZON
SimReturnTSLA = qLO(PseudoScenarios[,2], mu = fit2$mu, sigma = fit2$sigma)#####Simulated returns of TESLA
SimReturnMO = qLO(PseudoScenarios[,3], mu = fit3$mu, sigma = fit3$sigma)
SimReturnNEM = qLO(PseudoScenarios[,4], mu = fit4$mu, sigma = fit4$sigma)


Sim_port_returns_copula <- data.frame(Portfolio = SimReturnAMZN + SimReturnTSLA + SimReturnMO + SimReturnNEM)
ScenarioPortfolioValues = at[length(at[,"AMZN"]),"AMZN"]*exp(-SimReturnAMZN) + at[length(at[,"TSLA"]),"TSLA"]*exp(-SimReturnTSLA) + at[length(at[,"MO"]),"MO"]*exp(-SimReturnMO) + at[length(at[,"NEM"]),"NEM"]*exp(-SimReturnNEM)
Simulated_copula_prices <- data.frame(
  Asset_price_portfolio = ScenarioPortfolioValues, 
  AMZN_price = at[length(at[,"AMZN"]),"AMZN"]*exp(-SimReturnAMZN), 
  TSLA_price = at[length(at[,"TSLA"]),"TSLA"]*exp(-SimReturnTSLA), 
  MO_price = at[length(at[,"MO"]),"MO"]*exp(-SimReturnMO), 
  NEM_price = at[length(at[,"NEM"]),"NEM"]*exp(-SimReturnNEM)
)

###########POT Model
mrlPlot(Sim_port_returns_copula$Portfolio, umin = 0, umax = 10)
POTfit <- gpdFit(Sim_port_returns_copula$Portfolio, treshold = 9)
#####################Plots
par(mfrow = c(2, 2))
plot(POTfit, which = 1) 
plot(POTfit, which = 2) 
plot(POTfit, which = 3) 
plot(POTfit, which = 4)

#################################Plots also


#plot(POTfit, type = "cdf")
#abline(v = POTfit$treshold, lty = 2, col = "red")


#################Simulated distribution


GPD_Simbis <- gpdSim(model = list(xi = POTfit@fit[["par.ests"]][["xi"]], beta = POTfit@fit[["par.ests"]][["beta"]], mu = 0), n = 1358, seed = NULL)

# Histogram of simulated data
hist(GPD_Simbis$GPD,
     freq = F,
     main = paste("Simulated Left-Tail Density Histogram"),
     ylim = c(0, 0.2),
     xlim = c(0, 30),
     xlab = c("estimated daily loss"),
     breaks = 100,
     col = "cornflowerblue",
     panel.first = grid() )
mtext(side = 3,"based on 1358 simulations", line = 0.2)
box() 


##########Daily Roll-VaR
Window_Length = 100


VaR_95_Roll=c()
idx=1
for(t in (Window_Length+1):length(GPD_Simbis$GPD)){
  VaR_95_Roll[idx]=quantile(GPD_Simbis$GPD[(t-Window_Length):(t-1)], 0.95)   #These are also relative VaRs
  idx=idx+1
}


Overshootings_Rolling <- Portfolio_historical_returns[,"historical_portfolio_neglog_returns"] - VaR_95_Roll
num_positives_Rolling <- sum(Overshootings_Rolling > 0)
positive_values_Rolling <- Overshootings_Rolling[Overshootings_Rolling > 0]
AVG_magnitude = mean(positive_values_Rolling)











#####Comparison with delta-normal VaR##############################################
weights <- data.frame(
  AMZN = 0.25 * at[1, "AMZN"] / (0.25 * at[1, "AMZN"] + 0.25 * at[1, "TSLA"] + 0.25 * at[1, "MO"] + 0.25 * at[1, "NEM"]),
  TSLA = 0.25 * at[1, "TSLA"] / (0.25 * at[1, "AMZN"] + 0.25 * at[1, "TSLA"] + 0.25 * at[1, "MO"] + 0.25 * at[1, "NEM"]),
  MO = 0.25 * at[1, "MO"] / (0.25 * at[1, "AMZN"] + 0.25 * at[1, "TSLA"] + 0.25 * at[1, "MO"] + 0.25 * at[1, "NEM"]),
  NEM = 0.25 * at[1, "NEM"] / (0.25 * at[1, "AMZN"] + 0.25 * at[1, "TSLA"] + 0.25 * at[1, "MO"] + 0.25 * at[1, "NEM"])
)

weights_bis <- as.numeric(weights)

Vector_expected_returns <- data.frame(vector_AMZN = mean(rt[,1]), vector_TSLA = mean(rt[,2]), vector_MO = mean(rt[,3]), vector_NEM = mean(rt[,4]))

Mean_returns_portfolio =  data.frame(Portfolio_mean_neglog_return = mean(rt[,"neg_log_return_AMZN"]) * weights[,"AMZN"]
                                     +                                   + mean(rt[,"neg_log_return_TSLA"]) * weights[,"TSLA"] +
                                       +                                      mean(rt[,"neg_log_return_MO"]) * weights[,"MO"] +
                                       +                                      mean(rt[,"neg_log_return_NEM"]) * weights[,"NEM"])

covariance_matrix <- cov(rt)


Portfolio_variance <- function(weights_bis, covariance_matrix) {
  t_weights <- t(weights_bis)
  portfolio_variance <- as.numeric(t_weights %*% covariance_matrix %*% weights_bis)
  return(portfolio_variance)
}


stdev_portfolio <- sqrt (Portfolio_variance(weights_bis, covariance_matrix))

VaR_Delta_Normal = -(Mean_returns_portfolio-qnorm(0.95)*stdev_portfolio)
names(VaR_Delta_Normal) <- "VaR"

Deltalist <- c(rep(VaR_Delta_Normal$VaR, 1258))


Overshootings_Delta_Normal <- Portfolio_historical_returns[,"historical_portfolio_neglog_returns"] - VaR_Delta_Normal$VaR
num_positives_Delta_Normal <- sum(Overshootings_Delta_Normal > 0)
positive_values_Delta_Normal <- Overshootings_Delta_Normal[Overshootings_Delta_Normal > 0]




#################Hist-Sim VaR


WindowLength <- 100

VaR_95_HistSim <-c()
for (i in (WindowLength+1):length(Portfolio_historical_returns$historical_portfolio_neglog_returns)){
  VaR_95_HistSim[i-WindowLength] <- quantile(Portfolio_historical_returns$historical_portfolio_neglog_returns[(i-WindowLength):(i-1)], 0.95)
}

sum(1*(Portfolio_historical_returns$historical_portfolio_neglog_returns[(WindowLength+1):length(Portfolio_historical_returns$historical_portfolio_neglog_returns)]> VaR_95_HistSim))

VaR_95_HistSim <- c(rep(0, Window_Length), VaR_95_HistSim)


####################All methods graph
qplot(x = 1:length(Portfolio_historical_returns$historical_portfolio_neglog_returns),
      y = VaR_95_Roll,
      geom = "line",
      color = "VaR_95_Roll") +
  geom_line(y = Deltalist, color = "green") +
  geom_line(y = Portfolio_historical_returns$historical_portfolio_neglog_returns,
            color = "lightgrey") +
  geom_line(y = VaR_95_HistSim,
            color = "blue") +
  labs(x = "", y = "VaR") +
  scale_color_manual(values = c("VaR_95_Roll" = "red", "VaR_95_HistSim" = "blue"),
                     labels = c("VaR_95_Roll", "VaR_95_HistSim")) +
  theme_minimal() +
  coord_cartesian(ylim = c(min(VaR_95_Roll, Portfolio_historical_returns$historical_portfolio_neglog_returns, VaR_95_HistSim),
                           max(VaR_95_Roll, Portfolio_historical_returns$historical_portfolio_neglog_returns, VaR_95_HistSim)))






