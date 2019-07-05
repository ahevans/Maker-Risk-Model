install.packages("msm")
library(msm)
library(tidyverse)

int_data <- read.csv("intermediate output.csv")

Q <- rbind(c(0,0.8,0.2),
           c(0,0,0),
           c(0,0,0)) #initialization

#processing
int_data$coll_factor <- factor(int_data$coll_factor)
int_data$state <- factor(int_data$state)
levels(int_data$state) <- c(3,1,2)

model_out <- msm(state ~ age,id,data=int_data,qmatrix=Q,deathexact=c(2,3),covariates = list("1-2" = ~ stab_fee,"1-3" = ~coll_factor)) #result

qmatrix.msm(model_out) #output matrix
totlos.msm(model_out) #days spent it each state

###Surival Functions

rate <- qmatrix.msm(model_out)[1,1][1] #rate out of the open state
Days <- seq(0, 500, by=1)
mean_fee <- int_data$stab_fee %>% unique() %>% mean() #mean in dataset
ir <- read_csv("interest rates daily.csv") %>% data.frame()
mean_fee_bydate <-mean(ir$Rate) #mean for unique days

get_rate <- function (fac){
  rate <- qmatrix.msm(model_out,covariates=list(coll_factor=fac,stab_fee=mean_fee_bydate))[1,1][1]
  return(rate)
} #returns transition rate out of open for different collateralization levels

get_rate_fee <- function (stab_fee){
  rate <- qmatrix.msm(model_out,covariates=list(stab_fee=stab_fee))[1,1][1]
  return(rate)
} #returns transition rate out of open for different stability fees

#Survival Function Plots

par(mfrow=(c(1,2)))
plot(Days,exp(Days*get_rate(2)),type="l",col=2,ylab="Survival Probability")
lines(exp(Days*get_rate(1)),col=1)
lines(exp(Days*get_rate(0)),col=3)
legend("topright",legend=c("Above 280%","250%-280%","Below 250%"),title="Collateralization",col=c(3,1,2),lty=1)


plot(Days,exp(Days*get_rate_fee(1.005)),type="l",col=3,ylab="")
lines(exp(Days*get_rate_fee(1.10)),col=1)
lines(exp(Days*get_rate_fee(1.25)),col=2)
legend("topright",1,legend=c("0.5%","10%","25%"),title="Stability Fee",col=c(3,1,2),lty=1)

#Loss Distribution Plots

get_bite_rate <- function (fac){
  rate <- qmatrix.msm(model_out,covariates=list(coll_factor=fac,stab_fee=mean_fee))[1,3][1]
  return(rate)
} #returns the transition rate into the Bitten statte for different collateralization ratios

par(mfrow=(c(1,2)))

plot(0:100,dbinom(0:100,size=100,prob=-get_bite_rate(2)/get_rate(2)),type="l",col=2,ylab="Probability (Density)",xlab="Percentrage Bitten",ylim=c(0,0.2))
lines(dbinom(0:100,size=100,prob=-get_bite_rate(1)/get_rate(1)),col=1)
lines(dbinom(0:100,size=100,prob=-get_bite_rate(0)/get_rate(0)),col=3)
legend("topright",legend=c("Below 250%","250%-280%","Above 280%"),title="Collateralization",col=c(2,1,3),lty=1)

plot(1:100,pbinom(1:100,size=100,prob=-get_bite_rate(2)/get_rate(2)),type="l",col=2,ylab="Probability (Distribution)",xlab="Cumulative Percentrage Bitten")
lines(pbinom(0:100,size=100,prob=-get_bite_rate(1)/get_rate(1)),col=1)
lines(pbinom(0:100,size=100,prob=-get_bite_rate(0)/get_rate(0)),col=3)
#legend("bottomright",legend=c("Below 250%","250%-275%","Above 275%"),title="Collateralization",col=c(2,1,3),lty=1)

