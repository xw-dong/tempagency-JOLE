library(tidyverse)
library(haven)
setwd("/Users/dongxinwei/Documents/Research/Temp Agency/Temp-agency/IRR")

fittedfw <- read_dta("fittedfw.dta")

dfactor <- matrix(0, nrow = 6, ncol = 7)

power0 <- list()
for (k in c(1,4,5,6))  power0[[k]] <- 0:59
power0[[2]] <- 0:71
power0[[3]] <- 0:47
fcost0 <- list()
for (k in 1:3)  fcost0[[k]] <- 200000
fcost0[[4]] <- 300000
fcost0[[5]] <- 100000
fcost0[[6]] <- 0
inflat_factor0 <- list()
for (k in 1:5)  inflat_factor0[[k]] <- (4.2+10.9)/70
inflat_factor0[[6]] <- (13.7/4+4.2+10.9)/70

for (k in 1:6) {
  power <- power0[[k]]
  fcost <- fcost0[[k]]
  inflat_factor <- inflat_factor0[[k]]
  
  lf_fitted <- fittedfw %>% 
    select(num_range("lf_fitted", 1:length(power))) %>% 
    as.matrix()
  lw_fitted <- fittedfw %>% 
    select(num_range("lw_fitted", 1:length(power))) %>% 
    as.matrix()
  SV <- fittedfw %>% 
    select(num_range("s", 1:length(power))) %>% 
    as.matrix()
  training <- fittedfw["initialblank"] %>% 
    as.matrix() %>% as.vector()
  initialwage <- fittedfw["cost_noised_ini"] %>% 
    as.matrix() %>% as.vector()
  
  MF <- exp(lf_fitted) - (1+inflat_factor)*exp(lw_fitted)
  for(i in 1:nrow(fittedfw)){
    if(training[i]>0){
      for (j in 1:training[i]) {
        MF[i,j] = 0
      }
    }
  }
  
  MFSV <- MF*SV %>% unname()
  
  # full
  Training_profit <- function(discount_factor){
    DF <- discount_factor^power
    EB <- MFSV %*% DF
    TC <- initialwage*training*(1+inflat_factor) + fcost
    TP <- EB - TC
    abs(mean(TP))
  }
  dfactor[k,1] <- optimize(Training_profit, c(0,1.1), tol = 0.00001)$minimum
  
  # Males
  Training_profit <- function(discount_factor){
    DF <- discount_factor^power
    EB <- MFSV %*% DF
    TC <- initialwage*training*(1+inflat_factor) + fcost
    TP <- EB - TC
    TP <- TP[fittedfw$female==0]
    abs(mean(TP))
  }
  dfactor[k,2] <- optimize(Training_profit, c(0,1.1), tol = 0.00001)$minimum

  # Females
  Training_profit <- function(discount_factor){
    DF <- discount_factor^power
    EB <- MFSV %*% DF
    TC <- initialwage*training*(1+inflat_factor) + fcost
    TP <- EB - TC
    TP <- TP[fittedfw$female==1]
    abs(mean(TP))
  }
  dfactor[k,3] <- optimize(Training_profit, c(0,1.1), tol = 0.00001)$minimum
  
  # Non-univs
  Training_profit <- function(discount_factor){
    DF <- discount_factor^power
    EB <- MFSV %*% DF
    TC <- initialwage*training*(1+inflat_factor) + fcost
    TP <- EB - TC
    TP <- TP[fittedfw$univ==0]
    abs(mean(TP))
  }
  dfactor[k,4] <- optimize(Training_profit, c(0,1.1), tol = 0.00001)$minimum
  
  # Univs
  Training_profit <- function(discount_factor){
    DF <- discount_factor^power
    EB <- MFSV %*% DF
    TC <- initialwage*training*(1+inflat_factor) + fcost
    TP <- EB - TC
    TP <- TP[fittedfw$univ==1]
    abs(mean(TP))
  }
  dfactor[k,5] <- optimize(Training_profit, c(0,1.1), tol = 0.00001)$minimum
  
  
  # 0-5 years
  Training_profit <- function(discount_factor){
    DF <- discount_factor^power
    EB <- MFSV %*% DF
    TC <- initialwage*training*(1+inflat_factor) + fcost
    TP <- EB - TC
    TP <- TP[fittedfw$expentry_long==0]
    abs(mean(TP))
  }
  dfactor[k,6] <- optimize(Training_profit, c(0,1.1), tol = 0.00001)$minimum
  
  
  # 6+ years
  Training_profit <- function(discount_factor){
    DF <- discount_factor^power
    EB <- MFSV %*% DF
    TC <- initialwage*training*(1+inflat_factor) + fcost
    TP <- EB - TC
    TP <- TP[fittedfw$expentry_long==1]
    abs(mean(TP))
  }
  dfactor[k,7] <- optimize(Training_profit, c(0,1.1), tol = 0.00001)$minimum
}

IRR <- 1-(2-1/dfactor)^12 %>% round(digits = 3)
rownames(IRR) <- c("1. Baseline", "2. 6-year Horizon", "3. 4-year Horizon",
                   "4. FC=300,000 JPY", "5. FC=100,000 JPY", "6. Mult=1.265,FC=0")
colnames(IRR) <- c("Full sample", "Males", "Females", "Non-university", "University",
                   "0-5 years", "6+years")

write.csv(IRR, file = "IRRresults.csv")
