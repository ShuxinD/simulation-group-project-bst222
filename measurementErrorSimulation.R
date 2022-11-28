# library(dplyr)
library(data.table)

#' health outcome analysis: association between PM2.5 and systolic blood pressure
#' simulate different form of measurement error
#' use 3 regression calibration methods to correct for measurement error
#' see how well the regression calibration methods could recover the true relationship between bp pm2.5

#' try 2 scenarios: the classical error vary by season; the classical error does not vary by season
#' try 3 regression calibration methods: 
#' 1) do 1 regression calibration for sampled data
#' 2) stratified regression calibration (by season); 
#' 3) regression calibration with season as a variable

#' try different sample size (100, 500, 1000 ,2000, 5000)
#' the population size is 10000

rm(list=ls())
gc()

## ideal population data ----
### load pm2.5 data ----
ideal <- readRDS("bst222pm25.rds")
names(ideal)[1] <- "truePM"
setDT(ideal)
### systolic bp ----
ideal$bp <- 5*ideal$truePM + 80 # the relationship is assumed to be 127 + 0.14*pm25 according to systematic review


## SCENARIO 1 ----
#' the measurement error are the same across season
#'  only work on classical measurement error here
set.seed(3)
cesd <- 0.5; cd <- rnorm(10000, mean=0, sd=cesd) # get classical error, change the sd of classical error (try 1,2 and 3?)
ideal$errorPM_scen1 <- ideal$truePM + cd

## simulation for scenario 1
set.seed(13)
sampleSize <- c(100, 500, 1000, 2000)

scen1Result <- data.frame()

coeffMat <- NA
for (siz in 1:4) {
  # matrix for storing the regression coefficients
  coeffMat <- matrix(ncol = 4, nrow = 500)
  for (iter in 1:500) {
    print(iter)
    ## random sampling
    sampS <- sample(c(1:10000), size = sampleSize[siz])
    subDat <- ideal[sampS,]
    ## regression calibration models
    model1 <- lm(truePM~errorPM_scen1, data=subDat)
    model2 <- lm(truePM~errorPM_scen1+as.factor(season), data=subDat)
    model3 <- lm(truePM~errorPM_scen1*as.factor(season), data=subDat)
    ## predict the calibrated PM2.5 from different regression calibration methods
    pmVal <- data.frame(mod1_pred = predict(model1, newdata=ideal), 
                        mod2_pred = predict(model2, newdata=ideal), 
                        mod3_pred = predict(model3, newdata=ideal),
                        ideal$errorPM_scen1)
    
    for (rc in 1:4) {
      # estimate the association between systolic blood pressure and 
      # calibrated pm2.5, true pm2.5 and error-prone pm2.5
      model4 <- lm(ideal$bp ~ pmVal[,rc])
      coeffMat[iter, rc] <- model4$coefficients[2]
    }
  } 
  
  coeff <- as.data.frame(coeffMat)
  colnames(coeff) <-c("regCal1", "regCal2", "regCal3", "errorPM_scene1")
  coeff$sampleSize <- sampleSize[siz]
  scen1Result <- rbind(scen1Result, coeff)
}
View(scen1Result)

# SCENARIO 2: measurement error vary by season (additive only)


oriDat <- data.frame(bp, truePM, season)
scen3Dat <- data.frame()

seasonErr <- c(1, 1.5, 2, 2.5)
slopes <- c(1.9, 0.5, 0.8, 1.2)

for ( s in 1:4) {
  
  sbset <- dplyr::filter(oriDat, season == s)
  error <- rnorm(n = dim(sbset)[1], mean=0, sd = seasonErr[s])
  sbset$errorPM <- sbset$truePM*slopes[s]+error
  scen3Dat <- rbind(scen3Dat, sbset)
  
}



# SCENARIO 2&3: measurement error vary by season (additive error w/wo multiplicative)

oriDat <- data.frame(bp, truePM, season)
scen3Dat <- data.frame()

seasonErr <- c(1, 1.5, 2, 2.5)

# for additive only, change all the slopes to 1
slopes <- c(1.9, 0.5, 0.8, 1.2)

for ( s in 1:4) {
  
  sbset <- dplyr::filter(oriDat, season == s)
  error <- rnorm(n = dim(sbset)[1], mean=0, sd = seasonErr[s])
  sbset$errorPM <- sbset$truePM*slopes[s]+error
  scen3Dat <- rbind(scen3Dat, sbset)
  
}




## simulation for scenario 2&3
set.seed(888)

scen3Result <- data.frame()

for (siz in 1:4) {
  
  coeffMat <- matrix(ncol = 5, nrow = 500)
  
  
  for (iter in 1:500) {
    
    print(iter)
    
    sampS <- sample(c(1:10000), size = sampleSize[siz])
    subDat <- scen3Dat[sampS,]
    
    # regression calibration models
    model1 <- lm(truePM~errorPM, data=subDat)
    model2 <- lm(truePM~errorPM+as.factor(season), data=subDat)
    model3 <- lm(truePM~errorPM*as.factor(season), data=subDat)
    ## predict the calibrated PM2.5 from different regression calibration methods
    pmVal <- data.frame(predict(model1), predict(model2), predict(model3), subDat$truePM, subDat$errorPM)
    
    for (rc in 1:5) {
      # estimate the association between systolic blood pressure and 
      # calibrated pm2.5, true pm2.5 and error-prone pm2.5
      model4 <- lm(subDat$bp~pmVal[,rc])
      coeffMat[iter, rc] <- model4$coefficients[2]
      
      
    }
    
    
  } 
  
  coeff <- as.data.frame(coeffMat)
  colnames(coeff) <-c("regCal1", "regCal2", "regCal3", "truePM", "errorPM")
  coeff$sampleSize <- sampleSize[siz]
  
  scen3Result <- rbind(scen3Result, coeff)
  
  
}


check <- dplyr::filter(scen3Result, sampleSize==2000)
summary(check)







