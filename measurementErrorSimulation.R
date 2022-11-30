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

## set up ----
library(data.table)

rm(list=ls())
gc()

## ideal population data ----
### load pm2.5 data ----
ideal <- readRDS("bst222pm25.rds")
names(ideal)[1] <- "truePM"
setDT(ideal)
### systolic bp ----
ideal$bp <- 0.14*ideal$truePM + 127 # the relationship is assumed to be 127 + 0.14*pm25 according to systematic review


## SCENARIO 1 ----
## measurement errors that are the same across season
### generate error-prone pm25 ----
## only work on classical measurement error here
set.seed(3)
cesd <- 2; cd <- rnorm(10000, mean=0, sd=cesd) # get classical error, change the sd of classical error (try 1,2 and 3?)
ideal$errorPM_scen1 <- ideal$truePM + cd

### simulation ----
sampleSize <- c(100, 500, 1000, 2000)

pmVal <- NULL
coeffMat <- NULL
scen1Result <- data.frame()
n_iter <- 500

for (siz in 1:4) {
  # matrix for storing the regression coefficients
  coeffMat <- matrix(ncol = 4, nrow = n_iter)
  for (iter in 1:n_iter) {
    print(iter)
    ## random sampling
    set.seed(iter)
    sampS <- sample(c(1:10000), size = sampleSize[siz])
    subDat <- ideal[sampS,]
    ## regression calibration models
    model1 <- lm(truePM~errorPM_scen1, data=subDat)
    model2 <- lm(truePM~errorPM_scen1+as.factor(season), data=subDat)
    model3 <- lm(truePM~errorPM_scen1*as.factor(season), data=subDat)
    ## predict the calibrated PM2.5 from different regression calibration methods
    pmVal <- cbind(predict(model1, newdata=ideal), 
                   predict(model2, newdata=ideal), 
                   predict(model3, newdata=ideal),
                   ideal$errorPM_scen1)
    
    for (rc in 1:4) {
      # estimate the association between systolic blood pressure and 
      # calibrated pm2.5, and error-prone pm2.5
      model4 <- lm(ideal$bp ~ pmVal[,rc])
      coeffMat[iter, rc] <- model4$coefficients[2]
    }
  } 
  coeff <- cbind(coeffMat, sampleSize[siz])
  scen1Result <- rbind(scen1Result, coeff)
}
setDT(as.data.frame(scen1Result))
colnames(scen1Result) <-c("regCal1", "regCal2", "regCal3", "errorPM", "sampleSize")
View(scen1Result)
## create results directory
ifelse(dir.exists(file.path("results")), "results dir exists", dir.create(file.path("results")))
## save simulation results
fwrite(scen1Result, file.path("results", "scen1Results.csv"))

## SCENARIO 2.1 additive ----
## additive error
sErr_sd <- c(1, 1.5, 2, 2.5)
### generate error-prone data ----
errorPM_scen21 <- NULL
for (s in 1:4) {
  truePM_s <- ideal[season==s, truePM]
  set.seed(s)
  error <- rnorm(length(truePM_s), mean = 0, sd = sErr_sd[s]) # additive
  errorPM_s <- truePM_s + error
  errorPM_scen21 <- c(errorPM_scen21, errorPM_s)
}
ideal <- cbind(ideal, errorPM_scen21)

### simulation ----
sampleSize <- c(100, 500, 1000, 2000)

pmVal <- NULL
coeffMat <- NULL
scen21Result <- data.frame()
n_iter <- 500

for (siz in 1:4) {
  # matrix for storing the regression coefficients
  coeffMat <- matrix(ncol = 4, nrow = n_iter)
  for (iter in 1:n_iter) {
    print(iter)
    ## random sampling
    set.seed(iter)
    sampS <- sample(c(1:10000), size = sampleSize[siz])
    subDat <- ideal[sampS,]
    ## regression calibration models
    model1 <- lm(truePM~errorPM_scen21, data=subDat)
    model2 <- lm(truePM~errorPM_scen21+as.factor(season), data=subDat)
    model3 <- lm(truePM~errorPM_scen21*as.factor(season), data=subDat)
    ## predict the calibrated PM2.5 from different regression calibration methods
    pmVal <- cbind(predict(model1, newdata=ideal), 
                   predict(model2, newdata=ideal), 
                   predict(model3, newdata=ideal),
                   ideal$errorPM_scen21)
    
    for (rc in 1:4) {
      # estimate the association between systolic blood pressure and 
      # calibrated pm2.5, and error-prone pm2.5
      model4 <- lm(ideal$bp ~ pmVal[,rc])
      coeffMat[iter, rc] <- model4$coefficients[2]
    }
  } 
  coeff <- cbind(coeffMat, sampleSize[siz])
  scen21Result <- rbind(scen21Result, coeff)
}
setDT(as.data.frame(scen21Result))
colnames(scen21Result) <-c("regCal1", "regCal2", "regCal3", "errorPM", "sampleSize")
View(scen21Result)
## create results directory
ifelse(dir.exists(file.path("results")), "results dir exists", dir.create(file.path("results")))
## save simulation results
fwrite(scen21Result, file.path("results", "scen21Results.csv"))

## SCENARIO 2.2 multiplicative and additive ----
## additive error
sErr_sd <- c(1, 1.5, 2, 2.5)
## multiplicative error
sErr_slopes <- c(1.9, 0.5, 0.8, 1.2) # for additive only, change all the slopes to 1
### generate error-prone data ----
errorPM_scen22 <- NULL
for (s in 1:4) {
  truePM_s <- ideal[season==s, truePM]
  set.seed(s)
  error <- rnorm(length(truePM_s), mean = 0, sd = sErr_sd[s]) # additive
  errorPM_s <- truePM_s*sErr_slopes[s] + error # multiplicative
  errorPM_scen22 <- c(errorPM_scen22, errorPM_s)
}
ideal <- cbind(ideal, errorPM_scen22)

### simulation ----
sampleSize <- c(100, 500, 1000, 2000)

pmVal <- NULL
coeffMat <- NULL
scen22Result <- data.frame()
n_iter <- 500

for (siz in 1:4) {
  # matrix for storing the regression coefficients
  coeffMat <- matrix(ncol = 4, nrow = n_iter)
  for (iter in 1:n_iter) {
    print(iter)
    ## random sampling
    set.seed(iter)
    sampS <- sample(c(1:10000), size = sampleSize[siz])
    subDat <- ideal[sampS,]
    ## regression calibration models
    model1 <- lm(truePM~errorPM_scen22, data=subDat)
    model2 <- lm(truePM~errorPM_scen22+as.factor(season), data=subDat)
    model3 <- lm(truePM~errorPM_scen22*as.factor(season), data=subDat)
    ## predict the calibrated PM2.5 from different regression calibration methods
    pmVal <- cbind(predict(model1, newdata=ideal), 
                   predict(model2, newdata=ideal), 
                   predict(model3, newdata=ideal),
                   ideal$errorPM_scen22)
    
    for (rc in 1:4) {
      # estimate the association between systolic blood pressure and 
      # calibrated pm2.5, and error-prone pm2.5
      model4 <- lm(ideal$bp ~ pmVal[,rc])
      coeffMat[iter, rc] <- model4$coefficients[2]
    }
  } 
  coeff <- cbind(coeffMat, sampleSize[siz])
  scen22Result <- rbind(scen22Result, coeff)
}
setDT(as.data.frame(scen22Result))
colnames(scen22Result) <-c("regCal1", "regCal2", "regCal3", "errorPM", "sampleSize")
View(scen22Result)
## create results directory
ifelse(dir.exists(file.path("results")), "results dir exists", dir.create(file.path("results")))
## save simulation results
fwrite(scen22Result, file.path("results", "scen22Results.csv"))

## test ----
# sampleSize <- c(100, 500, 1000, 2000)
# n_iter <- 500
# scen21Result <- NULL
# for (siz in 1:4) {
#   # matrix for storing the regression coefficients
#   coeffMat <- matrix(ncol = 4, nrow = n_iter)
#   for (iter in 1:n_iter) {
#     print(iter)
#     ## random sampling
#     set.seed(iter)
#     sampS <- sample(c(1:10000), size = sampleSize[siz])
#     subDat <- ideal[sampS,]
#     ## regression calibration models
#     model1 <- lm(truePM~errorPM_scen21, data=subDat)
#     ## predict the calibrated PM2.5 from different regression calibration methods
#     pmVal <- predict(model1, newdata=ideal)
#     
#     model4 <- lm(ideal$bp ~ pmVal)
#     coeffMat[iter, ] <- model4$coefficients[2]
#   } 
#   coeff <- cbind(coeffMat, sampleSize[siz])
#   scen21Result <- rbind(scen21Result, coeff)
# }
# scen21Result <- as.data.frame(scen21Result)
# setDT(scen21Result)
# scen21Result[,.(lowCI=quantile(V1,0.025)), by=V5]
