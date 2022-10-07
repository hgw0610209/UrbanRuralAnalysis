
library(rstan)

load(file = "raw_data_annual.RData")

#take the log for observations and also modelled grid data
data <- raw_data_annual
data$site_type.x[data$site_type.x=="Suburban Industrial"] <- "Urban Industrial"

data[,2:21] <- log(data[,2:21])
data[,45:74] <- log(data[,45:74])

# delete the NA from covariate (temperature)
data <- data[!is.na(data$gridtemperature_2019),]

## Fit the model and save the coefficients
NO2model2013 <- lm(no2_2013~factor(site_type.x)+gridno2_2013+gridtemperature_2013,data=data)
NO2model2014 <- lm(no2_2014~factor(site_type.x)+gridno2_2014+gridtemperature_2014,data=data)
NO2model2015 <- lm(no2_2015~factor(site_type.x)+gridno2_2015+gridtemperature_2015,data=data)
NO2model2016 <- lm(no2_2016~factor(site_type.x)+gridno2_2016+gridtemperature_2016,data=data)
NO2model2017 <- lm(no2_2017~factor(site_type.x)+gridno2_2017+gridtemperature_2017,data=data)
NO2model2018 <- lm(no2_2018~factor(site_type.x)+gridno2_2018+gridtemperature_2018,data=data)
NO2model2019 <- lm(no2_2019~factor(site_type.x)+gridno2_2019+gridtemperature_2019,data=data)

## Extract response and design matrix
frame <- try(suppressWarnings(model.frame(NO2model2013, data=data, na.action=na.pass)), silent=TRUE)
Y1_2013<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2014, data=data, na.action=na.pass)), silent=TRUE)
Y1_2014<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2015, data=data, na.action=na.pass)), silent=TRUE)
Y1_2015<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2016, data=data, na.action=na.pass)), silent=TRUE)
Y1_2016<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2017, data=data, na.action=na.pass)), silent=TRUE)
Y1_2017<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2018, data=data, na.action=na.pass)), silent=TRUE)
Y1_2018<-model.response(frame)

frame <- try(suppressWarnings(model.frame(NO2model2019, data=data, na.action=na.pass)), silent=TRUE)
Y1_2019<-model.response(frame)


## Fit the model and save the coefficients
PM10model2013 <- lm(pm10_2013~factor(site_type.x)+gridpm10_2013+gridtemperature_2013,data=data)
PM10model2014 <- lm(pm10_2014~factor(site_type.x)+gridpm10_2014+gridtemperature_2014,data=data)
PM10model2015 <- lm(pm10_2015~factor(site_type.x)+gridpm10_2015+gridtemperature_2015,data=data)
PM10model2016 <- lm(pm10_2016~factor(site_type.x)+gridpm10_2016+gridtemperature_2016,data=data)
PM10model2017 <- lm(pm10_2017~factor(site_type.x)+gridpm10_2017+gridtemperature_2017,data=data)
PM10model2018 <- lm(pm10_2018~factor(site_type.x)+gridpm10_2018+gridtemperature_2018,data=data)
PM10model2019 <- lm(pm10_2019~factor(site_type.x)+gridpm10_2019+gridtemperature_2019,data=data)

## Extract response and design matrix
frame <- try(suppressWarnings(model.frame(PM10model2013, data=data, na.action=na.pass)), silent=TRUE)
Y2_2013<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2014, data=data, na.action=na.pass)), silent=TRUE)
Y2_2014<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2015, data=data, na.action=na.pass)), silent=TRUE)
Y2_2015<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2016, data=data, na.action=na.pass)), silent=TRUE)
Y2_2016<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2017, data=data, na.action=na.pass)), silent=TRUE)
Y2_2017<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2018, data=data, na.action=na.pass)), silent=TRUE)
Y2_2018<-model.response(frame)

frame <- try(suppressWarnings(model.frame(PM10model2019, data=data, na.action=na.pass)), silent=TRUE)
Y2_2019<-model.response(frame)


# the following is used for multipollutant model 

X1_2013<-model.matrix(NO2model2013,na.action=na.pass)
X1_2014<-model.matrix(NO2model2014,na.action=na.pass)
X1_2015<-model.matrix(NO2model2015,na.action=na.pass)
X1_2016<-model.matrix(NO2model2016,na.action=na.pass)
X1_2017<-model.matrix(NO2model2017,na.action=na.pass)
X1_2018<-model.matrix(NO2model2018,na.action=na.pass)
X1_2019<-model.matrix(NO2model2019,na.action=na.pass)


X2_2013<-model.matrix(PM10model2013,na.action=na.pass)
X2_2014<-model.matrix(PM10model2014,na.action=na.pass)
X2_2015<-model.matrix(PM10model2015,na.action=na.pass)
X2_2016<-model.matrix(PM10model2016,na.action=na.pass)
X2_2017<-model.matrix(PM10model2017,na.action=na.pass)
X2_2018<-model.matrix(PM10model2018,na.action=na.pass)
X2_2019<-model.matrix(PM10model2019,na.action=na.pass)


data_obs_raw <- cbind(c(Y1_2013,Y1_2014,Y1_2015,Y1_2016,Y1_2017,Y1_2018,Y1_2019),
                      c(Y2_2013,Y2_2014,Y2_2015,Y2_2016,Y2_2017,Y2_2018,Y2_2019))
data_obs <- cbind(c(Y1_2013,Y1_2014,Y1_2015,Y1_2016,Y1_2017,Y1_2018,Y1_2019),
                  c(Y2_2013,Y2_2014,Y2_2015,Y2_2016,Y2_2017,Y2_2018,Y2_2019))
data_cov <- array(NA, c(ncol(data_obs),nrow(data_obs),ncol(X1_2013)))
data_cov[1,,] <- rbind(X1_2013,X1_2014,X1_2015,X1_2016,X1_2017,X1_2018,X1_2019)
data_cov[2,,] <- rbind(X2_2013,X2_2014,X2_2015,X2_2016,X2_2017,X2_2018,X2_2019)

# Extract the missing values into a VECTOR
dat_complete <- data_obs[!is.na(data_obs)]

# Extract the missing and present values as MATRICES
ind_pres <- which(!is.na(data_obs), arr.ind = TRUE)
ind_miss <- which(is.na(data_obs), arr.ind = TRUE)


# run the stan pollution model --------------------------------------------

mod.data <- list(K=ncol(X1_2013),
                 Nrow = nrow(data_obs),
                 Ncol = ncol(data_obs),
                 Ncomp = length(dat_complete),
                 Nmiss = sum(is.na(data_obs)),
                 dat_complete = dat_complete,
                 ind_pres = ind_pres,
                 ind_miss = ind_miss
                 ,data_cov=data_cov
)

fit <- stan(file = "s_pollutionmodel.stan", data = mod.data,seed = 128, iter = 10000, chains = 2,thin = 5, verbose=TRUE)
result <- rstan::extract(fit)



