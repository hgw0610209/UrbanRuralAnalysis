
library(sp)
library(spdep)
library(rstan)
# load the ready data 
load(file="data.combined_withPredPollution.RData")

##build neighbor matrix, using the following few lines to obtain "W.nb.RData"
# data.combined<-na.omit(data.combined)
# W.nb<-poly2nb(data.combined,row.names=data.combined$FeatureCode)
# save(W.nb, file = "W.nb.RData")

load(file = "W.nb.RData")
##change into matrix form
W.mat<-nb2mat(W.nb,style="B", zero.policy=TRUE)

# for stan car model
W_rowsum <- apply(W.mat,1,sum)

N_edges = sum(W.mat)/ 2
node1 =NULL
node2 = NULL
for(i in 1:1279)
{
  stuff <- which(W.mat[i,i:ncol(W.mat)]==1)+i-1
  node1 <- c(node1, rep(i, length(stuff)))
  node2 <- c(node2, stuff)
}


## using the following few lines to obtain the health model stan code
# fit.code <- stanc(file = "BYM_caseStudy_UrbanRural_multiTimes.stan") # convert to C++ code
# fit.model <- stan_model(stanc_ret = fit.code) # compile C++ code
# save(fit.model, file="BYM_caseStudy_UrbanRural_multiTimes.RData")

load(file="BYM_caseStudy_UrbanRural_multiTimes.RData")


# run the stan model 

# the Urban_rural column, urban is 1 and rural is 0
mod.data <- list(k_area=1279,
                 t_year=6,
                 n_covariate=3,
                 Y_kt=as.matrix(data.combined@data[,c("Deaths2014","Deaths2015","Deaths2016","Deaths2017","Deaths2018","Deaths2019")]),
                 E_kt=as.matrix(data.combined@data[,c("E_Deaths2014","E_Deaths2015","E_Deaths2016","E_Deaths2017","E_Deaths2018","E_Deaths2019")]),
                 area_AP_one=as.matrix(data.combined@data[,c("pm102013mean","pm102014mean","pm102015mean","pm102016mean","pm102017mean","pm102018mean")])*data.combined$Urban_rural,
                 area_AP_two=as.matrix(data.combined@data[,c("pm102013mean","pm102014mean","pm102015mean","pm102016mean","pm102017mean","pm102018mean")])*(1-data.combined$Urban_rural),
                 covariate_one=as.matrix(data.combined@data[,c("Crime","Crime","Crime","Crime","Crime","Crime")]),
                 covariate_two=as.matrix(data.combined@data[,c("EST","EST","EST","EST","EST","EST")]),
                 covariate_three=as.matrix(data.combined@data[,c("AtS","AtS","AtS","AtS","AtS","AtS")]),
                 N_edges=N_edges,
                 node1=node1,
                 node2=node2
)

# set the MCMC iteration and thinning method.
Nsample=5000
thin=5

# run the stan model
for(AP in c("pm10","no2"))
{
  for(metric in c("mean","max")) # max has no weighted.
  {
        #grid
        mod.data$area_AP_one <- as.matrix(data.combined@data[,paste0(AP,seq(2013,2018),metric)])*data.combined$Urban_rural
        mod.data$area_AP_two <- as.matrix(data.combined@data[,paste0(AP,seq(2013,2018),metric)])*(1-data.combined$Urban_rural)

        stan_model <-  rstan::sampling(fit.model,  data = mod.data,seed=158, control = list(max_treedepth=15),
                iter = Nsample, chains = 2, thin = thin,verbose=TRUE, par=c("alpha","lambda1","lambda2","nu2","phi_kt","theta","tau_theta"))
    stan_result <-rstan::extract(stan_model)
    save(stan_result, file=paste0(AP, metric,"grid_","stan_result.RData"))
    
    if(metric != "max")
    {
      
      #grid weighted
      mod.data$area_AP_one <- as.matrix(data.combined@data[,paste0(AP,seq(2013,2018),metric,"_weighted")])*data.combined$Urban_rural
      mod.data$area_AP_two <- as.matrix(data.combined@data[,paste0(AP,seq(2013,2018),metric,"_weighted")])*(1-data.combined$Urban_rural)
      
      stan_model <-  rstan::sampling(fit.model,  data = mod.data,seed=158, control = list(max_treedepth=15),
                                     iter = Nsample, chains = 2, thin = thin,verbose=TRUE, par=c("alpha","lambda1","lambda2","nu2","phi_kt","theta","tau_theta"))
      stan_result <-rstan::extract(stan_model)
      save(stan_result, file=paste0(AP, metric,"grid_weighted_","stan_result.RData"))
    }
    
        # monitoring
        mod.data$area_AP_one <- as.matrix(data.combined@data[,paste0("pred",AP,seq(2013,2018),metric)])*data.combined$Urban_rural
        mod.data$area_AP_two <- as.matrix(data.combined@data[,paste0("pred",AP,seq(2013,2018),metric)])*(1-data.combined$Urban_rural)

        stan_model <-  rstan::sampling(fit.model,  data = mod.data,seed=158, control = list(max_treedepth=15),
                iter = Nsample, chains = 2, thin = thin,verbose=TRUE, par=c("alpha","lambda1","lambda2","nu2","phi_kt","theta","tau_theta"))
    stan_result <-rstan::extract(stan_model)
    save(stan_result, file=paste0(AP, metric,"pred_","stan_result.RData"))
    
    if(metric != "max")
    {
      # monitoring weighted
      mod.data$area_AP_one <- as.matrix(data.combined@data[,paste0("pred",AP,seq(2013,2018),metric,"_weighted")])*data.combined$Urban_rural
      mod.data$area_AP_two <- as.matrix(data.combined@data[,paste0("pred",AP,seq(2013,2018),metric,"_weighted")])*(1-data.combined$Urban_rural)
      stan_model <-  rstan::sampling(fit.model,  data = mod.data,seed=158, control = list(max_treedepth=15),
                                     iter = Nsample, chains = 2, thin = thin,verbose=TRUE, par=c("alpha","lambda1","lambda2","nu2","phi_kt","theta","tau_theta"))
      stan_result <-rstan::extract(stan_model)
      save(stan_result, file=paste0(AP, metric,"pred_weighted_","stan_result.RData"))
    }
  }
}



