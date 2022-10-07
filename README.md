# UrbanRuralAnalysis

All the analysis are based on Windows 10 platform (R version 4.1.2 (2021-11-01)), and the version of package rstan is 2.21.3 

The main Rscript documents include: \\
(1) "Rcode_pollutionModel.R", used to fit the proposed pollution model;\\
(2) "Rcode_healthModel.R", used to fit the health model;\\
(3) "Rcode_healthModel_biPollutant.R", used to fit the bi-pollutant health model;\\
(4) "Rcode_maps.R", used to produced maps in the paper.\\

The other supporting files:\\
a) "data.combined_withPredPollution.rar", this requires unzip to get "data.combined_withPredPollution.RData". This is the main clean data using for health models;\\
b) "raw_data_annual.RData", the raw pollution data;\\
c) "pop_grid.RData", the population density data;\\
d) "W.nb.RData",the neighbor matrix for health model;\\
e) "s_pollutionmodel.stan", the stan code used for fitting the proposed pollution model;\\
f) "BYM_caseStudy_UrbanRural_multiTimes.stan", the stan code used for fitting health model;\\
g) "BYM_caseStudy_UrbanRural_multiTimes_MultP.stan", the stan code used for fitting bi-pollutant health model;\\
h) "BYM_caseStudy_UrbanRural_multiTimes.RData", the C++ compiled file can actually generated by the stan file "BYM_caseStudy_UrbanRural_multiTimes.stan"; \\
i) "BYM_caseStudy_UrbanRural_multiTimes_MultP.RData", the C++ compiled file can actually generated by the stan file "BYM_caseStudy_UrbanRural_multiTimes_MultP.stan". 

