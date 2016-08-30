#######################################################################################
#                                  Roc analysis                                       #
#######################################################################################
# 
# a) Generates samples of different sizes from a normally distributed population  and
#    from a non-normally distributed population (its distribution is user-defined)
# 
# b) for each sample, applies two normality tests (Shapiro Wilk and another test) and computes
#    resulting p-values
#
# c) repeats a) and b) several times (i.e., generates several non-normally distributed
#    populations and draws many samples out of the population. It also draws many samples
#    OUT OF of normally distributed populations)
#
# d) for each non-normally distributed population several samples (of varying sample sizes) 
#    were obtained. For each sample size, several samples out of normally distributed data
#    were also obtained. For each non-normally distributed population, both normality test 
#    are applied to the samples (drawn from normally and non-normally distributed populations): 
#    for each sample size and each normality test a ROC analysis is performed. The areas under
#    ROC curves are computed.
#
# Output: 
# 1) a data frame 'AUCs', which displays the areas under the ROC curves achieved by each 
#    normality test for each non-normally distributed population and each sample size
#
# 2) the information contained in the data frame is plotted and and 95% confidence intervals
#    for the proportion of times SW outperforms the other normality test are printed
#
# 2015: Miguel Patricio (mjpd@uc.pt) and Fábio Ferreira (ferreira.fabio80@gmail.com)
#######################################################################################

### set working directory
setwd('C:/Users/Miguel/Desktop/A_Artigos_meus/em progresso/Testes à normalidade/artigo/scripts/ROC analysis')
pdirectory=getwd()
source('testing_for_normality.r')
source('compute_AUCs.r')
source('plot_AUCs.R')
library(fBasics)
library(ggplot2)
library(VGAM)
library(pROC)
library(nortest)
library(moments)

### Input
number_populations=10 
population_size=100000
number_samples=500
sample_sizes=seq(from = 10, to = 100, by=2)
population_distribution='chisquare'
# the random distribution of the population is implemented within compute_AUCs
# (it's also easy to customise further options)
# Possibilites: 'rbeta'=Beta(2, 2), 'laplace'=Laplace(0, 1), 'uniform'=U(0, 1), 't10'=t(10);
# Other possibilities: 'weibull'=Weibull(1, 1), 'chisquare'=chisquare(4), 'gamma'=Gamma(1, 2),
#                      'beta'=Beta(6, 2)          
normality_test='UKS'
# Possibilites: 'KS'=Kolmogorov-Smirnov, 'AD'=Anderson-Darling, 'CVM'=Cramer-Von Mises, 'PEARSON'=Pearson chi-square, 
#                'SF'=Shapiro-Francia, 'JB'=Jarque-Bera, 'DA'=D'Agostino and 'UKS'=uncorrected Kolmogorov-Smirnov



AUCs=matrix(data=NA, length(sample_sizes)*number_populations, 3) 
# matrix with 3 columns. This is to be filled with the #number_population computed AUCs
# for other test and SW
AUCs=data.matrix(AUCs)
colnames(AUCs)=c("sample.size", "AUCs.othertest", "AUCs.SW")

linecounter=1 # auxiliary counter for filling out the 'AUCs' matrix

for (seednumber in 1:number_populations){
 
   progress_message=sprintf("Computing AUCs: simulation for population number #%d out of %d", seednumber, number_populations)
   print(progress_message)
   
   iteration_output=compute_AUCs(seednumber, population_size, sample_sizes, number_samples, population_distribution, normality_test)
      
   AUCs[linecounter:(linecounter+length(sample_sizes)-1), 2]=iteration_output[[1]]
   AUCs[linecounter:(linecounter+length(sample_sizes)-1), 3]=iteration_output[[2]]
   AUCs[linecounter:(linecounter+length(sample_sizes)-1), 1]=sample_sizes
   linecounter=linecounter+length(sample_sizes)   
}

plot_AUCs(AUCs, population_distribution, pdirectory, 'bw')
# Last input: if 'bw', black and white pictures are generated
#             if 'colours', the pictures are generated in colours


# Confidence interval
p=sum(AUCs[, 3]>AUCs[, 2])/nrow(AUCs)
q=1-p
z=qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
lowest_bound=p-z*sqrt(p*q/nrow(AUCs))
highest_bound=min(c(1, p+z*sqrt(p*q/nrow(AUCs))))
CI_message=sprintf("The 95%% confidence interval for the proportion of times SW outperforms the other test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)
