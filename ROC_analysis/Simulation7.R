#######################################################################################
#                                  Roc analysis                                       #
#######################################################################################
# 
# a) Generates samples of different sizes from a normally distributed population  and
#    from a non-normally distributed population (its distribution is user-defined)
# 
# b) for each sample, applies two normality tests (KS and Shapiro Wilk) and computes
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
#    for the proportion of times SW outperforms KS are printed
#
# 2015: Miguel Patricio (mjpd@uc.pt) and Fábio Ferreira (ferreira.fabio80@gmail.com)
#######################################################################################

### set working directory
setwd('C:/Users/Miguel/Desktop/A_Artigos_meus/em progresso/Testes à normalidade/scripts/ROC analysis')
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
number_populations=100
population_size=100000
number_samples=500
sample_sizes=c(100, 150, 200, 250, 300)
population_distribution='t30'
# the random distribution of the population is implemented within compute_AUCs
# (it's also easy to customise further options)
# Possibilites: 't5'=t(5), 't10'=t(10),  't15'=t(15),  't20'=t(20),  't25'=t(25),  
#               't30'=t(30)


AUCs=matrix(data=NA, length(sample_sizes)*number_populations, 3) 
# matrix with 3 columns. This is to be filled with the #number_population computed AUCs
# for KS and SW
AUCs=data.matrix(AUCs)
colnames(AUCs)=c("sample.size", "AUCs.KS", "AUCs.SW")

linecounter=1 # auxiliary counter for filling out the 'AUCs' matrix

for (seednumber in 1:number_populations){
 
   progress_message=sprintf("Computing AUCs: simulation for population number #%d out of %d", seednumber, number_populations)
   print(progress_message)
   
   iteration_output=compute_AUCs(seednumber, population_size, sample_sizes, number_samples, population_distribution)
      
   AUCs[linecounter:(linecounter+length(sample_sizes)-1), 2]=iteration_output[[1]]
   AUCs[linecounter:(linecounter+length(sample_sizes)-1), 3]=iteration_output[[2]]
   AUCs[linecounter:(linecounter+length(sample_sizes)-1), 1]=sample_sizes
   linecounter=linecounter+length(sample_sizes)   
}

plot_AUCs(AUCs, population_distribution, pdirectory, 'bw')
# Last input: if 'bw', black and white pictures are generated
#             if 'colours', the pictures are generated in colours


# Confidence interval
AUCs=as.data.frame(AUCs)

# n=100
size100=AUCs[AUCs$sample.size==100, ]
lowest_bound=quantile(size100$AUCs.KS, probs = c(0.025))
highest_bound=quantile(size100$AUCs.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 100) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size100$AUCs.SW, probs = c(0.025))
highest_bound=quantile(size100$AUCs.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 100) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

# n=150
size150=AUCs[AUCs$sample.size==150, ]
lowest_bound=quantile(size150$AUCs.KS, probs = c(0.025))
highest_bound=quantile(size150$AUCs.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 150) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size150$AUCs.SW, probs = c(0.025))
highest_bound=quantile(size150$AUCs.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 150) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

# n=200
size200=AUCs[AUCs$sample.size==200, ]
lowest_bound=quantile(size200$AUCs.KS, probs = c(0.025))
highest_bound=quantile(size200$AUCs.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 200) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size200$AUCs.SW, probs = c(0.025))
highest_bound=quantile(size200$AUCs.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 200) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

# n=250
size250=AUCs[AUCs$sample.size==250, ]
lowest_bound=quantile(size250$AUCs.KS, probs = c(0.025))
highest_bound=quantile(size250$AUCs.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 250) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size250$AUCs.SW, probs = c(0.025))
highest_bound=quantile(size250$AUCs.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 250) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

# n=300
size300=AUCs[AUCs$sample.size==300, ]
lowest_bound=quantile(size300$AUCs.KS, probs = c(0.025))
highest_bound=quantile(size300$AUCs.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 300) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size300$AUCs.SW, probs = c(0.025))
highest_bound=quantile(size300$AUCs.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the AUC (for samples of sample size 300) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)
