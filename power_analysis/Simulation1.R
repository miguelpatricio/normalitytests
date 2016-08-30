#######################################################################################
#                                    Power analysis                                   #
#######################################################################################
# 
# a) Generates samples of different sizes from a population 
#    (its distribution is user-defined)
# 
# b) for each sample, applies two normality tests (KS and Shapiro Wilk) and computes
#    resulting p-values
#
# c) repeats a) and b) several times (i.e., generates several populations and many samples
#    out of each population)
#
# d) for each population and each sample size the power of each normality test is computed
#
# Output: 
# 1) a data frame 'powers' which displays the power achieved by each normality test for
#    each population and each sample size
#
# 2) the information contained in the data frame is plotted and 95% confidence intervals
#    for the proportion of times SW outperforms KS are printed
#
# 2015: Miguel Patricio (mjpd@uc.pt) and Fábio Ferreira (ferreira.fabio80@gmail.com)
#######################################################################################

### set working directory
setwd('C:/Users/Miguel/Desktop/A_Artigos_meus/em progresso/Testes à normalidade/scripts/power analysis')
pdirectory=getwd()
source('testing_for_normality.r')
source('compute_power.r')
source('plot_powers.R')
library(fBasics)
library(ggplot2)
library(VGAM)

### Input
number_populations=10
population_size=100000
number_samples=500
sample_sizes=seq(from = 10, to = 100, by=2)
population_distribution='beta'
# the random distribution of the population is implemented within compute_AUCs
# (it's also easy to customise further options)
# Possibilites: 'rbeta'=Beta(2, 2), 'laplace'=Laplace(0, 1), 'uniform'=U(0, 1), 't10'=t(10);
# Other possibilities: 'weibull'=Weibull(1, 1), 'chisquare'=chisquare(4), 'gamma'=Gamma(1, 2),
#                      'beta'=Beta(6, 2) 


powers=matrix(data=NA, length(sample_sizes)*number_populations, 3) 
# matrix with 3 columns. This is to be filled with the #number_population computed powers
# for KS and SW
powers=data.matrix(powers)
colnames(powers)=c("sample.size", "power.KS", "power.SW")

linecounter=1 # auxiliary counter for filling out the 'powers' matrix

for (seednumber in 1:number_populations){
   
   progress_message=sprintf("Computing powers: simulation for population number #%d out of %d", seednumber, number_populations)
   print(progress_message)
   
   iteration_output=compute_power(seednumber, population_size, sample_sizes, number_samples, population_distribution)
      
   powers[linecounter:(linecounter+length(sample_sizes)-1), 2]=iteration_output[[1]]
   powers[linecounter:(linecounter+length(sample_sizes)-1), 3]=iteration_output[[2]]
   powers[linecounter:(linecounter+length(sample_sizes)-1), 1]=sample_sizes
   linecounter=linecounter+length(sample_sizes)   
}

plot_powers(powers, population_distribution, pdirectory, 'bw')
# Last input: if 'bw', black and white pictures are generated
#             if 'colours', the pictures are generated in colours

# Confidence interval
p=sum(powers[, 3]>powers[, 2])/nrow(powers)
q=1-p
z=qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
lowest_bound=p-z*sqrt(p*q/nrow(powers))
highest_bound=min(c(1, p+z*sqrt(p*q/nrow(powers))))
CI_message=sprintf("The 95%% confidence interval for the proportion of times SW outperforms KS is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)
   
