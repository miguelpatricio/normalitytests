#######################################################################################
#                                Specificity analysis                                 #
#######################################################################################
# 
# a) Generates normally distributed samples of different sizes from a population 
#    (its distribution is user-defined)
# 
# b) for each sample, applies two normality tests (KS and Shapiro Wilk) and computes
#    resulting p-values
#
# c) repeats a) and b) several times (i.e., generates several populations and many samples
#    out of each population)
#
# d) for each population and each sample size the specificity of each normality test is 
#    computed
#
# Output: 
# 1) a data frame 'specificity' which displays the specificity achieved by each normality 
#    test for each population and each sample size
#
# 2) the information contained in the data frame is plotted and 95% confidence intervals
#    for the proportion of times SW outperforms KS are printed
#
# 2015: Miguel Patricio (mjpd@uc.pt) and Fábio Ferreira (ferreira.fabio80@gmail.com)
#######################################################################################

### set working directory
setwd('C:/Users/Miguel/Desktop/A_Artigos_meus/em progresso/Testes à normalidade/scripts/specificity analysis')
pdirectory=getwd()
source('testing_for_normality.r')
source('compute_specificity.r')
source('plot_specificities.R')
library(fBasics)
library(ggplot2)
library(VGAM)

### Input
number_populations=100
population_size=100000
number_samples=500
sample_sizes=c(10, 30, 50, 100, 200, 300)

specificities=matrix(data=NA, length(sample_sizes)*number_populations, 3) 
# matrix with 3 columns. This is to be filled with the #number_population computed specificities
# for KS and SW
specificities=data.matrix(specificities)
colnames(specificities)=c("sample.size", "specificity.KS", "specificity.SW")

linecounter=1 # auxiliary counter for filling out the 'specificities' matrix

for (seednumber in 1:number_populations){
   
   progress_message=sprintf("Computing specificities: simulation for population number #%d out of %d", seednumber, number_populations)
   print(progress_message)
   
   iteration_output=compute_specificity(seednumber, population_size, sample_sizes, number_samples)
   
   specificities[linecounter:(linecounter+length(sample_sizes)-1), 2]=iteration_output[[1]]
   specificities[linecounter:(linecounter+length(sample_sizes)-1), 3]=iteration_output[[2]]
   specificities[linecounter:(linecounter+length(sample_sizes)-1), 1]=sample_sizes
   linecounter=linecounter+length(sample_sizes)   
}

plot_specificities(specificities, pdirectory, 'bw')
# Last input: if 'bw', black and white pictures are generated
#             if 'colours', the pictures are generated in colours

# Confidence interval
specificities=as.data.frame(specificities)

# n=10
size10=specificities[specificities$sample.size==10, ]
lowest_bound=quantile(size10$specificity.KS, probs = c(0.025))
highest_bound=quantile(size10$specificity.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 10) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size10$specificity.SW, probs = c(0.025))
highest_bound=quantile(size10$specificity.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 10) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

# n=30
size30=specificities[specificities$sample.size==30, ]
lowest_bound=quantile(size30$specificity.KS, probs = c(0.025))
highest_bound=quantile(size30$specificity.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 30) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size30$specificity.SW, probs = c(0.025))
highest_bound=quantile(size30$specificity.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 30) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

# n=50
size50=specificities[specificities$sample.size==50, ]
lowest_bound=quantile(size50$specificity.KS, probs = c(0.025))
highest_bound=quantile(size50$specificity.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 50) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size50$specificity.SW, probs = c(0.025))
highest_bound=quantile(size50$specificity.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 50) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

# n=100
size100=specificities[specificities$sample.size==100, ]
lowest_bound=quantile(size100$specificity.KS, probs = c(0.025))
highest_bound=quantile(size100$specificity.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 100) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size100$specificity.SW, probs = c(0.025))
highest_bound=quantile(size100$specificity.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 100) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)


# n=200
size200=specificities[specificities$sample.size==200, ]
lowest_bound=quantile(size200$specificity.KS, probs = c(0.025))
highest_bound=quantile(size200$specificity.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 200) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size200$specificity.SW, probs = c(0.025))
highest_bound=quantile(size200$specificity.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 200) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)


# n=300
size300=specificities[specificities$sample.size==300, ]
lowest_bound=quantile(size300$specificity.KS, probs = c(0.025))
highest_bound=quantile(size300$specificity.KS, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 300) attained by the KS test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)

lowest_bound=quantile(size300$specificity.SW, probs = c(0.025))
highest_bound=quantile(size300$specificity.SW, probs = c(0.975))
CI_message=sprintf("The 95%% confidence interval for the specificity (for samples of sample size 300) attained by the SW test is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)
