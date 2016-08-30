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
number_populations=10
population_size=100000
number_samples=500
sample_sizes=seq(from = 10, to = 500, by=10)

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

plot_specificities(specificities, pdirectory, colour='bw')
# Last input: if 'bw', black and white pictures are generated
#             if 'colours', the pictures are generated in colours

# Confidence interval
p=sum(specificities[, 3]>specificities[, 2])/nrow(specificities)
q=1-p
z=qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
lowest_bound=p-z*sqrt(p*q/nrow(specificities))
highest_bound=min(c(1, p+z*sqrt(p*q/nrow(specificities))))
CI_message=sprintf("The 95%% confidence interval for the proportion of times SW outperforms KS is [%.2f, %.2f]", lowest_bound, highest_bound)
print(CI_message)
   
