compute_AUCs=function(seednumber, population_size, sample_sizes, number_samples, population_distribution, normality_test){


### step 1: obtain normally distributed population
set.seed(seednumber)


if (population_distribution=='rbeta') {
   nnormalpopulation=rbeta(population_size, 2, 2)
} else if (population_distribution=='laplace') {
   nnormalpopulation=rlaplace(population_size, location = 0, scale = 1)
} else if ( population_distribution=='uniform') {
   nnormalpopulation=runif(population_size, min = 0, max = 1)
} else if ( population_distribution=='t5') {
   nnormalpopulation=rt(population_size, 5)
} else if ( population_distribution=='t10') {
   nnormalpopulation=rt(population_size, 10)
} else if ( population_distribution=='t15') {
   nnormalpopulation=rt(population_size, 15)
} else if ( population_distribution=='t20') {
   nnormalpopulation=rt(population_size, 20)
} else if ( population_distribution=='t25') {
   nnormalpopulation=rt(population_size, 25)
} else if ( population_distribution=='t30') {
   nnormalpopulation=rt(population_size, 30)
} else if ( population_distribution=='weibull') {
   nnormalpopulation=rweibull(population_size, shape=1, scale = 1)
} else if ( population_distribution=='chisquare') {
   nnormalpopulation=rchisq(population_size, 4, ncp=0)
} else if ( population_distribution=='gamma') {
   nnormalpopulation=rgamma(population_size, shape=1, scale = 2)
} else {
   nnormalpopulation=rbeta(population_size, 6, 2)
}
normalpopulation=rnorm(population_size, 0, 1)
# Numenclature: nnormal=non-normal   


### step 2: initialise outputs
normalpopulation_p_values_othertest=matrix(data=NA, length(sample_sizes), number_samples)
normalpopulation_p_values_SW=matrix(data=NA, length(sample_sizes), number_samples)
nnormalpopulation_p_values_othertest=matrix(data=NA, length(sample_sizes), number_samples)
nnormalpopulation_p_values_SW=matrix(data=NA, length(sample_sizes), number_samples)

### step 3: for cycle, for each component of the sample_sizes vector, it generates the
#           corrresponding lines in the outputs that were already initialised in step 2
for (current_sample_size_index in 1:length(sample_sizes)){
   
   # applying normality tests to samples from a normally distributed population
   outputs=testing_for_normality(number_samples, sample_sizes[current_sample_size_index], normalpopulation, normality_test)
   normalpopulation_p_values_othertest[current_sample_size_index, ]=outputs[[1]]
   normalpopulation_p_values_SW[current_sample_size_index, ]=outputs[[2]]
   
   # applying normality tests to samples from a non-normally distributed population
   outputs=testing_for_normality(number_samples, sample_sizes[current_sample_size_index], nnormalpopulation, normality_test)
   nnormalpopulation_p_values_othertest[current_sample_size_index, ]=outputs[[1]]
   nnormalpopulation_p_values_SW[current_sample_size_index, ]=outputs[[2]]   
    
}

### step 4: compute AUC for each test and for each sample size

AUCs_othertest=matrix(data=NA, length(sample_sizes), 1)
AUCs_SW=matrix(data=NA, length(sample_sizes), 1)
for (linecounter in 1:length(sample_sizes)){
   non_normal_p_values_othertest=nnormalpopulation_p_values_othertest[linecounter, ]
   normal_p_values_othertest=normalpopulation_p_values_othertest[linecounter, ]
   p_values_othertest=c(non_normal_p_values_othertest, normal_p_values_othertest)
   true_classification_othertest=c(rep(0, number_samples), rep(1, number_samples))
   # true==1 means it is normal; true==0 means it is not normal
   AUCs_othertest[linecounter]=auc(true_classification_othertest, p_values_othertest)[[1]]
     
   non_normal_p_values_SW=nnormalpopulation_p_values_SW[linecounter, ]
   normal_p_values_SW=normalpopulation_p_values_SW[linecounter, ]
   p_values_SW=c(non_normal_p_values_SW, normal_p_values_SW)
   true_classification_SW=c(rep(0, number_samples), rep(1, number_samples))
   # true==1 means it is normal; true==0 means it is not normal
   AUCs_SW[linecounter]=auc(true_classification_SW, p_values_SW)[[1]]
   
}


return(list(AUCs_othertest, AUCs_SW))
}