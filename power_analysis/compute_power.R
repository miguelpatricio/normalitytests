compute_power=function(seednumber, population_size, sample_sizes, number_samples, population_distribution){


### step 1: obtain normally distributed population
set.seed(seednumber)


if (population_distribution=='rbeta') {
   population=rbeta(population_size, 2, 2)
} else if (population_distribution=='laplace') {
   population=rlaplace(population_size, location = 0, scale = 1)
} else if ( population_distribution=='uniform') {
   population=runif(population_size, min = 0, max = 1)
} else if ( population_distribution=='t10') {
   population=rt(population_size, 10)
} else if ( population_distribution=='weibull') {
   population=rweibull(population_size, shape=1, scale = 1)
} else if ( population_distribution=='chisquare') {
   population=rchisq(population_size, 4, ncp=0)
} else if ( population_distribution=='gamma') {
   population=rgamma(population_size, shape=1, scale = 2)
} else {
   population=rbeta(population_size, 6, 2)
}


### step 2: initialise outputs
p_values_KS=matrix(data=NA, length(sample_sizes), number_samples)
p_values_SW=matrix(data=NA, length(sample_sizes), number_samples)

### step 3: for cycle, for each component of the sample_sizes vector, it generates the
#           corrresponding lines in the outputs that were already initialised in step 2
for (current_sample_size_index in 1:length(sample_sizes)){
   outputs=testing_for_normality(number_samples, sample_sizes[current_sample_size_index], population)
   
   p_values_KS[current_sample_size_index, ]=outputs[[1]]
   p_values_SW[current_sample_size_index, ]=outputs[[2]]
   
  # print(current_sample_size_index)   
}

### step 4: compute final outputs (mean_p_values and power, for each test)

# a) initialise outputs
mean_p_values_KS=matrix(data=NA, length(sample_sizes), 1)
mean_p_values_SW=matrix(data=NA, length(sample_sizes), 1)
powers_KS=matrix(data=NA, length(sample_sizes), 1)
powers_SW=matrix(data=NA, length(sample_sizes), 1)

# b) compute outputs
mean_p_values_KS=rowSums(p_values_KS)/number_samples
mean_p_values_SW=rowSums(p_values_SW)/number_samples

powers_KS=rowSums(p_values_KS<0.05)/number_samples
powers_SW=rowSums(p_values_SW<0.05)/number_samples


return(list(powers_KS, powers_SW))
}