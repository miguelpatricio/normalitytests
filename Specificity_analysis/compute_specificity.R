compute_specificity=function(seednumber, population_size, sample_sizes, number_samples){


### step 1: obtain normally distributed population
set.seed(seednumber)
population=rnorm(population_size, 0, 1)


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

### step 4: compute final outputs (mean_p_values and specificities, for each test)

# a) initialise outputs
mean_p_values_KS=matrix(data=NA, length(sample_sizes), 1)
mean_p_values_SW=matrix(data=NA, length(sample_sizes), 1)
specificities_KS=matrix(data=NA, length(sample_sizes), 1)
specificities_SW=matrix(data=NA, length(sample_sizes), 1)

# b) compute outputs
mean_p_values_KS=rowSums(p_values_KS)/number_samples
mean_p_values_SW=rowSums(p_values_SW)/number_samples

specificities_KS=rowSums(p_values_KS>=0.05)/number_samples
specificities_SW=rowSums(p_values_SW>=0.05)/number_samples


return(list(specificities_KS, specificities_SW))
}