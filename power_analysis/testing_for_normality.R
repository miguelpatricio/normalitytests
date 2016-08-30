testing_for_normality=function(number_samples, current_sample_size, population){
   
   
   ### initialise outputs (p_values from each test)
   output_KS=matrix(data=NA, 1, number_samples)
   output_SW=matrix(data=NA, 1, number_samples)
   
   ### create sample matrix (for current_sample_size)
   for (current_number_samples_index in 1:number_samples){
      random_sample_vector=sample(population, current_sample_size, replace = FALSE)
      output_KS[current_number_samples_index]=attr(lillieTest(random_sample_vector), 'test')$p.value
      output_SW[current_number_samples_index]=shapiro.test(random_sample_vector)$p.value
      
   }
   
return(list(output_KS, output_SW))
}




