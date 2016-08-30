testing_for_normality=function(number_samples, current_sample_size, population, normality_test){
   
   
   ### initialise outputs (p_values from each test)
   output_othertest=matrix(data=NA, 1, number_samples)
   output_SW=matrix(data=NA, 1, number_samples)
   
   ### create sample matrix (for current_sample_size)
   for (current_number_samples_index in 1:number_samples){
      random_sample_vector=sample(population, current_sample_size, replace = FALSE)
      
      if (normality_test=='AD') {
         output_othertest[current_number_samples_index]=ad.test(random_sample_vector)$p.value
      } else if (normality_test=='CVM') {
         output_othertest[current_number_samples_index]=cvm.test(random_sample_vector)$p.value
      } else if (normality_test=='PEARSON') {
         output_othertest[current_number_samples_index]=pearson.test(random_sample_vector)$p.value
      } else if (normality_test=='SF') {
         output_othertest[current_number_samples_index]=sf.test(random_sample_vector)$p.value
      } else if (normality_test=='JB') {
         output_othertest[current_number_samples_index]=attr(jarqueberaTest(random_sample_vector), 'test')$p.value
      } else if (normality_test=='DA') {
         output_othertest[current_number_samples_index]=agostino.test(random_sample_vector)$p.value
      } else if (normality_test=='UKS') {
         output_othertest[current_number_samples_index]=attr(ksnormTest((random_sample_vector-mean(random_sample_vector))/sd(random_sample_vector)), 'test')$p.value[1]
      } else {
         output_othertest[current_number_samples_index]=attr(lillieTest(random_sample_vector), 'test')$p.value
      }
            
      
      output_SW[current_number_samples_index]=shapiro.test(random_sample_vector)$p.value
      
   }
   
return(list(output_othertest, output_SW))
}




