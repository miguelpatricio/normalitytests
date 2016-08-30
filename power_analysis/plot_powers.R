plot_powers=function(powers, population_distribution, pdirectory, colour){

   
   if (colour=='colours'){
   
   
   powers=data.frame(powers)

   

   powers2plot=data.frame(sample_sizes=rep(rep(sample_sizes, each = 2), nrow(powers)/length(sample_sizes)), 
                       computed_powers=as.numeric(rbind(powers$power.KS, powers$power.SW)), test=as.factor(rep(c(1, 2), length(powers$sample.size))))

   powers2plot$test=factor(powers2plot$test, levels=c(1, 2), labels=c("KS","SW")) 
   colnames(powers2plot)[3]="Test"
   
   
   p <- ggplot(powers2plot, aes(x = sample_sizes, y = computed_powers, color = Test))
   p=p + geom_point(alpha = 4/10, size=4)+theme_bw()+xlab("Sample sizes")+ylab("Power")
   p=p+theme(axis.title.x = element_text( size=20), axis.title.y = element_text( size=20))+ theme(legend.text=element_text(size=20))
   p=p+theme(legend.title = element_text(size=20, face="bold"))
   p=p+ theme(axis.text.x = element_text(size=18))+theme(axis.text.y = element_text(size=18))
   print(p)

   
   setwd(paste(pdirectory, "/images", sep=""))
   
   
   tiff(paste(population_distribution, "_power.tiff", sep=""), res = 200, width = 1920, height = 1080)
   print(p)
   dev.off()
   
   setwd(pdirectory)}
   
   
   else {
      
      
      powers=data.frame(powers)
      
      
      
      powers2plot=data.frame(sample_sizes=rep(rep(sample_sizes, each = 2), nrow(powers)/length(sample_sizes)), 
                             computed_powers=as.numeric(rbind(powers$power.KS, powers$power.SW)), test=as.factor(rep(c(1, 2), length(powers$sample.size))))
      
      powers2plot$test=factor(powers2plot$test, levels=c(1, 2), labels=c("KS","SW")) 
      colnames(powers2plot)[3]="Test"
      
      
      p <- ggplot(powers2plot, aes(x = sample_sizes, y = computed_powers, color = Test))
      p=p + geom_point(size=4)+theme_bw()+xlab("Sample sizes")+ylab("Power")+scale_color_manual(values = c("black", "gray"))
      p=p+theme(axis.title.x = element_text( size=20), axis.title.y = element_text( size=20))+ theme(legend.text=element_text(size=20))
      p=p+theme(legend.title = element_text(size=20, face="bold"))
      p=p+ theme(axis.text.x = element_text(size=18))+theme(axis.text.y = element_text(size=18))
      print(p)
      
      
      
      
      setwd(paste(pdirectory, "/images", sep=""))
      
      
      tiff(paste(population_distribution, "_power_bw.tiff", sep=""), res = 200, width = 1920, height = 1080)
      print(p)
      dev.off()
      
      setwd(pdirectory)
   
   }
   
   
      
}