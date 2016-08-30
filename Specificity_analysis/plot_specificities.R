plot_specificities=function(specificities, pdirectory, colour){

   
   if (colour=='colours'){
   
   
   specificities=data.frame(specificities)

   

   specificities2plot=data.frame(sample_sizes=rep(rep(sample_sizes, each = 2), nrow(specificities)/length(sample_sizes)), 
                       computed_specificities=as.numeric(rbind(specificities$specificity.KS, specificities$specificity.SW)), test=as.factor(rep(c(1, 2), length(specificities$sample.size))))

   specificities2plot$test=factor(specificities2plot$test, levels=c(1, 2), labels=c("KS","SW")) 
   colnames(specificities2plot)[3]="Test"
   
   
   p <- ggplot(specificities2plot, aes(x = sample_sizes, y = computed_specificities, color = Test))
   p=p + geom_point(alpha = 4/10, size=4)+theme_bw()+xlab("Sample sizes")+ylab("Specificity")
   p=p+theme(axis.title.x = element_text( size=20), axis.title.y = element_text( size=20))+ theme(legend.text=element_text(size=20))
   p=p+theme(legend.title = element_text(size=20, face="bold"))
   p=p+ theme(axis.text.x = element_text(size=18))+theme(axis.text.y = element_text(size=18))
   print(p)

   
   setwd(paste(pdirectory, "/images", sep=""))
   
   
   tiff("specificity.tiff", res = 200, width = 1920, height = 1080)
   print(p)
   dev.off()
   
   setwd(pdirectory)}
   
   
   else {
      
      
      specificities=data.frame(specificities)
      
      
      
      specificities2plot=data.frame(sample_sizes=rep(rep(sample_sizes, each = 2), nrow(specificities)/length(sample_sizes)), 
                             computed_specificities=as.numeric(rbind(specificities$specificity.KS, specificities$specificity.SW)), test=as.factor(rep(c(1, 2), length(specificities$sample.size))))
      
      specificities2plot$test=factor(specificities2plot$test, levels=c(1, 2), labels=c("KS","SW")) 
      colnames(specificities2plot)[3]="Test"
      
      
      p <- ggplot(specificities2plot, aes(x = sample_sizes, y = computed_specificities, color = Test))
      p=p + geom_point(size=4)+theme_bw()+xlab("Sample sizes")+ylab("Specificity")+scale_color_manual(values = c("black", "gray"))
      p=p+theme(axis.title.x = element_text( size=20), axis.title.y = element_text( size=20))+ theme(legend.text=element_text(size=20))
      p=p+theme(legend.title = element_text(size=20, face="bold"))
      p=p+ theme(axis.text.x = element_text(size=18))+theme(axis.text.y = element_text(size=18))
      print(p)
      
      
      
      
      setwd(paste(pdirectory, "/images", sep=""))
      
      
      tiff("specificity_bw.tiff", res = 200, width = 1920, height = 1080)
      print(p)
      dev.off()
      
      setwd(pdirectory)
   
   }
   
   
      
}