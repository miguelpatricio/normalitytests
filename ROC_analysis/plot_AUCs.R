plot_AUCs=function(AUCs, population_distribution, pdirectory, colour){
   
   
   if (colour=='colours'){

   AUCs=data.frame(AUCs)

   

   AUCs2plot=data.frame(sample_sizes=rep(rep(sample_sizes, each = 2), nrow(AUCs)/length(sample_sizes)), 
                       computed_AUCs=as.numeric(rbind(AUCs$AUCs.othertest, AUCs$AUCs.SW)), test=as.factor(rep(c(1, 2), length(AUCs$sample.size))))

   AUCs2plot$test=factor(AUCs2plot$test, levels=c(1, 2), labels=c("Other test","SW")) 
   colnames(AUCs2plot)[3]="Test"

   p <- ggplot(AUCs2plot, aes(x = sample_sizes, y = computed_AUCs, color = Test))
   p=p + geom_point(alpha = 4/10, size=4)+theme_bw()+xlab("Sample sizes")+ylab("AUC")
   p=p+theme(axis.title.x = element_text( size=20), axis.title.y = element_text( size=20))+ theme(legend.text=element_text(size=20))
   p=p+theme(legend.title = element_text(size=20, face="bold"))
   p=p+ theme(axis.text.x = element_text(size=18))+theme(axis.text.y = element_text(size=18))
   print(p)
   
   
   setwd(paste(pdirectory, "/images", sep=""))
   
   
   tiff(paste(population_distribution, "_AUCs.tiff", sep=""), res = 200, width = 1920, height = 1080)
   print(p)
   dev.off()
   
   setwd(pdirectory)}
   
   
   else {
      
      AUCs=data.frame(AUCs)
      
      
      
      AUCs2plot=data.frame(sample_sizes=rep(rep(sample_sizes, each = 2), nrow(AUCs)/length(sample_sizes)), 
                           computed_AUCs=as.numeric(rbind(AUCs$AUCs.othertest, AUCs$AUCs.SW)), test=as.factor(rep(c(1, 2), length(AUCs$sample.size))))
      
      AUCs2plot$test=factor(AUCs2plot$test, levels=c(1, 2), labels=c("Other test","SW")) 
      colnames(AUCs2plot)[3]="Test"
      
      p <- ggplot(AUCs2plot, aes(x = sample_sizes, y = computed_AUCs, color = Test))
      p=p + geom_point(size=4)+theme_bw()+xlab("Sample sizes")+ylab("AUC")+scale_color_manual(values = c("black", "gray"))
      p=p+theme(axis.title.x = element_text( size=20), axis.title.y = element_text( size=20))+ theme(legend.text=element_text(size=20))
      p=p+theme(legend.title = element_text(size=20, face="bold"))
      p=p+ theme(axis.text.x = element_text(size=18))+theme(axis.text.y = element_text(size=18))
      print(p)
      
      
      setwd(paste(pdirectory, "/images", sep=""))
      
      
      tiff(paste(population_distribution, "_AUCs_bw.tiff", sep=""), res = 200, width = 1920, height = 1080)
      print(p)
      dev.off()
      
      setwd(pdirectory)    
      
      
   }
   
   
}