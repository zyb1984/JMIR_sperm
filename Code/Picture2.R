library("ggplot2")
library("tidyr")
library("mgcv")
data <- read.csv("./Data/Scopus_Year.csv",header=T)
data_long_raw <- data[,c(1:3)]
data_long <- gather(data_long_raw, Group, Number, -Year)
p1<- ggplot(data=data_long,aes(Year, Number,fill=Group))+ 
     geom_bar(stat="identity",position="stack", color="black", width=0.7,size=0.25)+
	 ## scale_fill_manual(values=c("red", "blue", "green","darkgreen","black"))
     scale_fill_manual(values=c("red", "blue"))+
	 stat_smooth(method = gam, formula = y ~ s(x))+
	 theme(legend.position = "none")+
     theme(axis.text.x=element_text(angle=90, hjust=1))+
     theme_classic() 
	  
p2<- ggplot(data=data,aes(Year, Ratio))+ 
     geom_bar(stat="identity",position="stack", color="black", width=0.7,size=0.25)+
	 ## scale_fill_manual(values=c("red", "blue", "green","darkgreen","black"))
     scale_fill_manual(values=c("black"))+
	 geom_hline(aes(yintercept=mean(data$Ratio)), colour="red", linetype="dashed")+
	 stat_smooth(method = gam, formula = y ~ s(x))+
	 theme(legend.position = "none")+
     theme(axis.text.x=element_text(angle=90, hjust=1))+
     theme_classic() 
library(magick)
library("rsvg")
p3_image <- image_read_svg("./Data/Scopus_Article_Subject.svg", width =1200)
p3 <- image_ggplot(p3_image, interpolate = FALSE)	 
p4_image <- image_read_svg("./Data/Scopus_Review_Subject.svg", width =1200)
p4 <- image_ggplot(p4_image, interpolate = FALSE)	 
library("cowplot")
library(showtext)
picture02 <- ggdraw() +     
      draw_plot(p1, 0, 0.5, 1, 0.5) + 
      draw_plot(p2, 0, 0, 0.5, 0.5) + 
      draw_plot(p3, 0.5, 0.25, 0.5, 0.25) + 
      draw_plot(p4, 0.5, 0, 0.5, 0.25) +
       draw_plot_label(c("A", "B", "C","D"),c(0, 0, 0.5,0.5), c(1, 0.5, 0.5,0.25)) 
pdf('Picture2.pdf',width = 6,height = 6)
picture02
dev.off()
