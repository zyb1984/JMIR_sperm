library(tidyverse)
library(sf)
library("tmap")
library("classInt")

data(World)
articles <- read.csv("./Data/Articles_country_map.csv",header=T)
articles <- articles[,-c(1,3:5)]
head(articles)

## 最佳分割点
breaks_Article <- classIntervals(articles$Article, n =5,style = "kmeans")$brks
breaks_Article
##[1] 0.0   57.0  200.0  516.5 1344.0 2196.0
breaks_Review <- classIntervals(articles$Review, n =5,style = "kmeans")$brks
breaks_Review
##[1] 0.0   10.5  47.0  115.0  462.0  732.0
breaks_All <- classIntervals(articles$All, n =5,style = "kmeans")$brks
breaks_All
## [1]    0.0   68.0  252.5  655.0 1505.0 2928.0
World <- merge(World,articles,by="iso_a3")
p_country<- tm_shape(World) +
	    tm_polygons("All",palette = "Blues", alpha = 0.5, n=8) +
	      tm_text("name", size = "AREA") +
		  tm_bubbles(size="Review", col = "Article", style="fixed", breaks=c(0,seq(0, 2500, by=400), 2500),
		 	  palette=  "Reds",contrast= 1) 

library("ggplot2")
articles_country <- read.csv("./Data/Ratio_country.csv", header=T)
Ratio_rank <- ggplot(articles_country,aes(x = reorder(Country, Ratio),  Ratio)) +  
   geom_col() +
   geom_hline(aes(yintercept=mean(Ratio)), colour="red", linetype="dashed")+
   coord_flip() + 
   theme(legend.position = "none")+
     theme(axis.text.x=element_text(angle=90, hjust=1))+
     theme_classic() 	 

pdf('Picture_country01.pdf',width = 6,height = 6)
p_country
Ratio_rank
dev.off()

library(bibliometrix)
file_article <- c("./Data/Scopus_article.bib")
M_article <- convert2df(file_article, dbsource = "scopus", format = "bibtex") 
## 1.3 国家及主要发文机构
M_article <- metaTagExtraction(M_article, Field = "AU_CO", sep = ";")
NetMatrix_article <- biblioNetwork(M_article, analysis = "collaboration", network = "countries", sep = ";")
net_article=networkPlot(NetMatrix_article, n = dim(NetMatrix_article)[1], Title = "Country Collaboration", 
       type = "circle", 
	   #type="kamada",
	   size=TRUE, 
	   remove.multiple=FALSE,labelsize=0.7,
	   cluster="leiden")
igraph_article <- net_article$graph 
library(RCy3)
library(igraph)
createNetworkFromIgraph(igraph_article,"igraph_article")

file_review <- c("./Data/Scopus_review.bib")
M_review <- convert2df(file_review, dbsource = "scopus", format = "bibtex") 
## 1.3 国家及主要发文机构
M_review <- metaTagExtraction(M_review, Field = "AU_CO", sep = ";")
NetMatrix_review <- biblioNetwork(M_review, analysis = "collaboration", network = "countries", sep = ";")
net_review=networkPlot(NetMatrix_review, n = dim(NetMatrix_review)[1], Title = "Country Collaboration", 
       type = "circle", 
	   #type="kamada",
	   size=TRUE, 
	   remove.multiple=FALSE,labelsize=0.7,
	   cluster="leiden")
igraph_review <- net_review$graph 
library(RCy3)
library(igraph)
createNetworkFromIgraph(igraph_review,"igraph_review")


		
pdf('result02.pdf',width = 10,height = 8)
p1
p2
p3
dev.off()


	


