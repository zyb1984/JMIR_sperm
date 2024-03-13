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
##[1]0.0   57.0  203.5  529.0 1356.5 2249.0
breaks_Review <- classIntervals(articles$Review, n =5,style = "kmeans")$brks
breaks_Review
##[1]  0.0   9.0  36.5 116.0 465.5 740.0
breaks_All <- classIntervals(articles$All, n =5,style = "kmeans")$brks
breaks_All
## [1]    0.0   81.5  254.0  574.0 1518.0 2989.0
World <- merge(World,articles,by="iso_a3")
p_country<- tm_shape(World) +
		   tm_polygons(fill = "All",alpha = 0.7, 
	              fill.scale =  tm_scale_intervals(values = "scico.roma")) +
	      tm_text("name", size = "AREA") +
		  tm_bubbles(size="Review", col = "Article", 
	 	       border.col = "black", border.alpha = .5, style="fixed", breaks=c(0,seq(0, 2500, by=500), 2500),
		 	   palette="brewer.rd_bu", contrast=1) 

library("ggplot2")
articles_country <- read.csv("./Data/Ratio_country.csv", header=T)
Ratio_rank <- ggplot(articles_country,aes(x = reorder(Country, Ratio),  Ratio)) +  
   geom_col() +
   geom_hline(aes(yintercept=0.1527), colour="red", linetype="dashed")+
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

















		

library(readxl)
library(albersusa)
library(biscale)
library(sf)
library(tidyverse)
library(hrbrthemes)
library(ggtext)
library(scatterpie)
library("ggsci")
articles <- read.csv(file.choose(),header=T)
rownames(articles) <- articles[,2]
articles <- articles[,-c(1,2)]

articles<- st_as_sf(articles, coords = c(x ="lon", y = "lat")) %>% as.data.frame(stringsAsFactors = F)




%>% st_transform(crs = 2343)%>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                lat = sf::st_coordinates(.)[,2])%>% select(Article,Review,All,lon,lat) %>% data.frame(stringsAsFactors = F) 
p4 <- ggplot() +
    geom_sf(data = World,fill="gray90",size=.125,color="black")+
    #geom_sf(data = World,aes(fill = "All"))+
    # scale_fill_gradient(low = "white",  high = "red",n.breaks = 6)+
    geom_scatterpie(aes(x=lon,y=lat,r=All*0.01),data=articles,cols=c("Review", "Article"),alpha=.9)+
    geom_scatterpie_legend(articles$All*0.01,x=-50, y=20,n=3, labeller=function(x) x/40)+
    scale_fill_jco(name="Type")






		   
	
		
pdf('result02.pdf',width = 10,height = 8)
p1
p2
p3
dev.off()


	


