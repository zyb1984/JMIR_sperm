
library(bibliometrix)
file_article <- c("./data/Scopus_article.bib")
M_article <- convert2df(file_article, dbsource = "scopus", format = "bibtex") 
results_article <- biblioAnalysis(M_article)
file_review <- c("./data/Scopus_review.bib")
M_review <- convert2df(file_review, dbsource = "scopus", format = "bibtex") 
results_review <- biblioAnalysis(M_review)

## Article_Authors’ Dominance ranking
DF_article <- dominance(results_article, k = 10)
DF_article
write.csv(DF_article,"top_article.CSV")
## Review_Authors’ Dominance ranking
DF_review <- dominance(results_review, k = 10)
DF_review
write.csv(DF_review,"top_review.CSV")
# Bornmann's impact indices:
topAU_article <- authorProdOverTime(M_article, k = 10, graph = TRUE)
top_article <- topAU_article$graph
topAU_review <- authorProdOverTime(M_review, k = 10, graph = TRUE)
top_review <- topAU_review$graph
pdf('Impact_indices.pdf',width = 6,height = 6)
top_article
top_review
dev.off()

# Article_collaboration_network 
NetMatrix_article <- biblioNetwork(M_article, analysis = "collaboration", network = "authors", sep = ";")
net_article=networkPlot(NetMatrix_article, 
     normalize = "salton", 
	 weighted=TRUE, 
	 n = 30, 
	 Title = "Authors' Coupling", 
	 type = "fruchterman", 
	 size=20,
	 size.cex=T,
	 remove.multiple=TRUE,
	 labelsize=0.5,
	 label.n=50,
	 label.cex=F)
igraph_article <- net_article$graph 
library(RCy3)
library(igraph)
createNetworkFromIgraph(igraph_article,"article_Igraph")
#Review_collaboration_network 
NetMatrix_review <- biblioNetwork(M_review, analysis = "collaboration", network = "authors", sep = ";")
net_review=networkPlot(NetMatrix_review, 
     normalize = "salton", 
	 weighted=TRUE, 
	 n = 30, 
	 Title = "Authors' Coupling", 
	 type = "fruchterman", 
	 size=20,
	 size.cex=T,
	 remove.multiple=TRUE,
	 labelsize=0.5,
	 label.n=50,
	 label.cex=F)
	 
igraph_review <- net_review$graph 
library(RCy3)
library(igraph)
createNetworkFromIgraph(igraph_review,"review_Igraph")	 