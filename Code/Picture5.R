library(bibliometrix)
# Load all required packages
library(tidyverse)        # Also loads dplyr, ggplot2, and haven
library(quanteda)         # For NLP
library(readtext)         # To read .txt files
library(stm)              # For structural topic models
library(stminsights)      # For visual exploration of STM
library(wordcloud)        # To generate wordclouds
library(gsl)              # Required for the topicmodels package
library(topicmodels)      # For topicmodels
library(caret)            # For machine learning
file <- c("./Data/scopus_article.bib")
M <- convert2df(file, dbsource = "scopus", format = "bibtex") 
M_sub <- M[,c(6,11,17,21,25)]
## [1] 12521     5
M_sub <- M_sub[!is.na(M_sub[,1]),]                   
M_sub <- M_sub[complete.cases(M_sub[,1]),]
## [1] 11920     5
M_sub <- data.frame(M_sub)
M_sub$doc_id <- paste0("text",1:nrow(M_sub),sep="")
library("quanteda")
## Createcorpus
corpus <- corpus(M_sub,docid_field="doc_id", text_field = "AB",
               meta = list(c(M_sub$SN,M_sub$pmid,M_sub$TI,M_sub$PY)))

# Create tokens
token <- tokens(
    corpus,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE, 
	remove_separators=TRUE,
    split_tags =TRUE,
	include_docvars = TRUE
  )
## Lowercase text
token <- tokens_tolower(token)
MESH_Stopwords <- c("a", "about", "again", "all", "almost", "also", "although", "always", "among", "an", "and", "another", "any", "are", "as", "at","be", "because", "been", "before", "being", "between", "both", "but", "by", 
		    "can", "could", "did", "do", "does", "done", "due", "during", "each", "either", "enough", "especially", "etc", "for", "found", "from", "further", "had", "has", "have", "having", "here", "how", "however",
		    "i", "if, in, into","is", "it", "its", "itself", "just", "kg", "km", "made", "mainly", "make", "may", "mg", "might", "ml", "mm", "most", "mostly", "must", "nearly", "neither", "no", "nor", "obtained", 
		    "of", "often", "on", "our", "overall", "perhaps", "pmid", "quite", "rather", "really", "regarding", "seem", "seen", "several", "should", "show", "showed", "shown", "shows", "significantly", "since", "so", "some", 
		    "such", "than", "that", "the", "their", "theirs", "them", "then", "there", "therefore", "these", "they", "this", "those", "through", "thus", "to", "upon",  "various", "very", "was", "we", "were", "what", 
		    "when", "which", "while", "with", "within", "without", "would")
LLM_Stopwords <- c("accordingly", "addition", "adventurous", "after", "all", "also", "although", "amazing", "ambitious", "ancient", "and", "angry", "anxious", "any", "arrogant", "as", "awful", "bad", "beautiful", "because", "before", 
		     "besides", "big", "bitter", "bored", "boring", "brave", "bright", "but", "calm", "case", "cautious", "cheap", "clean", "clever", "cold", "complex","conjunctions", "consequently", "contrary", "contrast", "cool", 
		     "cowardly","creative", "cruel", "curious", "dangerous", "dark", "deceitful", "delicious", "difficult", "dirty", "disgusting", "dry","dull","eager","easy", "eight", "eighth", "either", "elderly", "empty","energetic",
		    "even", "excited", "expensive", "fabulous", "fact", "fast", "fat", "few", "fifth", "firm", "first", "five", "for", "four", "fourth", "fresh", "full","furthermore", "generous", "gentle", "go", "good", "handsome", 
		    "happy", "hard", "healthy", "heavy", "hence", "high",  "honest",  "hot", "how", "however", "humble", "if", "important", "in", "indifferent", "instead", "interesting", "joyful", "kind", "large", "lazy", "least", 
		   "lest", "light", "like", "likewise", "lively", "long", "loose", "loud", "low", "lucky", "many", "meanwhile", "modern", "moreover", "most", "much", "mysterious", "neither", "nervous", "nevertheless", "new", "nine", 
		   "ninth", "noisy", "nonetheless", "nor", "not", "odd", "old","on", "once","one", "only", "or", "otherwise", "polite", "poor", "powerful", "pretty", "provided", "quick", "quiet", "reckless", "result", "rich", "rough", 
		   "rude", "sad", "safe", "salty", "scared",  "scary", "second", "seven", "seventh", "shiny", "short", "silent", "similarly", "simple", "since","six", "sixth", "slow", "small", "smooth", "so", "soft", "some", "sour", 
		   "stale", "stingy", "strong", "sweet", "tall", "ten",  "tenth", "than", "that", "the", "therefore", "these", "thin", "third", "this", "those", "though", "thoughtful", "thoughtless", "three", "thus", "tiny", "tired", 
		    "two","ugly", "unhealthy", "unimaginative", "unimportant", "unless", "until", "useful", "useless", "vast","warm","weak", "wet","what", "when", "whenever","where", "whereas","wherever", "whether", "which", "while", "who", "whose", "yet","young", "youthful","zany")
## Define and eliminate all custom stopwords
myStopwords <- c(stopwords("english"),
 "people", "time", "day", "years", "just", "really", "one", "can", "like", "go", "get", "gt",
 "amp", "now", "one","two","h", "p", "ci","per","use","s", "n", "y","der","und","r","kg","ii","get", "much", "many", "every", "lot", "even", "also")
token <- tokens_select(token, MESH_Stopwords, selection = "remove")
token <- tokens_select(token, LLM_Stopwords, selection = "remove")
token <- tokens_select(token, myStopwords, selection = "remove")
## wordStem uses Martin Porter's stemming algorithm and the C libstemmer library generated by Snowball
token <- tokens_wordstem(token)
##Remove punctuation again
token <- tokens(token, remove_punct = TRUE)
token <- tokens_select(token, myStopwords, selection = "remove", case_insensitive = TRUE)
token <- tokens_select(token, MESH_Stopwords, selection = "remove", case_insensitive = TRUE)
token_DEstop <- tokens_select(token, LLM_Stopwords , selection = "remove", case_insensitive = TRUE)

## we then create the document-feature matrix. 
## We lower and stem the words (tolower and stem) and remove common stop words (remove=stopwords()). 
##Stopwords are words that appear in texts but do not give the text a substantial meaning (e.g., “the”, “a”, or “for”)
mydfm <- dfm(token_DEstop, tolower = TRUE)
##[1] 11920 51056


topfeatures(mydfm,n=100)
##   sperm        patient       infertil            men          studi 
#         26161          22282          14820          14224          13322 
##          male          group     testicular           cell         fertil 
#         12703          12385           9577           9430           8756 
##   azoospermia          semen          level         normal    spermatozoa 
#          8572           8182           8086           7149           6850 
##          gene           rate      treatment        analysi          motil 
#          6769           6404           6214           6105           5946 

mydfm_remove <- dfm_select(mydfm, pattern = stopwords("english"), selection = "remove", valuetype = "fixed")
##[1] 12067 51056
		 
## Trim the text with dfm_trim, we filter words that appear less than 1% and more than 95%. 
mydfm.trim <- dfm_trim(mydfm_remove,
          # min_docfreq = 0.01, # min 1.0%
	  max_docfreq = 0.99, #  max 99.0%
	  docfreq_type = "prop" )


##1 Classification
##1.1 Determine k number of topics
library("topicmodels")
library("Rmpfr")
library("ldatuning")
#
ldatuning.metrics <- FindTopicsNumber(mydfm.trim, 
       topics = seq(from = 2, to = 20, by = 1), 
	   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), 
	   method = "Gibbs", 
	   control = list(seed = 77), mc.cores = 2L, verbose = TRUE
)
 FindTopicsNumber_plot(ldatuning.metrics)

k <- 16
burnin <- 1000
iter <- 1000
keep <- 50
LDA_model <- topicmodels::LDA(mydfm.trim, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )
tmResult <- posterior(LDA_model)


library("ggwordcloud")

plotlist = list()
for (n in 1:k) {
    topicToViz <- n
	top50terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:50]
	words <- names(top50terms)
	probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:50]
    # visualize the terms as wordcloud
    p_topic_n   <-  ggplot(data= data.frame(words, probabilities))+
      geom_text_wordcloud(aes(label=words,size=probabilities))+
	  ## scale_radius(range = c(0, 30), limits = c(0, NA)) +
	  scale_radius(range = c(2.5, 30), limits = c(0, NA)) +
	  theme_minimal()
	plotlist[[n]] = p_topic_n  
	}
	 
library(gridExtra)
p_topic <- marrangeGrob(grobs=plotlist, nrow=4, ncol = 4)

#2 Topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta) 
##[1]   15 1543
# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics 
dim(theta)   
exampleIds <- c(2, 100, 200)
cat(corpus[exampleIds[1]])
cat(corpus[exampleIds[2]])
cat(corpus[exampleIds[3]])

# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")

# What are the most probable topics in the entire collection?
topicProportions <- colSums(theta) / nrow(mydfm.trim)  # mean probablities over all paragraphs
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order
countsOfPrimaryTopics <- rep(0, k)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nrow(mydfm.trim)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)  
# get mean topic proportions per decade
topic_proportion_per_year <- aggregate(theta, by = list(year = M_sub$PY), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year)[2:(k+1)] <- topicNames


# reshape data frame
library("reshape2")
vizDataFrame <- melt(topic_proportion_per_year, id.vars = "year")
# plot topic proportions per year as bar plot
require(pals)
topic_year <- ggplot(vizDataFrame, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(25), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pdf('result12.pdf',width = 40,height = 25)
FindTopicsNumber_plot(ldatuning.metrics)
p_topic
topic_year
dev.off() 	 
