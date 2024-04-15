library(bibliometrix)
file_article <- c("./data/Scopus_article.bib")
M_article <- convert2df(file_article, dbsource = "scopus", format = "bibtex") 
results_article <- biblioAnalysis(M_article)
file_review <- c("./data/Scopus_review.bib")
M_review <- convert2df(file_review, dbsource = "scopus", format = "bibtex") 
results_review <- biblioAnalysis(M_review)
## Rank article_authors based on the H-index
indices_article <- Hindex(M_article, field = "author", sep = ";")
authors_H <- indices_article$H 
authors_top <- head(authors_H[order(authors_H$h_index,decreasing = T),], n=10)

## Rank review_authors based on the H-index
indices_review <- Hindex(M_review, field = "author", sep = ";")
review_H <- indices_review$H 
authors_review_top <- head(review_H[order(review_H$h_index,decreasing = T),], n=10)

## Top article_author Productivity over Time

M <- M_article
k <- 100
if (!("DI" %in% names(M))){M$DI="NA"}
M$TC <- as.numeric(M$TC)
M$PY <- as.numeric(M$PY)
M <- M[!is.na(M$PY),] #remove rows with missing value in PY
library("dplyr")
Y <- as.numeric(substr(Sys.time(),1,4))
  listAU <- (strsplit(M$AU, ";"))
  nAU <- lengths(listAU)
  df <- data.frame(AU=trimws(unlist(listAU)), SR=rep(M$SR, nAU)) 
  AU <- df %>% 
    group_by(AU) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    ungroup() 
k <- min(k ,nrow(AU))
 AU <- AU %>% 
    slice_head(n=k)

df <- df %>% 
    right_join(AU, by = "AU") %>%
    left_join(M, by = "SR") %>% 
    select("AU.x","PY","TI","SO","DI","TC") %>% 
    mutate(TCpY = TC/(Y-PY+1)) %>%
    group_by(AU.x) %>% 
    mutate(n = length(AU.x)) %>% 
    ungroup() %>% 
    rename(Author = AU.x,
           year = PY,
           DOI = DI) %>% 
    arrange(desc(n), desc(year)) %>% 
    select(-n)
df2 <- dplyr::group_by(df, Author,year) %>%
    dplyr::summarise(freq=length(year),TC=sum(TC),TCpY=sum(TCpY)) %>% 
    as.data.frame()
df2$Author <- factor(df2$Author, levels=AU$AU[1:k])
library("data.table")
top_article_authors <- authors_top$Element
df_article_top <- setDT(df2, key="Author") [J(top_article_authors)]

x <- c(0.5,1.5*k/10)
y <- c(min(df$year),min(df$year)+diff(range(df2$year))*0.125)

library("ggplot2")
Article_top <- ggplot(df_article_top, 
            aes(x=Author, y=year, 
			     text = paste("Author: ", Author,"\nYear: ",year ,"\nN. of Articles: ",freq ,"\nTotal Citations per Year: ", round(TCpY,2))))+
    geom_point(aes(alpha=TCpY,size = freq), color="dodgerblue4")+ 
    scale_size(range=c(2,6))+
    scale_alpha(range=c(0.3,1))+
    scale_y_continuous(breaks = seq(min(df_article_top$year),max(df_article_top$year), by=2))+
    guides(size = guide_legend(order = 1, "N.Articles"), alpha = guide_legend(order = 2, "TC per Year"))+
    theme(legend.position = 'right'
          ,text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = '#FFFFFF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 90)#, face="bold")
          ,axis.title.x = element_text(hjust = .95)#,face="bold")
          ,axis.text.x = element_text(face="bold", angle = 90)
          ,axis.text.y = element_text(face="bold")
          #,axis.line.x = element_line(color="black", size=1)
          ,axis.line.x = element_line(color="grey50", linewidth=0.5)
          ,panel.grid.major.x = element_blank() 
          ,panel.grid.major.y = element_line( linewidth=.2, color="grey90" ) 
    )+
    #coord_fixed(ratio = 2/1) +
    labs(title="Authors' Production over Time", 
         x="Author",
         y="Year")+
    geom_line(data=df_article_top,aes(x = Author, y = year, group=Author),size=1.0, color="firebrick4", alpha=0.3 )+
    scale_x_discrete(limits = rev(levels(df_article_top$Author)))+
    coord_flip() 

## Top review_author Productivity over Time
M <- M_review
k <- 100
if (!("DI" %in% names(M))){M$DI="NA"}
M$TC <- as.numeric(M$TC)
M$PY <- as.numeric(M$PY)
M <- M[!is.na(M$PY),] #remove rows with missing value in PY
library("dplyr")
Y <- as.numeric(substr(Sys.time(),1,4))
  listAU <- (strsplit(M$AU, ";"))
  nAU <- lengths(listAU)
  df <- data.frame(AU=trimws(unlist(listAU)), SR=rep(M$SR, nAU)) 
  AU <- df %>% 
    group_by(AU) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    ungroup() 
k <- min(k ,nrow(AU))
 AU <- AU %>% 
    slice_head(n=k)

df <- df %>% 
    right_join(AU, by = "AU") %>%
    left_join(M, by = "SR") %>% 
    select("AU.x","PY","TI","SO","DI","TC") %>% 
    mutate(TCpY = TC/(Y-PY+1)) %>%
    group_by(AU.x) %>% 
    mutate(n = length(AU.x)) %>% 
    ungroup() %>% 
    rename(Author = AU.x,
           year = PY,
           DOI = DI) %>% 
    arrange(desc(n), desc(year)) %>% 
    select(-n)
df2 <- dplyr::group_by(df, Author,year) %>%
    dplyr::summarise(freq=length(year),TC=sum(TC),TCpY=sum(TCpY)) %>% 
    as.data.frame()
df2$Author <- factor(df2$Author, levels=AU$AU[1:k])
library("data.table")
top_review_authors <- authors_review_top$Element
df_review_top <- setDT(df2, key="Author") [J(top_review_authors)]

x <- c(0.5,1.5*k/10)
y <- c(min(df$year),min(df$year)+diff(range(df2$year))*0.125)

library("ggplot2")
Review_top <- ggplot(df_review_top, 
            aes(x=Author, y=year, 
			     text = paste("Author: ", Author,"\nYear: ",year ,"\nN. of Articles: ",freq ,"\nTotal Citations per Year: ", round(TCpY,2))))+
    geom_point(aes(alpha=TCpY,size = freq), color="dodgerblue4")+ 
    scale_size(range=c(2,6))+
    scale_alpha(range=c(0.3,1))+
    scale_y_continuous(breaks = seq(min(df_review_top$year),max(df_review_top$year), by=2))+
    guides(size = guide_legend(order = 1, "N.Articles"), alpha = guide_legend(order = 2, "TC per Year"))+
    theme(legend.position = 'right'
          ,text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = '#FFFFFF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 90)#, face="bold")
          ,axis.title.x = element_text(hjust = .95)#,face="bold")
          ,axis.text.x = element_text(face="bold", angle = 90)
          ,axis.text.y = element_text(face="bold")
          #,axis.line.x = element_line(color="black", size=1)
          ,axis.line.x = element_line(color="grey50", linewidth=0.5)
          ,panel.grid.major.x = element_blank() 
          ,panel.grid.major.y = element_line( linewidth=.2, color="grey90" ) 
    )+
    #coord_fixed(ratio = 2/1) +
    labs(title="Authors' Production over Time", 
         x="Author",
         y="Year")+
    geom_line(data=df_review_top,aes(x = Author, y = year, group=Author),size=1.0, color="firebrick4", alpha=0.3 )+
    scale_x_discrete(limits = rev(levels(df_review_top$Author)))+
    coord_flip() 
library("cowplot")
library(showtext)
Author_top <- ggdraw() +     
      draw_plot(Article_top, 0, 0.5, 1, 0.5) +  
      draw_plot(Review_top, 0, 0, 1, 0.5)  
pdf('Picture2.pdf',width = 6,height = 6)
Author_top 
dev.off()
