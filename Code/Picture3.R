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
 ##              Element h_index g_index PY_start   TC  NP   m_index
27836       SCHLEGEL PN      54      96     1993 9401 105 1.6875000
22574       NIESCHLAG E      50      85     1978 7478 110 1.0638298
31437        TOURNAYE H      46      75     1991 8008  75 1.3529412
9951          FORESTA C      41      68     1992 4836  84 1.2424242
7664          DEVROEY P      38      50     1986 9937  50 0.9743590
18409      LIPSHULTZ LI      37      65     1979 4670  65 0.8043478
9620           FERLIN A      34      57     1995 3599  57 1.1333333
32212 VAN STEIRTEGHEM A      34      47     1994 4531  47 1.0967742
10630         GAROLLA A      32      50     1996 3196  50 1.1034483
33013            WANG X      32      53     2003 3664 148 1.4545455


## Rank review_authors based on the H-index
indices_review <- Hindex(M_review, field = "author", sep = ";")
review_H <- indices_review$H 
authors_review_top <- head(review_H[order(review_H$h_index,decreasing = T),], n=10)
         Element h_index g_index PY_start   TC NP   m_index
44       AGARWAL A      27      39     2004 3326 39 1.2857143
1351    ESTEVES SC      21      25     2011 1645 25 1.5000000
2556      KRAUSZ C      21      24     1999 3774 24 0.8076923
4208   SCHLEGEL PN      21      30     1997 1252 30 0.7500000
1420      FERLIN A      13      14     2000 1613 14 0.5200000
1476     FORESTA C      13      15     2000 1605 15 0.5200000
4742    TOURNAYE H      13      20     1994 1263 20 0.4193548
656    CALOGERO AE      12      18     2000  598 18 0.4800000
3907    RAMASAMY R      12      18     2012  468 18 0.9230769
916  CONDORELLI RA      11      14     2013  708 14 0.9166667
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
