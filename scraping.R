#Group No. = 9 
#Journal Number = 7 Genome Biology http://genomebiology.biomedcentral.com/
#Group Members = Madhuri Chanumolu(mc882)  Manish Patel(mp933) Reneel Sagar Pamarthi(rp826)

library(RCurl)
library(XML)
library(stringr)
library(rvest)
library(xlsx)
crawl_article <- function(year){
    volume = 0
    if(year>2020 | year<2000){
      stop("Entered I/P is not correct.")
    } else {
      volume = (year %% 2000) + 1 
    }
    journal = "https://genomebiology.biomedcentral.com/"
    journal_clear = "https://genomebiology.biomedcentral.com"
    
    list_of_articles = function(page_url){
      link_of_article = page_url %>% read_html() %>% html_nodes("#main-content > div > main > div:nth-child(3) > ol > li > article > div:nth-child(1) > h3 > a") %>% html_attr("href")
      publish_date_str = page_url %>% read_html() %>% html_nodes("#main-content > div > main > div:nth-child(3) > ol > li > article > div > div > div > p:nth-child(2) > span") %>% html_text()
      published_date = as.numeric(str_extract(publish_date_str, "[0-9]{4}+"))
      return(data.frame(DATE=published_date,url=link_of_article))
    }
    
    #list_of_links<-as.list(journal%>%read_html()%>%html_nodes(".c-navbar__link")%>%html_attr("href"))
    #page_list_url = paste(journal_clear,list_of_links[3],"/",sep="")
    page_list_url = sprintf("https://genomebiology.biomedcentral.com/articles?tab=keyword&searchType=journalSearch&sort=PubDate&volume=%d&page=1",volume)
    max_str = page_list_url %>% read_html() %>% html_nodes("#main-content > div > main > div:nth-child(3) > h2 > strong") %>% html_text()
    max_articles = as.numeric(str_extract(max_str, "[0-9]+"))
    if(max_articles %% 50 == 0)
      max_pages = max_articles / 50
    else
      max_pages = (max_articles / 50) + 1 
    page_url = sprintf("https://genomebiology.biomedcentral.com/articles?tab=keyword&searchType=journalSearch&sort=PubDate&volume=%d&page=",volume)
    
    #generate article url list
    page_url_list = ""
    articles_data = data.frame(DATE=c(),url=c())
    
    for(i in 1:max_pages){
      cat("\r",paste("Page number ",i," Loading..."))
      full_url = paste(page_url,i,sep="")
      articlesUrl_and_DATE = list_of_articles(full_url)#function call: list_of_articles()
      articlesUrl_and_DATE$url = paste(journal_clear, articlesUrl_and_DATE$url, sep="")#add site url, to fulfill the url of an article
      articles_data = rbind(articles_data, articlesUrl_and_DATE)#rown bind, store all articles urls and DATE
      page_url_list = c(page_url_list, full_url)
    }
    page_url_list = page_url_list[-1]
    write(page_url_list, "page_url_list.txt")
    print("[I/O]: FILE: page_url_list.txt created.")
    write.csv(articles_data, "article_DATEandURL_list.csv")
    print("[I/O]: FILE: article_DATEandURL_list.csv created.")
    
    Analysis_of_article = function(DATE, url){
          options(warn=-1)
          doc = url %>% read_html()
          DATE = DATE
          title = doc %>% html_nodes("#main-content > main > article > div > h1") %>% html_text()
          
          author_list = doc %>% html_nodes("#main-content > main > article > div > ul.c-author-list.js-list-authors.js-etal-collapsed > li > span > a") %>% html_text()
          author = paste(unlist(author_list),collapse = ";")
          
          authorAffiliation = doc %>% html_nodes("#author-information-content > ol.c-article-author-affiliation__list") %>% html_text()
          correspondingAuthor = doc %>% html_nodes("#corresp-c1") %>% html_text()
          correspondingAuthorEmail = paste(journal_clear,doc %>% html_nodes("#corresp-c1") %>% html_attr("href"),"")
          publicationDate = doc %>% html_nodes("#main-content > main > article > div > ul.c-article-identifiers > li:nth-child(3) > a > time") %>% html_text()
          abstract = doc %>% html_nodes("#Abs1-content") %>% html_text()
          
          keywords_list = doc %>% html_nodes("#article-info-content > div > div:nth-child(2) > ul.c-article-subject-list > li > span") %>% html_text()
          keywords = paste(unlist(keywords_list),collapse = ";")
          
          fullText = paste("https:",doc %>% html_nodes("#main-content > div > aside > div.c-pdf-download.u-clear-both > a") %>% html_attr("href"),sep = "")
          
          if(length(abstract)==0) abstract <- NA
          if(length(correspondingAuthor)==0) correspondingAuthor <- NA
          if(length(authorAffiliation)==0) authorAffiliation <- NA
          extract_data_from_row = data.frame(DATE, title, author, authorAffiliation, correspondingAuthor,
                                             correspondingAuthorEmail, publicationDate, abstract, keywords, fullText)
          return(extract_data_from_row)
    }

    extracted_data = data.frame("DOP"=c(),"Title"=c(), "Author"=c(), "Author_Affiliation"=c(), "Corresponding_Author"=c(), "Corresponding_Author_email"=c(), 
                                "Publication_Date"=c(), "Abstract"=c(), "Keywords"=c(), "Full Text"=c())
    total_number = as.integer(length(articles_data[,1]))
    for(i in 1:total_number){
      cat("\r",paste("Crawling articles from article ",i,"..."))
      extracted_data = rbind(extracted_data, Analysis_of_article(articles_data[i,1], articles_data[i,2]))#function call: Analysis_of_article
    }
    
    options(warn=-1)
    dir.create("output")
    write.csv(extracted_data,file = "output/Genome_Biology.csv", append = TRUE)
    print("[I/O]: FILE: output/Genome_Biology.csv created.")
}


crawl_article(2018)

