# install.packages("rvest")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("lubridate")

library("httr")
library("rvest")
library("dplyr")
library("stringr")
library("ggplot2")
library("lubridate")
library("tidyverse")

header <- c("Title","Authors","CAuthor","CAuthorEmail","PublishedDate","Abstract","Keywords","Access","WebofScience","CrossRef","DOI","URL")
link <- "https://parasitesandvectors.biomedcentral.com/articles"
extended_link <- "?searchType=journalSearch&sort=PubDate&page="
base_link <- "https://parasitesandvectors.biomedcentral.com"

get_abstract <- function(article_link, session_page) {
  session_page <- session_page %>% session_jump_to(url = article_link)
  webpage <- session_page %>% read_html()
  abstract <- webpage %>% html_nodes("#Abs1-content p") %>% 
              html_text() %>% unlist() %>% paste(collapse = " ") %>% as.character()
  session_page <- session_page %>% session_back()
  return(abstract)
}

get_cauthors <- function(article_link, session_page) {
  session_page <- session_page %>% session_jump_to(url = article_link)
  webpage <- session_page %>% read_html()
  cauthors <- webpage %>% html_nodes("#corresp-c1") %>% html_text()
  cauthors_mail <- webpage %>% html_nodes("#corresp-c1") %>% html_attr(name = "href") %>% 
                   str_extract(pattern = "mailto:(.*)", group = 1)
  session_page <- session_page %>% session_back()
  ifelse(identical(cauthors, character(0)),return(c("","")),return(c(cauthors,cauthors_mail)))
}

get_metrics <- function(article_link, session_page) {
  metrics_link <- paste(article_link, "/metrics", sep = "")
  session_page <- session_page %>% session_jump_to(url = metrics_link)
  webpage <- session_page %>% read_html()
  article_access <- webpage %>% html_nodes("li:nth-child(1) dt") %>% html_text() %>% trimws()
  article_wos <- webpage %>% html_nodes("li:nth-child(2) dt") %>% html_text() %>% trimws()
  article_crossref <- webpage %>% html_nodes("li~ li+ li dt") %>% html_text() %>% trimws()
  session_page <- session_page %>% session_back()
  return(c(article_access,article_wos,article_crossref))
}

get_doi <- function(article_link, session_page) {
  session_page <- session_page %>% session_jump_to(url = article_link)
  webpage <- session_page %>% read_html()
  doi <- webpage %>% html_nodes(".c-bibliographic-information__list-item--full-width .c-bibliographic-information__value") %>% html_text()
  session_page <- session_page %>% session_back()
  return(doi)
}

get_keywords <- function(article_link, session_page) {
  session_page <- session_page %>% session_jump_to(url = article_link)
  webpage <- session_page %>% read_html()
  keywords <- webpage %>% html_nodes(".c-article-subject-list__subject a") %>% html_text() %>% trimws() %>% paste(collapse = ", ") %>% as.character()
  session_page <- session_page %>% session_back()
  return(keywords)
}

get_articles <- function(link, extended_link, base_link) {
  agent <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36")
  session_page <- session(link, agent)
  num_of_pages <- session_page %>% read_html() %>% html_nodes("#main-content .u-text-sm.u-reset-margin") %>% html_text() %>% 
                  str_extract(pattern = "of\\s(\\d*)", group = 1) %>% as.numeric()
  articles <- data.frame()
  i <- 1
  while (session_page$response$status_code == 200 & i <= num_of_pages) {
    print(paste("Scraping data from Page:", i))
    page_link <- paste(link,extended_link,as.character(i),sep = "")
    session_page <- session_page %>% session_jump_to(url = page_link)
    article_name <- session_page %>% read_html() %>% html_nodes(".c-listing__title a") %>% html_text()
    article_link <- session_page %>% read_html() %>% html_nodes(".c-listing__title a") %>% html_attr(name = "href") %>% paste(base_link, ., sep = "")
    authors <- session_page %>% read_html() %>% html_nodes(".c-listing__authors-list") %>% html_text()
    published_date <- session_page %>% read_html() %>% html_nodes(".c-listing__metadata span+ span") %>% html_text() %>% 
                      str_extract(pattern = "on:\\s(\\d{1,2}\\s[A-Za-z]*\\s\\d{4})", group = 1)
    abstract <- sapply(article_link, FUN = get_abstract, session_page, USE.NAMES = FALSE)
    article_metrics <- sapply(article_link, FUN = get_metrics, session_page, USE.NAMES = FALSE)
    article_doi <- sapply(article_link, FUN = get_doi, session_page, USE.NAMES = FALSE)
    cauthors <- sapply(article_link, FUN = get_cauthors, session_page, USE.NAMES = FALSE)
    keywords <- sapply(article_link, FUN = get_keywords, session_page, USE.NAMES = FALSE)
    articles_tmp <- data.frame(article_name,authors,cauthors[1,],cauthors[2,],published_date,abstract,keywords,article_metrics[1,],
                                article_metrics[2,],article_metrics[3,],article_doi,article_link,stringsAsFactors = FALSE)
    articles <- rbind(articles, articles_tmp)
    i <- i + 1
  }
  colnames(articles) <- header
  ArticlesUpdated <- articles
  save(ArticlesUpdated, file = "ArticlesUpdated.RData")
}

update_articles <- function(link, extended_link, base_link) {
  load("ArticlesUpdated.RData")
  agent <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36")
  session_page <- session(link, agent)
  num_of_pages <- session_page %>% read_html() %>% html_nodes("#main-content .u-text-sm.u-reset-margin") %>% html_text() %>% 
                  str_extract(pattern = "of\\s(\\d*)", group = 1) %>% as.numeric()
  i <- 1
  while (session_page$response$status_code == 200 & i <= num_of_pages) {
    print(paste("Scraping data from Page:", i))
    page_link <- paste(link,extended_link,as.character(i),sep = "")
    session_page <- session_page %>% session_jump_to(url = page_link)
    article_name <- session_page %>% read_html() %>% html_nodes(".c-listing__title a") %>% html_text()
    article_link <- session_page %>% read_html() %>% html_nodes(".c-listing__title a") %>% html_attr(name = "href") %>% paste(base_link, ., sep = "")
    authors <- session_page %>% read_html() %>% html_nodes(".c-listing__authors-list") %>% html_text()
    published_date <- session_page %>% read_html() %>% html_nodes(".c-listing__metadata span+ span") %>% html_text() %>% 
                      str_extract(pattern = "on:\\s(\\d{1,2}\\s[A-Za-z]*\\s\\d{4})", group = 1)
    abstract <- sapply(article_link, FUN = get_abstract, session_page, USE.NAMES = FALSE)
    article_metrics <- sapply(article_link, FUN = get_metrics, session_page, USE.NAMES = FALSE)
    article_doi <- sapply(article_link, FUN = get_doi, session_page, USE.NAMES = FALSE)
    cauthors <- sapply(article_link, FUN = get_cauthors, session_page, USE.NAMES = FALSE)
    keywords <- sapply(article_link, FUN = get_keywords, session_page, USE.NAMES = FALSE)
    ifelse(identical(ArticlesUpdated$DOI[1], article_doi[1]), break, ifelse((ArticlesUpdated$DOI[1] %in% article_doi),(i <- num_of_pages + 1),(i <- i + 1)))
    articles_tmp <- data.frame(article_name,authors,cauthors[1,],cauthors[2,],published_date,abstract,keywords,article_metrics[1,],
                               article_metrics[2,],article_metrics[3,],article_doi,article_link,stringsAsFactors = FALSE)
    colnames(articles_tmp) <- header
    ArticlesUpdated <- rbind(articles_tmp, ArticlesUpdated)
  }
  ArticlesUpdated <- distinct(ArticlesUpdated, DOI, .keep_all = TRUE)
  save(ArticlesUpdated, file = "ArticlesUpdated.RData")
  print("Articles are updated and saved to ArticlesUpdated.RData")
}

clean_df <- function() {
  load("ArticlesUpdated.RData")
  articles_df <- ArticlesUpdated
  articles_df$Title <- articles_df$Title %>% trimws()
  articles_df$Authors <- articles_df$Authors %>% str_split(pattern = ",\\s+|\\s+and\\s+")
  articles_df$CAuthor <- articles_df$CAuthor %>% trimws()
  articles_df$CAuthorEmail <- articles_df$CAuthorEmail %>% trimws()
  articles_df$PublishedDate <- articles_df$PublishedDate %>% as.Date(format = "%d %B %Y")
  articles_df$Abstract <- articles_df$Abstract %>% trimws()
  articles_df$Keywords <- articles_df$Keywords %>% str_split(pattern = ",\\s+") %>% sapply(., str_replace_all, pattern = "\\s+", replacement = " ")
  articles_df$Access <- articles_df$Access %>% str_replace(., pattern = "(\\d*)k$", replacement = "\\1000") %>% as.numeric()
  articles_df$WebofScience <- articles_df$WebofScience %>% str_replace(., pattern = "(\\d*)k$", replacement = "\\1000") %>% as.numeric()
  articles_df$CrossRef <- articles_df$CrossRef %>% str_replace(., pattern = "(\\d*)k$", replacement = "\\1000") %>% as.numeric()
  return(articles_df)
}

