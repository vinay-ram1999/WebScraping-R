source("scrape.R")

# Only use this function to scrape all articles from the URL (this takes a lot of time to run i.e. around ~6 hours)
#get_articles(link, extended_link, base_link)

# Use this function to scrape the latest published articles and append them to the existing scraped data
update_articles(link, extended_link, base_link)

# To view the raw scraped dataframe load it using the following code
load("ArticlesUpdated.RData")
View(ArticlesUpdated)

# Use this function to clean the raw scraped dataframe
articles_df <- clean_df()
View(articles_df)
str(articles_df)

annual_analytics <- function(year, df) {
  df <- articles_df
  df <- df[format(df$PublishedDate,"%Y") == year,]
  df$Month <- format(df$PublishedDate, "%b")
  p1 <- ggplot(data = df, mapping = aes(x=reorder(Month, order(df$PublishedDate)))) + geom_bar(fill="#69b3a2", color="#e9ecef") + 
              labs(x="Month", y="Number of Articles", title = paste("Number of Articles per Month in",year))
  ggsave(paste(dir,"NumOfArticles.png", sep = "/"), plot = p1, width = 9, height = 9)
  keywords <- unlist(as.vector(df$Keywords))
  keywords <- unlist(sapply(keywords, str_split, pattern = "\\s+", USE.NAMES = FALSE))
  keywords <- data.frame(keywords)
  keywords_clean <- data.frame(keywords[keywords$keywords != "",])
  colnames(keywords_clean) <- c("keywords")
  keywords <- keywords_clean %>% group_by(keywords) %>% summarise(count = n()) %>% arrange(desc(count))
  p2 <- ggplot(data = keywords[1:10,], mapping = aes(x=keywords,y=count)) + geom_bar(stat = "identity", fill="#69b3a2", color="#e9ecef") + 
              labs(x="Keyword", y="Number of times", title = paste("Number of times keyword used"))
  ggsave(paste(dir,"KeywordFreq.png", sep = "/"), plot = p2, width = 9, height = 9)
}

annual_analytics("2009",articles_df)
