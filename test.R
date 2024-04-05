load("Articles.RData")
View(Articles)
ArticlesUpdated <- Articles
save(ArticlesUpdated, file = "ArticlesUpdated.RData")
dups <- ArticlesUpdated[duplicated(Articles),]
View(dups)
test <- Articles[Articles$DOI == "https://doi.org/10.1186/s13071-018-2614-1",]
View(test)