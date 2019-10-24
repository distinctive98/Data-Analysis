#setwd
setwd('C:/Users/student/Desktop/Data-Analysis/MStock/R')
getwd()

textData <- source('news_scraping.R', encoding = "UTF-8")

#워드클라우드
textData_wc <- Filter(function(x){2 <= nchar(x) & nchar(x) <= 6}, unlist(textData))
textData_wc_df <- data.frame(sort(table(textData_wc), decreasing = T))
textData_wd_df <- textData_wc_df %>% filter(!textData_wc %in% filterWords)
head(textData_wd_df)
textCloud <- wordcloud2(textData_wd_df[1:100,])