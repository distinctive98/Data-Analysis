library(rvest)
library(stringr)
library(XML)
library(KoNLP)
library(dplyr) #filter
library(wordcloud2)

#setwd
setwd('C:/Users/student/Desktop/Data-Analysis/R/MStock')
getwd()

#종목 이름과 코드
company <- c('삼성SDI','현대모비스','SK하이닉스','NAVER','LG전자','셀트리온','아모레퍼시픽','신세계','신한은행','카카오','S-Oil','한국콜마')
code <- c('006400','012330','000660','035420','066570','068270','090430','004170','055550','035720','010950','161890')

#필터링 단어 모음
filterWords <- c('삼성','SDI','삼성SDI','현대','모비스','SK','하이닉스','네이버'
                  ,'LG','전자','셀트리온','아모레퍼시픽','신세계','KT'
                  ,'카카오','에스','오일','한국콜마','콜마','한국','오전'
                  ,'오후','올해','지난해','내일','모레','어제','오늘'
                  ,'이날','들이','하기','비롯','이번','예정','파이낸셜','관련종목'
                  ,'뉴스','때문','현대모비스','케이','엘지','NAVER','naver'
                  ,'만원','에쓰오일','신한','은행','신한은행'
                  ,'co','kr','com','www','https','http','hani','기자','연합뉴스')

newsList <- c()

#파이낸셜 종목 검색 결과
f_url_main <- paste0('http://www.fnnews.com/search?search_txt=', company[1])
f_html_main <- read_html(f_url_main, encoding = "UTF-8")

for(i in 1:5){
  f_node_main <- html_node(f_html_main, paste0('#container > div > div > div.section_list > div.bd > ul > li:nth-child(',i,') > strong > a'))
  
  #파이낸셜 뉴스 기사
  f_url_news <- html_attr(f_node_main, 'href')
  f_html_news <- read_html(f_url_news, encoding="UTF-8")
  f_node_news <- html_node(f_html_news, '#article_content')
  
  #파이낸셜 기사 추출 및 정제
  emailExp <- "[[:alnum:]]+[@][[:alnum:]]+[.][[[:alnum:]]+|[[:alnum:]]+[.][[:alnum:]]+]" #이메일 정규표현식
  f_text <- html_text(f_node_news)
  email <- regmatches(f_text, regexpr(emailExp, f_text)) #기자 이메일 추출
  f_text <- unlist(strsplit(f_text, email))[1] #이메일 뒤 제거
  f_text <- gsub('[[:digit:][:punct:][:cntrl:]]','', f_text) #숫자, 특수문자, 공백 제거
  f_text <- str_trim(f_text) #앞뒤 여백 제거
  
  newsList <- c(newsList, f_text)
}

#조선일보 종목 검색
j_url_main <- paste0('http://nsearch.chosun.com/search/total.search?query=',company[1],'&sort=0')
j_html_main <- read_html(j_url_main, encoding = "UTF-8")

for(i in 1:5){
  j_node_main <- html_node(j_html_main, paste0('#Wrap > div.schCont > div > div.l_area > div.search_news_box > dl:nth-child(',(3+i),') > dt > a'))

  #조선일보 기사
  j_url_news <- html_attr(j_node_main, 'href')
  j_html_news <- read_html(j_url_news, encoding = "UTF-8")
  j_node_news <- html_node(j_html_news, '#news_body_id > div.par')
  
  #조선일보 기사 추출 및 정제
  j_text <- html_text(j_node_news)
  j_text <- gsub('[[:digit:][:punct:][:cntrl:]]', '', j_text)
  
  newsList <- c(newsList, j_text)
}

#한국경제 종목 검색
h_url_main <- paste0('https://search.hankyung.com/apps.frm/search.news?query=', company[1], '&sort=RANK%2FDESC%2CDATE%2FDESC&period=ALL&area=ALL&mediaid_clust=HKPAPER&exact=&include=&except=')
h_html_main <- read_html(h_url_main, encoding = "UTF-8")

for(i in 1:5){
  h_node_main <- html_node(h_html_main, paste0('#content > div.left_cont > div > div.section.hk_news > div > ul > li:nth-child(',i,') > div > a'))
  
  #한국경제 기사
  h_url_news <- html_attr(h_node_main, 'href')
  h_html_news <- read_html(h_url_news, encoding = "UTF-8")
  h_node_news <- html_node(h_html_news, '#articletxt')
  
  #한국경제 기사 추출 및 정제
  emailExp <- "[[:alnum:]]+[@][[:alnum:]]+[.]" #이메일 정규표현식
  h_text <- html_text(h_node_news)
  email <- regmatches(h_text, regexpr(emailExp, h_text)) #기자 이메일 추출
  h_text <- unlist(strsplit(h_text, email))[1]
  h_text <- gsub('[[:digit:][:punct:][:cntrl:]]', '', h_text)
  h_text <- str_trim(h_text)
  
  newsList <- c(newsList, h_text)
}

#2차 정제
#기사별 단어 리스트화
textData <- sapply(newsList, extractNoun, USE.NAMES = F)

#워드클라우드
textData_wc <- Filter(function(x){2 <= nchar(x) & nchar(x) <= 6}, unlist(textData))
textData_wc_df <- data.frame(sort(table(textData_wc), decreasing = T))
textData_wd_df <- textData_wc_df %>% filter(!textData %in% filterWords)
head(textData_wd_df)
textCloud <- wordcloud2(textData_wd_df[1:100,])

#감정분석
textList <- list()
for(i in 1:15){
  textList[[i]] <- Filter(function(x){2 <= nchar(x) & nchar(x) <= 6}, textData[[i]])
  textList[[i]] <- data.frame(sort(table(textList[[i]]), decreasing = T))
  names(textList[[i]])[1] <- 'textData'
  textList[[i]] <- textList[[i]] %>% filter(!textData %in% filterWords)
}

#감정사전                  
positive <- readLines('positive.txt')
negative <- readLines('negative.txt')

#긍정, 부정 단어
text_pos <- textList
text_neg <- textList

#기사마다 긍정, 부정필터 적용
#긍/부정 단어별 Freq 개수 기반으로 문서에 대한 긍/부정 판단
emotion <- c()
for(i in  1:15){
  text_pos[[i]] <- text_pos[[i]] %>% filter(textData %in% positive)
  text_neg[[i]] <- text_neg[[i]] %>% filter(textData %in% negative)
  pos <- if(length(text_pos[[i]][,1]) > 0) sum(text_pos[[i]][2]) else 0
  neg <- if(length(text_neg[[i]][,1]) > 0) sum(text_neg[[i]][2]) else 0
  emotion[i] <- if(pos > neg) 'pos' else 'neg'
}

#단어테이블 생성
table_em <- Filter(function(x){2 <= nchar(x) & nchar(x) <= 6}, unlist(textData))
table_em <- data.frame(table(table_em))[1]
table_em <- table_em %>% filter(!table_em %in% filterWords) %>% filter(table_em %in% positive | table_em %in% negative)
names(table_em) <- c('word')
head(table_em)

#긍/부정테이블 * 단어테이블
col_cnt <- 2 #col count
for(i in  1:15){
  text_em <- data.frame()
  text_em <- rbind(text_pos[[i]], text_neg[[i]])[1]
  if(length(text_em[,1]) > 0){
    names(text_em) <- c('word')
  
    temp_df <- c()
    for(word in table_em$word){
      if(any(word == text_em)){
        #print(TRUE)
        temp_df <- c(temp_df,1)
      } else {
        #print(FALSE)
        temp_df <- c(temp_df,0)
      }
    }
    temp_df <- data.frame(temp_df)
    table_em <- cbind(table_em,temp_df)
    names(table_em)[col_cnt] <- paste0('d',col_cnt-1)
    col_cnt <- col_cnt + 1
  }
}

#TF-IDF 구하기
tf_idf <- data.frame()
#TF : 문서 내 단어의 개수/문서 내 모든 단어의 개수
tf <- c()
tf_bot_df <- data.frame(Filter(function(x){2 <= nchar(x) & nchar(x) <= 6}, unlist(textData)))
names(tf_bot_df)[1] <- 'word'
tf_bot_df <- tf_bot_df %>% filter(word %in% positive | word %in% negative)
tf_top_df <- data.frame(table(tf_bot_df)) %>% filter(tf_bot_df %in% positive | tf_bot_df %in% negative)
names(tf_top_df)[1] <- 'word'
head(tf_top_df)
tf_bot <- length(tf_bot_df[,1])

tf_top <- c()
for(table_word in table_em$word){
  tf_top_list <- subset(tf_top_df, word==table_word)[2]
  tf_top <- c(tf_top, tf_top_list[,1])
}

tf <- tf_top / tf_bot
#tf_idf <- cbind(tf)

#IDF : 문서 전체 개수 / 단어를 포함한 문서의 수
idf <- c()
idf_top <- col_cnt-1
#text_all = text_pos + text_neg
text_all <- list()
for(i in 1:15){
  text_all[[i]] <- data.frame()
  if(length(text_pos[[i]][,1] > 0)){
    text_all[[i]] <- rbind(text_all[[i]], text_pos[[i]])
  }
  if(length(text_neg[[i]][,1] > 0)){
    text_all[[i]] <- rbind(text_all[[i]], text_neg[[i]])
  }
  names(text_all[[i]]) <- c('word', 'Freq')
}

idf_bot <- c()
for(table_word in table_em$word){
  word_cnt <- 0
  for(i in 1:15){
    if(length(subset(text_all[[i]], word==table_word)[,1]) > 0){
      word_cnt <- word_cnt + 1
    }
  }
  idf_bot <- c(idf_bot, word_cnt)
}
idf <- idf_top / idf_bot
idf <- log(idf)
tf_idf <- tf * idf




#이데일리 종목 검색 결과
e_url_main <- paste0('https://www.edaily.co.kr/search/E00.html?searchbody=&jname=&searchtitle=',company[1],'&searchSort=accuracy')
e_html_main <- read_html(e_url_main, encoding = "UTF-8")
e_node_main <- html_node(e_html_main, '#component_template_id_SEARCH_NEWS_bXUs4k6rkv0peEX > div.web_widget.id_thum_main_news > div > div.arti_data > ul > li:nth-child(1) > a')

#서울경제
s_url_main <- paste0('http://sentv.co.kr/search/news?findex=&srh_day=&srh_option=1&skeyword=', company[1])
s_html_main <- read_html(s_url_main, encoding = "UTF-8")
s_node_main <- html_node(s_html_main, '#content > div > div > div.tab_wrap > ul.tab_cont > li:nth-child(2) > div > section > div.news_list > ul > li:nth-child(1) > div > a')
