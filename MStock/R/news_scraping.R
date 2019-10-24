library(rvest)
library(stringr)
library(XML)
library(KoNLP)
library(dplyr) #filter
library(wordcloud2)

#setwd
setwd('C:/Users/student/Desktop/Data-Analysis/MStock/R')
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

