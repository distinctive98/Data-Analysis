#감정분석

#setwd
setwd('C:/Users/student/Desktop/Data-Analysis/MStock/R')
getwd()

#뉴스 데이터 불러오기
textData <- source('news_scraping.R', encoding = "UTF-8")
textData <- textData$value

#감정사전                  
positive <- readLines('positive.txt')
negative <- readLines('negative.txt')

#단어 정제
textList <- list()
for(i in 1:15){
  textList[[i]] <- Filter(function(x){2 <= nchar(x) & nchar(x) <= 6}, textData[[i]])
  textList[[i]] <- data.frame(sort(table(textList[[i]]), decreasing = T))
  names(textList[[i]])[1] <- 'textData'
  textList[[i]] <- textList[[i]] %>% filter(!textData %in% filterWords)
}

#긍정, 부정 단어
text_pos <- textList
text_neg <- textList

#기사마다 긍정, 부정필터 적용
#긍/부정 단어별 Freq 개수 기반으로 문서에 대한 긍/부정 판단
emotion <- c('class')
for(i in  1:15){
  text_pos[[i]] <- text_pos[[i]] %>% filter(textData %in% positive)
  text_neg[[i]] <- text_neg[[i]] %>% filter(textData %in% negative)
  pos <- if(length(text_pos[[i]][,1]) > 0) sum(text_pos[[i]][2]) else 0
  neg <- if(length(text_neg[[i]][,1]) > 0) sum(text_neg[[i]][2]) else 0
  if(pos != 0 | neg != 0){
    emotion <- c(emotion, if(pos > neg) 'pos' else 'neg')  
  }
}

#단어테이블 생성
#긍/부정 단어 리스트 생성
table_word <- Filter(function(x){2 <= nchar(x) & nchar(x) <= 6}, unlist(textData))
#table_word_cnt <- data.frame(table(table_word))
#names(table_word_cnt)[1] <- 'word'
#table_word_cnt <- table_word_cnt %>% filter(!word %in% filterWords) %>% filter(word %in% positive | word %in% negative)
table_word <- data.frame(unique(table_word), stringsAsFactors = F)
names(table_word)[1] <- 'word'
table_word <- table_word %>% filter(!word %in% filterWords) %>% filter(word %in% positive | word %in% negative)
#head(table_word)
#str(table_word)

#단어테이블에 문서별 긍/부정 단어 카운트 벡터 추가
#존재만 하면 1로 간주, 없으면 0으로 간주
table_em <- table_word
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
    
    names(table_em)[col_cnt] <- paste0('d',i)
    col_cnt <- col_cnt + 1
  }
}

#단어테이블 완성
table_em <- rbind(table_em, emotion)
str(table_em)

#긍/부정 테이블 구분
index_i <- length(table_em[,1])
index_j <- length(table_em[1,])
pos_index <- which(table_em[index_i,] == 'pos')
neg_index <- which(table_em[index_i,] == 'neg')

table_pos <- table_word
table_neg <- table_word
table_pos <- cbind(table_pos, table_em[-index_i,pos_index])
table_neg <- cbind(table_neg, table_em[-index_i,neg_index])

#TF-IDF 구하기
#IDF : 문서 전체 개수 / 단어를 포함한 문서의 수
idf <- c()
#idf_top <- col_cnt-1
idf_top <- length(table_em)-1
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
for(table_word in table_em$word[-length(table_em$word)]){
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

#긍정 테이블에 대한 TF 구하기
#긍정 인덱스 추출
names_pos <- names(table_pos[-1])
names_pos <- gsub('d', '', names_pos)
names_pos <- as.numeric(names_pos)

#각 단어에 대한 긍정 가중치 계산(TF-IDF 구하기)
max_i <- length(table_pos[1,])
max_j <- length(table_pos[,1])
for(i in 2:max_i){
  tf_bot <- sum(textList[[names_pos[i-1]]][2])
  for(j in 1:max_j){
     if(table_pos[j,i] == 1){
       word_index <- which(textList[[names_pos[i-1]]][1] == table_pos[j,1])
       tf_top <- textList[[names_pos[i-1]]][word_index,2]
       tf <- tf_top / tf_bot
       table_pos[j, i] <- tf * idf[j]
     }
  }
}

#긍정 합,평균 가중치 구하기
pos_cnt <- max_i-1
sum_pos <- c()
avg_pos <- c()
for(i in 1:max_j){
  sum <- sum(as.numeric(table_pos[i,][2:max_i]))
  sum_pos <- c(sum_pos, sum)
  avg <- mean(as.numeric(table_pos[i,][2:max_i]))
  avg_pos <- c(avg_pos, avg)
}

#긍정 평균 가중치 합치기
table_pos <- cbind(table_pos, sum_pos)
table_pos <- cbind(table_pos, avg_pos)


#부정 테이블에 대한 TF 구하기
#부정 인덱스 추출
names_neg <- names(table_neg[-1])
names_neg <- gsub('d', '', names_neg)
names_neg <- as.numeric(names_neg)

#각 단어에 대한 부정 가중치 계산(TF-IDF 구하기)
max_i <- length(table_neg[1,])
max_j <- length(table_neg[,1])
for(i in 2:max_i){
  tf_bot <- sum(textList[[names_neg[i-1]]][2])
  for(j in 1:max_j){
    if(table_neg[j,i] == 1){
      word_index <- which(textList[[names_neg[i-1]]][1] == table_neg[j,1])
      tf_top <- textList[[names_neg[i-1]]][word_index,2]
      tf <- tf_top / tf_bot
      table_neg[j, i] <- tf * idf[j]
    }
  }
}

#부정 평균 가중치 구하기
neg_cnt <- max_i-1
sum_neg <- c()
avg_neg <- c()
for(i in 1:max_j){
  sum <- sum(as.numeric(table_neg[i,][2:max_i]))
  sum_neg <- c(sum_neg, sum)
  neg <- mean(as.numeric(table_neg[i,][2:max_i]))
  avg_neg <- c(avg_neg, neg)
}

#부정 평균 가중치 합치기
table_neg <- cbind(table_neg, sum_neg)
table_neg <- cbind(table_neg, avg_neg)


#나이브-베이즈 분류기 적용
#word_cnt <- nrow(table_em)-1 
dc_cnt <- ncol(table_em)-1 
pos_cnt <- length(which(table_em[word_cnt+1,] == 'pos'))
neg_cnt <- length(which(table_em[word_cnt+1,] == 'neg'))
pos_rate <- pos_cnt / dc_cnt
neg_rate <- neg_cnt / dc_cnt

#긍/부정 문서 만들기
dcList <- textList
for(i in 1:dc_cnt){
  dcList[[i]] <- dcList[[i]] %>% filter(textData %in% positive | textData %in% negative)
}

#긍정값 구하기
nb_pos <- c()
for(i in i:dc_cnt){
  if(nrow(dcList[[i]]) > 0){
    word_cnt <- nrow(dcList[[i]])
    word_value <- table_pos %>% filter(word %in% dcList[[i]]$textData)
    nb_bot <- sum(word_value$avg_pos)
    nb_bot <- word_cnt + nb_bot
    nb_top <- c()
    
  } else {
    nb_pos <- c(nb_pos, 0) 
  }
}

###
### 어렵구만
# word_value <- table_pos %>% filter(word %in% dcList[[1]]$textData)
# dcList[[1]] %>% select(Freq)
# subset(dcList[[1]], textData %in% word_value$word)
# dcList[[1]]$textData
# dcList[[1]]$Freq
# word_cnt <- nrow(dcList[[1]])
# nb_bot <- sum(word_value$avg_pos)
# nb_bot <- word_cnt + nb_bot
# 
# 
# #긍정값 구하기
# nb_bot <- sum(table_pos$avg_pos)+word_cnt
# nb_top <- c()
# for(value in table_pos$avg_pos){
#   if(value != 0){
#     nb_top <- c(nb_top, value+1)
#   }
# }
# 
# nb_pos <- nb_top / nb_bot
# pos <- pos_rate
# for(value in nb_pos){
#   pos <- pos * value  
# }
# 
# #부정값 구하기
# nb_bot <- sum(table_neg$avg_neg)+word_cnt
# nb_top <- c()
# for(value in table_neg$avg_neg){
#   if(value != 0){
#     nb_top <- c(nb_top, value+1)
#   }
# }
# 
# nb_neg <- nb_top / nb_bot
# neg <- neg_rate
# for(value in nb_neg){
#   neg <- neg * value  
# }
# 
# 
# 
# 
# 
# 
# 
# names_neg <- names(table_neg[-1])
# names_neg <- gsub('d', '', names_neg)
# 
# 
# #tf_top <- subset(, textData="화재")
# word_index <- which(textList[[1]][1] == "화재")
# tf_top <- textList[[1]][word_index,2]
# tf_bot <- sum(textList[[1]][2])
# tf <- tf_top / tf_bot
# 
# tf_idf <- data.frame()
# #TF : 문서 내 단어의 개수/문서 내 모든 단어의 개수
# tf <- c()
# tf_bot_df <- data.frame(Filter(function(x){2 <= nchar(x) & nchar(x) <= 6}, unlist(textData)))
# names(tf_bot_df)[1] <- 'word'
# tf_bot_df <- tf_bot_df %>% filter(word %in% positive | word %in% negative)
# tf_top_df <- data.frame(table(tf_bot_df)) %>% filter(tf_bot_df %in% positive | tf_bot_df %in% negative)
# names(tf_top_df)[1] <- 'word'
# head(tf_top_df)
# tf_bot <- length(tf_bot_df[,1])
# 
# tf_top <- c()
# for(table_word in table_em$word){
#   tf_top_list <- subset(tf_top_df, word==table_word)[2]
#   tf_top <- c(tf_top, tf_top_list[,1])
# }
# 
# tf <- tf_top / tf_bot
# #tf_idf <- cbind(tf)
# 
# 
# 
# tf_idf <- tf * idf

