str <- "Subject: coca cola , mbna america , nascar partner with otcbb : imtsstockprofileaboutcompanyinvestmenthighlightspress release12 / 01 / 2003indianapolis , in - race car simulators ? inks the sale of eight simulators for installation in moscow09 / 17 / 2003indianapolis , in - nascar silicon motor speedway ? simulators go international09 / 05 / 2003indianapolis , in - nascar silicon motor speedway ? expands to monterey , california ' s famed cannery row09 / 02 / 2003indianapolis , in - nascar silicon motor speedway ? announces custom upgrades to world ' s most realistic racing simulation08 / 14 / 2003indianapolis , in - race car simulators ? and baldacci sign agreement to develop international markets for the new generation race simulutors08 / 12 / 2003indianapolis , in - imts forms new subsidiary for manufacturing and sales of race car simulators08 / 07 / 2003indianapolis , in - nascar silicon motor speedway ? renews licensing agreement with speedway motorsports , inc . , for race track simulators08 / 05 / 2003indianapolis , in - nascar silicon motor speedway ? , int . speedway corp . renew licensing agreement for race track simulators07 / 27 / 2003indianapolis , in - nascar silicon motor speedway ? simulators to be installed at st . louis nascar speedpark location07 / 24 / 2003indianapolis , in - nascar silicon motor speedway ? operator gets exclusive five - year nascar license extension05 / 30 / 2003nashville , tn - nascar silicon motor speedway ? at opry mills to host official media luncheon for nashville superspeedway ' s trace adkins chrome 300 event04 / 22 / 2003indianapolis , in - nascar silicon motor speedway ? simulators now running at nascar speedpark03 / 19 / 2003indianapolis , in - nascar silicon motor speedway ? expansion plans begin at two burroughs chapin entertainment venues02 / 27 / 2003indianapolis , in - nascar silicon motor speedway ? to determine national champion among simulator racers02 / 14 / 2003indianapolis , in - partnerships with coca - cola , mbna andin demand boost nascar silicon motor speedway ? racing centers02 / 28 / 2003indianapolis , in - nascar drivers sadler , nadeau give thumbs up to indianapolis simulation at nascar silicon motor speedway ?02 / 22 / 2003indianapolis , in - star studded lineup for make a wish fundraiser at nashville nascar silicon motor speedway location01 / 14 / 2003indianapolis , in - indianapolis motor speedway to be added to nascar silicon motor speedway simulators* * * * * * * important notice anddisclaimer : please read * * * * * * *intelligent stock picks , and affiliates ( isp ) , publishes reports providing information on selected companies that isp believes has investment potential . isp is not a registered investment advisor or broker - dealer . this report is provided as an information service only , and the statements and opinions in this report should not be construed as an offer or solicitation to buy or sell any security . isp accepts no liability for any loss arising from an investor ' s reliance on or use of this report . an investment in imts is considered to be highly speculative and should not be considered unless a person can afford a complete loss of investment . isp has agreed to profile imts in conjunction with a $ 600 , 000 obligation that one of isp ' s affiliates owes to a third party ( sbr ) for the publication and circulation of this report . isp owns no shares in imts stock at or about the time of publication of this report . subsequently isp may buy or sell shares of imts stock in the open market . this report contains forward - looking statements , which involve risks , and uncertainties that may cause actual results to differ materially from those set forth in the forward - looking statements . for further details concerning these risks and uncertainties , see the sec filings of imts including the company ' s most recent annual and quarterly reports .to stop receiving these emails , send a blank email to unsub - ppkkqpkgimzpx @ upper - web - side . com"

spam <- function(){
        setwd("C:/Users/sarav rajavelu/Sarav/My Study Material/Coursera/R_sandbox/mini_project/Mini Project/spamset/spam")
        
  list <- c('word_count','avg_word_length','spl_char_count','digit_count','space_count','char_count','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','small_word_count','large_word_count','label')
  dir_list <- dir()
  len <- length(dir_list)
  base <- data.frame(matrix(0, ncol = (length(list)), nrow = len))
  names(base) <- list
  for(i in 1:len){
    con <- file(dir_list[i],"r")
    str <-readLines(con)
    str <- paste(str, collapse = "")
    g <- gregexpr("[[:alnum:]]+",str,TRUE)
    base[i,1] <- length(attr(g[[1]],'match.length'))
    
    u <- attr(g[[1]],'match.length')
    base[i,43] <- length(u[u<4])
    base[i,44] <- length(u[u>8])
    
    g <- gregexpr("[[:alnum:]]+",str,TRUE)
    s <- sum(attr(g[[1]],'match.length'))
    base[i,2] <- s/base[i,1]
    
    g <- gregexpr("[[:punct:]]",str,TRUE)
    base[i,3] <- length(attr(g[[1]],'match.length'))
    
    g <- gregexpr("[[:digit:]]",str,TRUE)
    base[i,4] <- length(attr(g[[1]],'match.length'))
    
    g <- gregexpr("[[:blank:]]",str,TRUE)
    base[i,5] <- length(attr(g[[1]],'match.length'))
    
    g <- gregexpr("[[:print:]]",str,TRUE)
    base[i,6] <- length(attr(g[[1]],'match.length'))
    
    
    abc <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9')
    abc_d <- character(36)
    for ( j in 7:42){
      abc_d[i] <- paste("[",abc[j-6],"]",sep = "")
      g <- gregexpr(abc_d[i],str,TRUE)
      base[i,j] <- length(attr(g[[1]],'match.length'))
    }
    
    
    base[i,45] <- "Spam"
    close(con)
    print(base[i,])
  }
  write.csv(base, file = "spam_final.csv")
}

ham <- function(){
        setwd("C:/Users/sarav rajavelu/Sarav/My Study Material/Coursera/R_sandbox/mini_project/Mini Project/spamset/ham")
        
  list <- c('word_count','avg_word_length','spl_char_count','digit_count','space_count','char_count','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','small_word_count','large_word_count','label')
  dir_list <- dir()
  len <- length(dir_list)
  base <- data.frame(matrix(0, ncol = (length(list)), nrow = len))
  names(base) <- list
  for(i in 1:len){
    con <- file(dir_list[i],"r")
    str <-readLines(con)
    str <- paste(str, collapse = "")
    g <- gregexpr("[[:alnum:]]+",str,TRUE)
    base[i,1] <- length(attr(g[[1]],'match.length'))
    
    u <- attr(g[[1]],'match.length')
    base[i,43] <- length(u[u<4])
    base[i,44] <- length(u[u>8])
    
    g <- gregexpr("[[:alnum:]]+",str,TRUE)
    s <- sum(attr(g[[1]],'match.length'))
    base[i,2] <- s/base[i,1]
    
    g <- gregexpr("[[:punct:]]",str,TRUE)
    base[i,3] <- length(attr(g[[1]],'match.length'))
    
    g <- gregexpr("[[:digit:]]",str,TRUE)
    base[i,4] <- length(attr(g[[1]],'match.length'))
    
    g <- gregexpr("[[:blank:]]",str,TRUE)
    base[i,5] <- length(attr(g[[1]],'match.length'))
    
    g <- gregexpr("[[:print:]]",str,TRUE)
    base[i,6] <- length(attr(g[[1]],'match.length'))
    
    
    abc <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9')
    abc_d <- character(36)
    for ( j in 7:42){
      abc_d[i] <- paste("[",abc[j-6],"]",sep = "")
      g <- gregexpr(abc_d[i],str,TRUE)
      base[i,j] <- length(attr(g[[1]],'match.length'))
    }
    
    
    base[i,45] <- "Ham"
    close(con)
    print(base[i,])
  }
  write.csv(base, file = "ham_final.csv")
}



predict_mail  <- function(){
  library(caret)
  library(nnet)
  library(ggplot2)
  library(lattice)
  setwd("C:/Users/sarav rajavelu/Sarav/My Study Material/Coursera/R_sandbox/mini_project/Mini Project")
  
  mailset <- read.csv('mailset_final.csv')
  mailset <- mailset[,3:47]
  set.seed(nrow(mailset))
  mailset <- mailset[sample(nrow(mailset)),]
  split <- floor(nrow(mailset)/2)
  mailTrain <- mailset[0:split,]
  mailTest <- mailset[(split+1):nrow(mailset),]
  
  cylModel <- multinom(label~., data = mailTrain, maxit=1000, trace=T)
  
  mostImportantVariables <- varImp(cylModel)
  mostImportantVariables$Variables <- row.names(mostImportantVariables)
  mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
  print(head(mostImportantVariables))
  preds1 <- predict(cylModel, type="probs", newdata=mailTest)
  preds2 <- predict(cylModel, type="class", newdata=mailTest)
  print(postResample(mailTest$label,preds2))
  
  
  head(preds2)
  head(preds1)
}

plot_mail_nnet <- function(){
  library(nnet)
  library(RCurl)
  library(scales)
  library(reshape)

  library(bitops)
  mailset <- read.csv('mailset.csv')
  set.seed(nrow(mailset))
  mailset <- mailset[sample(nrow(mailset)),]
  la <- mailset[,47]
  l <- numeric()
  for(i in 1:length(la)){if(la[i] == 'Spam')l[i] <- 1 else l[i] <- 0}
  mod1 <- nnet(mailset[,3:46],l,size=21,linout = T,maxit = 1000)
  require(RCurl)
  
  #root.url<-'https://gist.github.com/fawda123'
  #raw.fun<-paste(
  #        root.url,
  #        '5086859/raw/17fd6d2adec4dbcf5ce750cbd1f3e0f4be9d8b19/nnet_plot_fun.r',
  #        sep='/'
  #)
  #script<-getURL(raw.fun, ssl.verifypeer = FALSE)
  #eval(parse(text = script))
  #rm('script','raw.fun')
  par(mar=numeric(4),mfrow=c(1,2),family='serif')
  plot(mod1,nid=F)
  plot(mod1)
}

cycle_predict <- function(){
        
        totalAccuracy <- c()
        cv <- 10
        mailset <- read.csv('mailset_final.csv')
        cvDivider <- floor(nrow(mailset) / (cv+1))
        
        for (cv in seq(1:cv)) {
                # assign chunk to data test
                dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
                dataTest <- mailset[dataTestIndex,]
                
                # everything else to train
                dataTrain <- mailset[-dataTestIndex,]
                print(nrow(dataTrain))
                cylModel <- multinom(label~., data=dataTrain, maxit=500, trace=T) 
                
                pred <- predict(cylModel, newdata=dataTest, type="class")
                
                #  classification error
                cv_ac <- postResample(dataTest$label, pred)[[1]]
                print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
                totalAccuracy <- c(totalAccuracy, cv_ac)
        }
        totalAccuracy
        mean(totalAccuracy) 
}


word_count <- function(str){
        g <- gregexpr("[[:alnum:]]+",str,TRUE)
        count <- length(attr(g[[1]],'match.length'))
        print(count)
}
avg_word_length <- function(str){
        g <- gregexpr("[[:alnum:]]+",str,TRUE)
        s <- sum(attr(g[[1]],'match.length'))
        count <- length(attr(g[[1]],'match.length'))
        print(s/count)
}

spl_char_count <- function(str){
        g <- gregexpr("[[:punct:]]",str,TRUE)
        count <- length(attr(g[[1]],'match.length'))
        print(count)
}

digit_count <- function(str){
        g <- gregexpr("[[:digit:]]",str,TRUE)
        count <- length(attr(g[[1]],'match.length'))
        print(count)
}
space_count <- function(str){
        g <- gregexpr(" [[:alnum:]]{1,3} ",str,TRUE)
        count <- length(attr(g[[1]],'match.length'))
        print(count)
}
char_count <- function(str){
        g <- gregexpr("[[:print:]]",str,TRUE)
        count <- length(attr(g[[1]],'match.length'))
        print(count)
}
upper_count <- function(str){
        g <- gregexpr("[[:upper:]]",str,TRUE)
        count <- length(attr(g[[1]],'match.length'))
        print(count)
}

alpha_details <- function(str){
        abc <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9')
        abc_d <- character(36)
        abc_count <- numeric(36)
        for ( i in 1:length(abc)){
                abc_d[i] <- paste("[",abc[i],"]",sep = "")
                g <- gregexpr(abc_d[i],str,TRUE)
                abc_count[i] <- length(attr(g[[1]],'match.length'))
        }
        print(abc_count)
}