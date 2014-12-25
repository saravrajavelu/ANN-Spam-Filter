

list <- c("to"          
          ,"for"         
          ,"and"         
          ,"on"          
          ,"you"        
          ,"this"        
          ,"i"           
          ,"is"          
          ,"in"                 
          ,"be"          
          ,"from"        
          ,"have"        
          ,"we"          
          ,"at"          
          ,"that"       
          ,"deal"        
          ,"will"        
          ,"with"      
          ,"if"          
          ,"please"      
          ,"are"                  
          ,"not"         
          ,"or"          
          ,"as"         
          ,"by"                         
          ,"meter"       
          ,"me"                  
          ,"your"        
          ,"was"         
          ,"can"         
          ,"need"       
          ,"thanks"     
          ,"any"         
          ,"message"    
          ,"may"         
          ,"should"      
          ,"our"                  
          ,"know"        
          ,"original"   
          ,"do"         
          ,"has"         
          ,"all"                  
          ,"been"        
          ,"these"       
          ,"day"         
          ,"they"        
          ,"volume"     
          ,"up"          
          ,"out"         
          ,"there"      
          ,"contract"    
          ,"no"         
          ,"more"        
          ,"here"        
          ,"only"        
          ,"us"          
          ,"information"
          ,"new"         
          ,"company"     
          ,"price"       
          ,"free"        
          ,"get"        
          ,"one"         
          ,"mail"        
          ,"investment"  
          ,"my"          
          ,"but"                
          ,"about"       
          ,"now"         
          ,"which"                 
          ,"its"         
          ,"prices"      
          ,"business"    
          ,"contact"     
          ,"news"       
          ,"over"        
          ,"report"      
          ,"money"      
          ,"stock"       
          ,"their"       
          ,"securities"  
          ,"statements"  
          ,"time"        
          ,"many"       
          ,"reply"       
          ,"looking")
frame_name <- c("to"          
          ,"for"         
          ,"and"         
          ,"on"          
          ,"you"        
          ,"this"        
          ,"i"           
          ,"is"          
          ,"in"                 
          ,"be"          
          ,"from"        
          ,"have"        
          ,"we"          
          ,"at"          
          ,"that"       
          ,"deal"        
          ,"will"        
          ,"with"      
          ,"if"          
          ,"please"      
          ,"are"                  
          ,"not"         
          ,"or"          
          ,"as"         
          ,"by"                         
          ,"meter"       
          ,"me"                  
          ,"your"        
          ,"was"         
          ,"can"         
          ,"need"       
          ,"thanks"     
          ,"any"         
          ,"message"    
          ,"may"         
          ,"should"      
          ,"our"                  
          ,"know"        
          ,"original"   
          ,"do"         
          ,"has"         
          ,"all"                  
          ,"been"        
          ,"these"       
          ,"day"         
          ,"they"        
          ,"volume"     
          ,"up"          
          ,"out"         
          ,"there"      
          ,"contract"    
          ,"no"         
          ,"more"        
          ,"here"        
          ,"only"        
          ,"us"          
          ,"information"
          ,"new"         
          ,"company"     
          ,"price"       
          ,"free"        
          ,"get"        
          ,"one"         
          ,"mail"        
          ,"investment"  
          ,"my"          
          ,"but"                
          ,"about"       
          ,"now"         
          ,"which"                 
          ,"its"         
          ,"prices"      
          ,"business"    
          ,"contact"     
          ,"news"       
          ,"over"        
          ,"report"      
          ,"money"      
          ,"stock"       
          ,"their"       
          ,"securities"  
          ,"statements"  
          ,"time"        
          ,"many"       
          ,"reply"       
          ,"looking","total_word_count","label")

ham <- function(){
        setwd("C:/Users/sarav rajavelu/Sarav/My Study Material/Coursera/R_sandbox/mini_project/Mini Project/spamset/ham")
        dir_list <- dir()
        len <- length(dir_list)
        base <- data.frame(matrix(0, ncol = (length(list)+2), nrow = len))
        names(base) <- frame_name
        for(i in 1:len){
                con <- file(dir_list[i],"r")
                file<-readLines(con)
                file <- paste(file, collapse = "")
                count <- 0
                while(grepl("[[:alnum:]]+",file,TRUE)){
                        match <- regexpr("[[:alnum:]]+",file,TRUE)
                        for (j in seq(list)){
                                
                                if(list[j] ==  regmatches(file,match)){
                                        base[i,j] <- base[i,j] + 1
                                        break
                                }
                        }
                        count<- count  + 1
                        file <- sub("[[:alnum:]]+","",file)
                }
                for(j in seq(list)){
                        base[i,j] <- (base[i,j]/count)*100
                }
                base[i,87] <- count
                base[i,88] <- "Ham"
                close(con)
                print(base[i,])
        }
        write.csv(base, file = "ham.csv")
}
        
                
spam <- function(){
        setwd("C:/Users/sarav rajavelu/Sarav/My Study Material/Coursera/R_sandbox/mini_project/Mini Project/spamset/spam")
        dir_list <- dir()
        len <- length(dir_list)
        base <- data.frame(matrix(0, ncol = (length(list)+2), nrow = len))
        names(base) <- frame_name
        for(i in 1:len){
                con <- file(dir_list[i],"r")
                file<-readLines(con)
                file <- paste(file, collapse = "")
                count <- 0
                while(grepl("[[:alnum:]]+",file,TRUE)){
                        match <- regexpr("[[:alnum:]]+",file,TRUE)
                        for (j in seq(list)){
                                
                                if(list[j] ==  regmatches(file,match)){
                                        base[i,j] <- base[i,j] + 1
                                        break
                                }
                        }
                        count<- count  + 1
                        file <- sub("[[:alnum:]]+","",file)
                }
                for(j in seq(list)){
                        base[i,j] <- (base[i,j]/count)*100
                }
                base[i,87] <- count
                base[i,88] <- "Spam"
                close(con)
                print(base[i,])
        }
        write.csv(base, file = "spam.csv")
}

predict_mail  <- function(){
        library(caret)
        library(nnet)
        library(ggplot2)
        library(lattice)
        mailset <- read.csv('mailset.csv')
        set.seed(nrow(mailset))
        mailset <- mailset[sample(nrow(mailset)),]
        split <- floor(nrow(mailset)/2)
        mailTrain <- mailset[0:split,]
        mailTest <- mailset[(split+1):nrow(mailset),]
        
        cylModel <- multinom(label~., data = mailTrain, maxit=100, trace=T)
        
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
la <- mailset[,90]
l <- numeric()
for(i in 1:length(la)){if(la[i] == 'Spam')l[i] <- 1 else l[i] <- 0}
mod1 <- nnet(mailset[,3:89],l,size=10,linout = T)
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