setwd("C:/Users/sarav rajavelu/Sarav/My Study Material/Coursera/R_sandbox")


ham <- read.table("HAM1.txt")
spam <- read.table("SPAM.txt")
a <- rep("Ham",40140)
a <- rep("Ham",40410)
ham$label <- a
nrow(spam)
a <- rep("Spam",33274)
spam$label <- a
total <- rbind(ham, spam)
q1 <- total[total$V3 == "this",]
q2 <- total[total$V3 == "free",]
q3 <- total[total$V3 == "security",]
total <- rbind(q1,q2,q3)
set.seed(3212)
total <- total[sample(nrow(total)),]
n1 <- total$V1
n2 <- total$V2
n3 <- total$V3
n4 <- total$label
total <- cbind(n1,n2,n3,n4)
total <- as.data.frame(total)

set.seed(3212)
total <- total[sample(nrow(total)),]


split <- floor(nrow(total)/2)
vehiclesTrain <- total[0:split,]
vehiclesTest <- total[(split+1):nrow(total),]
library(nnet)
cylModel <- multinom(n4~., data=vehiclesTrain, maxit=500, trace=T)



mailset <- read.csv('mailset.csv')
set.seed(nrow(mailset))
mailset <- mailset[sample(nrow(mailset)),]
split <- floor(nrow(mailset)/2)
mailTrain <- mailset[0:split,]
mailTest <- mailset[(split+1):nrow(mailset),]
library(nnet)
cylModel <- multinom(label~., data = mailTrain, maxit=100, trace=T)
library(caret)
mostImportantVariables <- varImp(cylModel)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))
preds1 <- predict(cylModel, type="probs", newdata=mailTest)
preds2 <- predict(cylModel, type="class", newdata=mailTest)
postResample(mailTest$label,preds2)

head(preds2)
head(preds1)
