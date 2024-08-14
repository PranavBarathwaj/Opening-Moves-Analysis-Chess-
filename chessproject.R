setwd("C:/Users/Prana/OneDrive/Documents/Appln data mining SP23/Project")
chess<-read.csv("chess.csv",header=TRUE,sep=",")

#to filter out incremented and unrated games. This was done before hand. The dataset given in the file is the new one.
for(i in 1:nrow(chess)){
  if(chess$increment[i]!=0 || chess$rated[i]=="FALSE"){
    inc[j]<-i
    j<-j+1
  }
}
#remove games that result in draw. This was done before hand. The dataset given in the file is the new one.
inc<-c()
j<-1
for(i in 1:nrow(chess)){
  if(chess$winner[i]!="Draw"){
    inc[j]<-i
    j<-j+1
  }
}

chess<-chess[-inc,]
library(partykit)

#creating traning and test dataset
set.seed(1234)
ind <- sample(2, nrow(chess), replace=TRUE, prob=c(0.7, 0.3))
chess.train <- chess[ind==1,]
chess.test <- chess[ind==2,]

#converting certain attributes to numeric data types
victory<-as.numeric(factor(chess$victory_status, levels = unique(chess$victory_status)))
code <- as.numeric(factor(chess$opening_code, levels = unique(chess$opening_code))) 

#creating relationships for different decision trees
myFormula <-chess$winner~ chess$white_rating + chess$black_rating #for winner as label (gives expected tree)
myFormula <-code~ chess$white_rating + chess$black_rating #for code as label (gives expected tree)
myFormula <-chess$winner~ code #for winner as label (gives expected tree)
myFormula <-victory~ chess$white_rating + chess$black_rating #isn't giving expected tree
myFormula <-victory~ code+ chess$winner
myFormula <-code~ chess$winner +victory #isn't giving expected tree

#ctree decision tree
chess_ctree <- ctree(myFormula, data=chess.train)
plot(ctree)

#predicting testdata with opening_code
table(predict(chess_ctree), code) #check the prediction with respect to opening code
testPred <- predict(chess_ctree, newdata = chess.test) #predict on test data with respect to opening code
plot(testPred, code) #plotting predicted with expected code

#rpart decision tree (applies every myFormula relationship written above)
chess_rpart <- rpart(myFormula, data = chess.train, control = rpart.control(minsplit = 10))
rpart.plot::rpart.plot(chess_rpart)

#pruning (applies rpart decision tree)
opt <- which.min(chess_rpart$cptable[,"xerror"])
cp <- chess_rpart$cptable[opt, "CP"]
chess_prune <- prune(chess_rpart, cp = cp)
rpart.plot::rpart.plot(chess_prune)

#logistic regression formula (applies every myFormula relationship written above)
chess.glm<-glm(myFormula, family = gaussian("log"), data = chess.train)
summary(chess.glm)

#linear regression summaries
summary(lm(code~chess$white_rating+chess$black_rating))
summary(lm(chess$winner~chess$white_rating+chess$black_rating))
summary(lm(chess$winner~code))

#plots
plot(chess$black_rating,code, xlab="black rating", ylab="opening code") 
plot(chess$white_rating,code, xlab="white rating", ylab="opening code")
plot(chess$winner,code, xlab="winner", ylab="opening code")

#clustering
hc <- hclust(dist(chess))
plot(hc)
rect.hclust(hc, k=2)
groups <- cutree(hc, k=3)