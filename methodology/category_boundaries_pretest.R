
library(lme4.0)
library(ggplot2)
library(plyr)

cat <- read.delim('sshcat_results.txt')

cat$StepNum <- as.numeric(gsub("[^0-9]","",cat$step))

cat <- na.omit(cat)

cat <- subset(cat,RT > 200)
cat  <- subset(cat,RT < 2500)

summary(cat)


exp <- subset(cat,which=='one')
exp$sword <- factor(exp$sword)
test <- subset(cat,which=='both')
test$sword <- factor(test$sword)


exp.sum <- ddply(exp, ~ sword * factor(StepNum),summarise,MeanResp = mean(ACC))
names(exp.sum) <- c('Word','StepNum','MeanResp')
test.sum <- ddply(test, ~ sword * factor(StepNum),summarise,MeanResp = mean(ACC))
names(test.sum) <- c('Word','StepNum','MeanResp')

ggplot(exp, aes(x=StepNum, y=ACC,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Exposure words', y='Proportion <S> responses',x='Step number')+ geom_vline(xintercept = 6) + geom_hline(yintercept=0.5)
ggplot(test, aes(x=StepNum, y=ACC,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Categorization words', y='Proportion <S> responses',x='Step number')+ geom_vline(xintercept = 6)+ geom_vline(xintercept = 6) + geom_hline(yintercept=0.5)

findStep <- function(summary,thresh){
  words <- levels(summary$Word)
  
  out <- data.frame(Word=character(),Step=numeric())
  for(i in 1:length(words)){
    w <- words[i]
    sub <- subset(summary,Word==w)
    for(j in 1:11){
      if (sub[sub$StepNum==j,]$MeanResp < thresh){
        out <- rbind(out,data.frame(Word=w,Step=j))
        break
      }
    }
  }
  return(out)
}

expout <- findStep(exp.sum,0.4)
expout <- rbind(expout,data.frame(Word='seedling',Step=7))

#sack 5.5 (3,4,5,6,7,8)
#sigh 5.5 (3,4,5,6,7,8)
#sin 5 (3,4,5,6,7,8)
#sock 6.5 (4,5,6,7,8,9)