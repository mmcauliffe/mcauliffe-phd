
library(lme4)
library(ggplot2)
library(plyr)

#cat <- read.delim('C:\\Users\\michael\\Dropbox\\Michael_Dissertation\\Pretest\\sshcat_results.txt')
cat <- read.delim('sshcat_results.txt')

cat$StepNum <- as.numeric(gsub("[^0-9]","",cat$step))
cat$Subject <- factor(cat$Subject)

cat <- na.omit(cat)

cat <- subset(cat,RT > 200)
cat  <- subset(cat,RT < 2500)

threshold = 0.4

summary(cat)


exp <- subset(cat,which=='one')
exp$sword <- factor(exp$sword)
exp$Type <- 'Initial'
exp[exp$sword %in% c('carousel','castle','concert','croissant','currency','cursor','curtsy','dancer','dinosaur','faucet','fossil','galaxy','medicine','missile','monsoon','pencil','pharmacy','tassel','taxi','whistle'),]$Type <- 'Final'
exp$Type <- factor(exp$Type)
test <- subset(cat,which=='both')
test$sword <- factor(test$sword)


exp.sum <- ddply(exp, ~ Type*sword * factor(StepNum),summarise,MeanResp = mean(ACC))
names(exp.sum) <- c('Type','Word','StepNum','MeanResp')
test.sum <- ddply(test, ~ sword * factor(StepNum),summarise,MeanResp = mean(ACC))
names(test.sum) <- c('Word','StepNum','MeanResp')

findStep <- function(summary,thresh){
  words <- levels(summary$Word)
  
  out <- data.frame(Word=character(),Step=numeric())
  for(i in 1:length(words)){
    w <- words[i]
    sub <- subset(summary,Word==w)
    for(j in 1:11){
      if (sub[sub$StepNum==j,]$MeanResp < thresh){
        out <- rbind(out,sub[sub$StepNum==j,])
        break
      }
    }
  }
  return(out)
}
exp.sum$Dprime <- 0.0
for (i in 1:nrow(exp.sum)){
  if (exp.sum[i,]$MeanResp == 1){
    exp.sum[i,]$MeanResp = 0.98
  }
  else if (exp.sum[i,]$MeanResp == 0){
    exp.sum[i,]$MeanResp = 0.02
  }
  exp.sum[i,]$Dprime <- dprime.mAFC(exp.sum[i,]$MeanResp,2)
}


expout1 <- findStep(exp.sum,0.4)
expout1 <- rbind(expout,data.frame(Type='Initial',Word='seedling',Step=7)) #seedling 7 for exp1, 6 for exp2

expout2 <- findStep(exp.sum,0.6)
expout2 <- rbind(expout,data.frame(Type='Initial',Word='seedling',Step=6)) #seedling 7 for exp1, 6 for exp2

expout4 <- findStep(exp.sum,0.8) #Seedling step 5

exp <- merge(exp,expout,by.x=c('sword'),by.y=c('Word'))

testout <- findStep(test.sum,0.5)

#sack 5.5 (3,4,5,6,7,8)
#sigh 5.5 (3,4,5,6,7,8)
#sin 5 (3,4,5,6,7,8)
#sock 6.5 (4,5,6,7,8,9)

ggplot(exp, aes(x=StepNum, y=ACC)) +geom_point() +geom_smooth(method="glm", family="binomial", size=1)  + labs(title='Exposure words', y='Proportion <S> responses',x='Step number')+facet_wrap(~sword)+ geom_vline(xintercept = 6) + geom_hline(yintercept=0.5)

ggplot(subset(exp,Type=='Initial'), aes(x=StepNum, y=ACC)) +geom_smooth(method="glm", family="binomial", size=1)  + labs(title='S-Initial exposure words', y='Proportion <S> responses',x='Step number')+facet_wrap(~sword)+ geom_vline(data=subset(expout2, Type=='Initial'),aes(xintercept = as.numeric(as.character(StepNum)))) + geom_hline(yintercept=0.5) + geom_hline(yintercept=0.3,linetype=2) +geom_vline(data=subset(expout1, Type=='Initial'),aes(xintercept = as.numeric(as.character(StepNum))),linetype=2)

cat.mod <- glmer(ACC~ StepNum + (1|Subject) + (1|sword), data = test, family='binomial')
xovers <- getCrossOver(coef(cat.mod)$sword)

ggplot(subset(exp,Type=='Final'), aes(x=StepNum, y=ACC)) +geom_smooth(method="glm", family="binomial", size=1)  + labs(title='S-Final exposure words', y='Proportion <S> responses',x='Step number')+facet_wrap(~sword)+ geom_vline(aes(xintercept = 6)) + geom_hline(yintercept=0.5)

ggplot(test, aes(x=StepNum, y=ACC,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~sword) + labs(title='Categorization words', y='Proportion <S> responses',x='Step number')+ geom_vline(xintercept = 6)+ geom_vline(xintercept=0.5) + geom_hline(yintercept=0.5)

#For 530 presentation
ggplot(subset(exp,sword=='silver'), aes(x=StepNum, y=ACC)) +geom_smooth(method="glm", family="binomial", size=1)  + labs(title='Silver to Shilver', y='Proportion <s> responses',x='Step number')+ geom_vline(aes(xintercept = 6)) + geom_hline(yintercept=0.5) + scale_x_continuous(breaks=1:11)

#For dissertation
ggplot(subset(exp.sum,Type=='Initial'), aes(x=as.numeric(as.character(StepNum)), y=MeanResp)) +geom_point() +geom_smooth(data =subset(exp,Type=='Initial'), aes(x=StepNum,y=ACC), method="glm", family="binomial", size=1)  + labs(y='Proportion /s/ response',x='Step number')+facet_wrap(~sword)+ geom_vline(data=subset(expout2, Type=='Initial'),aes(xintercept = as.numeric(as.character(StepNum)))) + geom_hline(yintercept=0.5) + geom_hline(yintercept=0.3,linetype=2) +geom_vline(data=subset(expout1, Type=='Initial'),aes(xintercept = as.numeric(as.character(StepNum))),linetype=2) +theme_bw()

ggsave('../thesis/graphs/sinitialpretest.pdf',width=170,height=110,units='mm',dpi=600)

ggplot(subset(exp.sum,Type=='Final'), aes(x=as.numeric(as.character(StepNum)), y=MeanResp)) +geom_point() +geom_smooth(data =subset(exp,Type=='Final'), aes(x=StepNum,y=ACC), method="glm", family="binomial", size=1)  + labs(y='Proportion /s/ response',x='Step number')+facet_wrap(~sword)+ geom_vline(data=subset(expout2, Type=='Final'),aes(xintercept = as.numeric(as.character(StepNum)))) + geom_hline(yintercept=0.5) + geom_hline(yintercept=0.3,linetype=2) +geom_vline(data=subset(expout1, Type=='Final'),aes(xintercept = as.numeric(as.character(StepNum))),linetype=2) +theme_bw()

ggsave('../thesis/graphs/sfinalpretest.pdf',width=170,height=110,units='mm',dpi=600)

ddply(expout1,~Type,summarise,mean(as.numeric(as.character(StepNum))),mean(MeanResp),sd(as.numeric(as.character(StepNum))),sd(MeanResp))

ddply(expout2,~Type,summarise,mean(as.numeric(as.character(StepNum))),mean(MeanResp),sd(as.numeric(as.character(StepNum))),sd(MeanResp))

ddply(expout4,~Type,summarise,mean(as.numeric(as.character(StepNum))),mean(MeanResp),sd(as.numeric(as.character(StepNum))),sd(MeanResp))
