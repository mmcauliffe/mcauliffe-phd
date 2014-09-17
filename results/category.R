library(lme4)
library(ggplot2)
library(plyr)
library(stringr)

#s100 initial, attend
#s200 final, noattend
#s300 initial, noattend
#s400 final, attend

expose <- read.delim('exposure.txt')
categ <- read.delim('categorization.txt')
categ <- na.omit(categ)

#fix for coding error
categ <- subset(categ,Trial < 169)
sub <- subset(categ,Subject %in% c('s113','s114','s115','s116','s117','s118'))
sub$RealAcc = 0
sub[sub$ACC == 0,]$RealAcc = 1

categ[categ$Subject %in% c('s113','s114','s115','s116','s117','s118'),]$ACC = sub$RealAcc

t <- paste(categ$Label1,categ$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

categ$Item <- factor(t)

categ$ExposureType <- 'initial'

categ[str_detect(categ$Subject,'^s2'),]$ExposureType <- 'final'
categ[str_detect(categ$Subject,'^s4'),]$ExposureType <- 'final'

categ$ExposureType <- factor(categ$ExposureType)

categ$Attention <- 'attend'

categ[str_detect(categ$Subject,'^s2'),]$Attention <- 'noattend'
categ[str_detect(categ$Subject,'^s3'),]$Attention <- 'noattend'

categ$Attention <- factor(categ$Attention)

categ <- subset(categ,!Subject %in% c('s215'))

ddply(categ,~ExposureType*Attention,summarise,mean(ACC))

cat.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=categ)
summary(cat.mod)

getCrossOver <- function(data){
  data$p <- -1*data[,'(Intercept)']/data[,'Step']
  data$pRound <- round(data$p)
  
  data <- data.frame(Subject = row.names(data),Xover = data$p)
  return(data)
}
t <- getCrossOver(coef(cat.mod)$Subject)

t2 <- merge(t,subj.tolerances)

t.test(t[str_detect(t$Subject,'^s2'),]$Xover,t[str_detect(t$Subject,'^s3'),]$Xover)

summary(aov(Xover ~ WordResp,data=t2))
summary(aov(Xover ~ Attention,data=t2))
cor.test(t2$Xover, t2$WordResp)

ggplot(categ, aes(x=Step, y=ACC,group=1)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~Subject) + labs(title='Categorization words', y='Proportion <S> responses',x='Step number')

ggplot(t2,aes(x=WordResp,y=Xover,colour=Attention,shape=itemtype)) + geom_point() + geom_smooth(method='lm')

cat.mod.full <- glmer(ACC ~ Step*ExposureType + (1+Step|Subject) + (1+Step|Item), family='binomial',data=categ)
