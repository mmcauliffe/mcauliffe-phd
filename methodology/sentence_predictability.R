#Sentence predictability analysis

library(ggplot2)
library(plyr)

senpred <- read.delim('senpred_parsed.txt')
senpred$RespProp <- as.numeric(as.character(senpred$RespProp))
summary(senpred)

t.test(senpred[senpred$Predictive=='no',]$RespProp,senpred[senpred$Predictive=='yes',]$RespProp)

ddply(senpred,~Predictive,summarise,mean(RespProp))

par(mfrow=c(1,2))
ggplot(senpred,aes(x=RespProp,colour=Predictive))+geom_density()
ggplot(senpred,aes(x=PresentProp,colour=Predictive))+geom_density()

sensum <- ddply(senpred,~Predictive*Word,summarise,MeanRespProp = mean(RespProp),MeanPresentProp=mean(PresentProp))
questionable <- subset(sensum,Predictive=='yes'& MeanPresentProp < 0.5)
