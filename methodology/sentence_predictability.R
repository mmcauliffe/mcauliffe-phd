#Sentence predictability analysis

library(ggplot2)
library(plyr)

senpred <- read.delim('C:\\Users\\michael\\Dropbox\\Michael_Dissertation\\Pretest\\senpred_parsed.txt')
summary(senpred)
senpred$RespProp <- as.numeric(as.character(senpred$RespProp))

t.test(senpred[senpred$Predictive=='no',]$RespProp,senpred[senpred$Predictive=='yes',]$RespProp)

ddply(senpred,~Predictive,summarise,mean(RespProp))

ggplot(senpred,aes(x=RespProp,colour=Predictive))+geom_density()
