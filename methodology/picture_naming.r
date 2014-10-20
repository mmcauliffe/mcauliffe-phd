#Picture naming analysis

library(ggplot2)
library(plyr)

pictname <- read.delim('picturenaming_parsed.txt')
pictname$RespProp <- as.numeric(as.character(pictname$RespProp))
summary(pictname)

par(mfrow=c(1,2))
ggplot(pictname,aes(y=RespProp,x=Word))+geom_bar(stat='identity') +coord_flip()
ggplot(senpred,aes(x=PresentProp,colour=Predictive))+geom_density()

questionable.picts <- subset(pictname,PresentProp < 0.4)
#To replace
#Toothpick
#Ukulele with new word
#Earmuff with new word
#Earplug with new word
