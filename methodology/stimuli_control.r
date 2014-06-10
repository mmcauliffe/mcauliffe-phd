library(plyr)
library(lme4)

iphod <- read.delim('C:/Users/michael/Documents/Data/Iphod/IPhOD2_Words.txt')
iphod <- unique(iphod[,c('Word','NSyll','SFreq')])

wordlist <- read.delim('lexdeclist.txt')

words <- wordlist[wordlist$Lexicality=='Word',]

words <- merge(words,iphod)
words$SFreq <- log(words$SFreq+1)

#s.final <- read.delim('s_final.txt')
#s.final$Position <- 'Final'
#s.final$Segment <- 'S'
#sh.final <- read.delim('sh_final.txt')
#sh.final$Position <- 'Final'
#sh.final$Segment <- 'SH'
#s.initial <- read.delim('s_initial.txt')
#s.initial$Position <- 'Initial'
#s.initial$Segment <- 'S'
#sh.initial <- read.delim('sh_initial.txt')
#sh.initial$Position <- 'Initial'
#sh.initial$Segment <- 'SH'
#fillers <- read.delim('fillers.txt')
#fillers$Position <- 'Filler'
#fillers$Segment <- 'Filler'


#t <- rbind(merge(s.final,iphod),merge(sh.final,iphod),merge(s.initial,iphod),merge(sh.initial,iphod))
#t$SFreq <- log(t$SFreq+1)

ddply(words,~itemtype, summarise, mean.Syll=mean(NSyll),sd.Syll=sd(NSyll),mean.Freq = mean(SFreq),sd.Freq=sd(SFreq))

summary(aov(NSyll~itemtype,data=words))
summary(aov(SFreq~itemtype,data=words))

#t <- lmer(SFreq~itemtype+(1|Word),data=words)
