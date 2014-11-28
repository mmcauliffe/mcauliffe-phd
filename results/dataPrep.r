library(lme4)
library(ggplot2)
library(plyr)
library(stringr)

#s100 initial, attend
#s200 final, noattend
#s300 initial, noattend
#s400 final, attend

#EXPOSURE
expose <- read.delim('exposure.txt')
expose$Experiment <- 'exp1'

expose$Attention <- 'attend'

expose[str_detect(expose$Subject,'^s2'),]$Attention <- 'noattend'
expose[str_detect(expose$Subject,'^s3'),]$Attention <- 'noattend'

expose$Attention <- factor(expose$Attention)

t <- read.delim('exp2_native_expose.txt')
t$Experiment <- 'exp2'
t$CRESP <- NULL

t$Attention <- 'attend'

t[str_detect(t$Subject,'^ns2-2'),]$Attention <- 'noattend'
t[str_detect(t$Subject,'^ns2-3'),]$Attention <- 'noattend'

t$Attention <- factor(t$Attention)
expose <- rbind(expose,t)
expose$Experiment <- factor(expose$Experiment)

expose <- subset(expose,RT > 200 & RT < 2500)
#expose[expose$RT < 200,]$ACC <- 0
#expose[expose$RT > 2500,]$ACC <- 0
expose <- na.omit(expose)

expose.word <- subset(expose,Lexicality=='Word')
expose.word$Word <- factor(expose.word$Word)

target <- na.omit(subset(expose,itemtype %in% c('S-Initial','S-Final')))

subj.tolerances <- ddply(target,~Subject*itemtype*Attention*Experiment,summarise,WordResp = mean(ACC))

summary(aov(WordResp ~ itemtype*Attention*Experiment,data=subj.tolerances))

#CATEGORIZATION


categ <- read.delim('categorization.txt')
categ$Experiment <- 'exp1'

categ$ExposureType <- 'initial'

categ[str_detect(categ$Subject,'^s2'),]$ExposureType <- 'final'
categ[str_detect(categ$Subject,'^s4'),]$ExposureType <- 'final'

categ$ExposureType <- factor(categ$ExposureType)

categ$Attention <- 'attend'

categ[str_detect(categ$Subject,'^s2'),]$Attention <- 'noattend'
categ[str_detect(categ$Subject,'^s3'),]$Attention <- 'noattend'

categ$Attention <- factor(categ$Attention)


t <- read.delim('exp2_native_categ.txt')
t$Experiment <- 'exp2'

t$ExposureType <- 'initial'

t[str_detect(t$Subject,'^ns2-2'),]$ExposureType <- 'final'
t[str_detect(t$Subject,'^ns2-4'),]$ExposureType <- 'final'

t$ExposureType <- factor(t$ExposureType)

t$Attention <- 'attend'

t[str_detect(t$Subject,'^ns2-2'),]$Attention <- 'noattend'
t[str_detect(t$Subject,'^ns2-3'),]$Attention <- 'noattend'

t$Attention <- factor(t$Attention)

categ <- rbind(categ,t)
categ$Experiment <- factor(categ$Experiment)

categ <- na.omit(categ)

categ <- subset(categ, RT > 200 & RT < 2500)

#fix for coding error
categ <- subset(categ,Trial < 169)
sub <- subset(categ,Subject %in% c('s113','s114','s115','s118'))
sub$RealAcc = 0
sub[sub$ACC == 0,]$RealAcc = 1

categ[categ$Subject %in% c('s113','s114','s115','s118'),]$ACC = sub$RealAcc

t <- paste(categ$Label1,categ$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

categ$Item <- factor(t)

categ <- subset(categ,!Subject %in% c('s215','s402'))

categ$Step <- categ$Step - mean(1:6)