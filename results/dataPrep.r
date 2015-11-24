library(lme4)
library(ggplot2)
library(plyr)
library(stringr)
#library(Cairo) # for unicode in saved plots

# s100 initial, attend
# s200 final, noattend
# s300 initial, noattend
# s400 final, attend

# exp2 refers to Experiment 1 in thesis
# exp1 refers to Experiment 2 in thesis

#EXPOSURE
expose <- read.delim('exp1_native_expose.txt')
expose$Experiment <- 'exp1'

expose$Attention <- 'attend'

expose[str_detect(expose$Subject,'^ns1-2'),]$Attention <- 'noattend'
expose[str_detect(expose$Subject,'^ns1-3'),]$Attention <- 'noattend'

expose$ExposureType <- 'initial'

expose[str_detect(expose$Subject,'^ns1-2'),]$ExposureType <- 'final'
expose[str_detect(expose$Subject,'^ns1-4'),]$ExposureType <- 'final'

t <- read.delim('exp2_native_expose.txt')
t$Experiment <- 'exp2'
t$CRESP <- NULL

t$Attention <- 'attend'

t[str_detect(t$Subject,'^ns2-2'),]$Attention <- 'noattend'
t[str_detect(t$Subject,'^ns2-3'),]$Attention <- 'noattend'

t$Attention <- factor(t$Attention)

t$ExposureType <- 'initial'

t[str_detect(t$Subject,'^ns2-2'),]$ExposureType <- 'final'
t[str_detect(t$Subject,'^ns2-4'),]$ExposureType <- 'final'

t$ExposureType <- factor(t$ExposureType)

expose <- rbind(expose,t)

t <- read.delim('exp4_native_expose.txt')

t$Experiment <- 'exp4'
t$Attention <- 'attend'

t[str_detect(t$Subject,'^ns4-2'),]$Attention <- 'noattend'
t[str_detect(t$Subject,'^ns4-3'),]$Attention <- 'noattend'

t$Attention <- factor(t$Attention)

t$ExposureType <- 'initial'

t[str_detect(t$Subject,'^ns4-2'),]$ExposureType <- 'final'
t[str_detect(t$Subject,'^ns4-4'),]$ExposureType <- 'final'

t$ExposureType <- factor(t$ExposureType)

expose <- rbind(expose,t)

expose$itemtype2 <- 'Filler'
expose[expose$itemtype%in%c('S-Final','S-Initial'),]$itemtype2 <- 'S'
expose[expose$itemtype%in%c('SH-Final','SH-Initial'),]$itemtype2 <- 'SH'
expose$itemtype2 <- factor(expose$itemtype2)

expose$Attention <- factor(expose$Attention, levels = c('noattend','attend'))

expose$ExposureType <- factor(expose$ExposureType, levels = c('initial','final'))

expose$Experiment <- factor(expose$Experiment)

expose <- subset(expose,RT > 200 & RT < 2500)
#expose[expose$RT < 200,]$ACC <- 0
#expose[expose$RT > 2500,]$ACC <- 0
expose <- na.omit(expose)

#expose <- subset(expose,!Subject %in% c('ns1-215','ns1-402', 'ns2-307', 'ns2-219'))

expose.word <- subset(expose,Lexicality=='Word')
expose.word$LogRT <- log(expose.word$RT)
expose.word$cLogRT <- scale(expose.word$LogRT)
expose.word$Word <- factor(expose.word$Word)

expose.word <- subset(expose.word, abs(cLogRT) < 2.5 )
expose.word$cTrial <- scale(expose.word$Trial)
expose.word$cLogRT <- scale(expose.word$LogRT)

target <- subset(expose,itemtype %in% c('S-Initial','S-Final'))
filler <- subset(expose, itemtype == 'Filler')

wresps <- ddply(target,~Subject,summarise,WordResp = sum(ACC)/20)

subj.tolerances <- ddply(target,~Subject*itemtype*Attention*Experiment,summarise,WordResp = sum(ACC)/20, MeanLogRT = mean(log(RT)))
subj.tolerances$aWordResp <- asin(subj.tolerances$WordResp)

ddply(subj.tolerances, ~Experiment*itemtype*Attention, summarise, MeanWordResp = mean(WordResp), SDWordResp = sd(WordResp), mean(MeanLogRT), sd(MeanLogRT))

summary(aov(aWordResp ~ Experiment*itemtype*Attention,data=subj.tolerances))

#CATEGORIZATION

categ <- read.delim('exp1_native_categ.txt')
categ$Experiment <- 'exp1'

categ$ExposureType <- 'initial'

categ[str_detect(categ$Subject,'^ns1-2'),]$ExposureType <- 'final'
categ[str_detect(categ$Subject,'^ns1-4'),]$ExposureType <- 'final'


categ$Attention <- 'attend'

categ[str_detect(categ$Subject,'^ns1-2'),]$Attention <- 'noattend'
categ[str_detect(categ$Subject,'^ns1-3'),]$Attention <- 'noattend'

t <- read.delim('exp2_native_categ.txt')
t$Experiment <- 'exp2'

t$ExposureType <- 'initial'

t[str_detect(t$Subject,'^ns2-2'),]$ExposureType <- 'final'
t[str_detect(t$Subject,'^ns2-4'),]$ExposureType <- 'final'

t$Attention <- 'attend'

t[str_detect(t$Subject,'^ns2-2'),]$Attention <- 'noattend'
t[str_detect(t$Subject,'^ns2-3'),]$Attention <- 'noattend'


categ <- rbind(categ,t)

t <- read.delim('exp4_native_categ.txt')
t$Experiment <- 'exp4'

t$ExposureType <- 'initial'

t[str_detect(t$Subject,'^ns4-2'),]$ExposureType <- 'final'
t[str_detect(t$Subject,'^ns4-4'),]$ExposureType <- 'final'

t$Attention <- 'attend'

t[str_detect(t$Subject,'^ns4-2'),]$Attention <- 'noattend'
t[str_detect(t$Subject,'^ns4-3'),]$Attention <- 'noattend'


categ <- rbind(categ,t)

categ$ExposureType <- factor(categ$ExposureType,levels = c('initial','final'))
categ$Attention <- factor(categ$Attention,levels=c('noattend','attend'))

categ$Experiment <- factor(categ$Experiment, levels = c('exp2','exp1','exp4'))

categ <- na.omit(categ)

categ <- subset(categ, RT > 200 & RT < 2500)

#fix for coding error
categ <- subset(categ,Trial < 169)
sub <- subset(categ,Subject %in% c('ns1-113','ns1-114','ns1-115','ns1-116','ns1-118'))
sub$RealAcc = 0
sub[sub$ACC == 0,]$RealAcc = 1

categ[categ$Subject %in% c('ns1-113','ns1-114','ns1-115','ns1-116','ns1-118'),]$ACC = sub$RealAcc

# Create consistent Item categories
t <- paste(categ$Label1,categ$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

categ$Item <- factor(t)

categ <- subset(categ,!Subject %in% c('ns1-215','ns1-402', 'ns2-214', 'ns2-219')) # Crazy crossovers
#categ <- subset(categ,!Subject %in% c('ns2-209', 'ns2-214-3')) #Weird data
categ <- subset(categ,!Subject %in% c('ns2-214-3')) #Non-native
#categ <- subset(categ,!Subject %in% c('ns1-105', 'ns1-117', 'ns1-319', 'ns1-323', 'ns1-401', 'ns2-124', 'ns2-206', 'ns2-215')) # Fitted probs near 1 or 0
categ <- merge(categ,wresps)

# By Item centering
categ$cStep <- 0
#sack-shack 3.642820
categ[categ$Item == 'sack-shack',]$cStep <- categ[categ$Item == 'sack-shack',]$Step - 3.642820
#sigh-shy 3.979852
categ[categ$Item == 'sigh-shy',]$cStep <- categ[categ$Item == 'sigh-shy',]$Step - 3.979852
#sin-shin 3.233012
categ[categ$Item == 'sin-shin',]$cStep <- categ[categ$Item == 'sin-shin',]$Step - 3.233012
#sock-shock 3.481329
categ[categ$Item == 'sock-shock',]$cStep <- categ[categ$Item == 'sock-shock',]$Step - 3.481329
categ$Step <- categ$Step - mean(1:6)

categ$ExposureType <- factor(categ$ExposureType, levels = c('initial','final'))
categ$Attention <- factor(categ$Attention, levels = c('noattend','attend'))

categ$LogRT <- log(categ$RT)
categ$cLogRT <- scale(categ$LogRT)

# CONTROL

cont <- read.delim('control_native_categ.txt')

cont <- na.omit(cont)

cont$ACC <- 0
cont[cont$RESP == cont$SResp,]$ACC <- 1

cont <- subset(cont, RT > 200 & RT < 2500)

t <- paste(cont$Label1,cont$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

cont$Item <- factor(t)
cont$Background <- 'Native'

nncont <- read.delim('control_nonnative_categ.txt')

nncont <- na.omit(nncont)

nncont$ACC <- 0
nncont[nncont$RESP == nncont$SResp,]$ACC <- 1

nncont <- subset(nncont, RT > 200 & RT < 2500)

t <- paste(nncont$Label1,nncont$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

nncont$Item <- factor(t)
nncont$Background <- 'Non-native'
cont <- rbind(cont, nncont)

cont$cStep <- 0
#sack-shack 3.642820
cont[cont$Item == 'sack-shack',]$cStep <- cont[cont$Item == 'sack-shack',]$Step - 3.642820
#sigh-shy 3.979852
cont[cont$Item == 'sigh-shy',]$cStep <- cont[cont$Item == 'sigh-shy',]$Step - 3.979852
#sin-shin 3.233012
cont[cont$Item == 'sin-shin',]$cStep <- cont[cont$Item == 'sin-shin',]$Step - 3.233012
#sock-shock 3.481329
cont[cont$Item == 'sock-shock',]$cStep <- cont[cont$Item == 'sock-shock',]$Step - 3.481329

cont$Step <- cont$Step - mean(1:6)
cont$Background <- factor(cont$Background)
contrasts(cont$Background) <- contr.sum
contrasts(cont$Background) <- contrasts(cont$Background) / 2

cont$LogRT <- log(cont$RT)
cont$cLogRT <- scale(cont$LogRT)
#categ <- rbind(categ, cont)

# EXP 3 EXPOSURE

expose3 <- read.delim('exp3_native_expose.txt')
expose3$Native <- 'yes'
t <- read.delim('exp3_nonnative_expose.txt')
t$Native <- 'no'

#expose3 <- rbind(expose3,t)
expose3$Native <- factor(expose3$Native)
expose3 <- na.omit(expose3)
expose3$LogRT <- log(expose3$RT)
#expose3$cLogRT <- expose3$LogRT - mean(expose3$LogRT)
expose3$cLogRT <- scale(expose3$LogRT)
expose3$cTrial <- scale(expose3$Trial)

expose3$Attention <- 'attend'

expose3[str_detect(expose3$Subject,'^ns3-2'),]$Attention <- 'noattend'
expose3[str_detect(expose3$Subject,'^ns3-3'),]$Attention <- 'noattend'

expose3$Attention <- factor(expose3$Attention, levels = c('noattend', 'attend'))

expose3$ExposureType <- 'predictive'

expose3[str_detect(expose3$Subject,'^ns3-2'),]$ExposureType <- 'unpredictive'
expose3[str_detect(expose3$Subject,'^ns3-4'),]$ExposureType <- 'unpredictive'

#expose3[str_detect(expose3$Subject,'^nns3-2'),]$ExposureType <- 'unpredictive'
#expose3[str_detect(expose3$Subject,'^nns3-4'),]$ExposureType <- 'unpredictive'

expose3$ExposureType <- factor(expose3$ExposureType, levels = c('unpredictive','predictive'))

expose3$Predictability <- factor(expose3$Predictability, levels = c('Unpredictive', 'Predictive'))

subj.tolerances3 <- ddply(subset(expose3,Type == 'S-final'),~Predictability*Attention*Subject, summarise, MeanLogRt = mean(LogRT))

# EXP 3 Categorization

categ3 <- read.delim('exp3_native_categ.txt')
categ3$Native <- 'yes'

t <- read.delim('exp3_nonnative_categ.txt')
t$Native <- 'no'
#categ3 <- rbind(categ3,t)
categ3$Native <- factor(categ3$Native)
categ3 <- na.omit(categ3)
categ3$ACC = 0
categ3[categ3$RESP == categ3$SResp,]$ACC <- 1
categ3$Experiment <- 'exp3'

categ3$ExposureType <- 'predictive'

categ3[str_detect(categ3$Subject,'^ns3-2'),]$ExposureType <- 'unpredictive'
categ3[str_detect(categ3$Subject,'^ns3-4'),]$ExposureType <- 'unpredictive'

#categ3[str_detect(categ3$Subject,'^nns3-2'),]$ExposureType <- 'unpredictive'
#categ3[str_detect(categ3$Subject,'^nns3-4'),]$ExposureType <- 'unpredictive'

categ3$ExposureType <- factor(categ3$ExposureType, levels = c('unpredictive','predictive'))

categ3$Attention <- 'attend'

categ3[str_detect(categ3$Subject,'^ns3-2'),]$Attention <- 'noattend'
categ3[str_detect(categ3$Subject,'^ns3-3'),]$Attention <- 'noattend'

categ3$Attention <- factor(categ3$Attention, levels = c('noattend','attend'))

t <- paste(categ3$Label1,categ3$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

categ3$Item <- factor(t)

categ3 <- subset(categ3, RT > 200 & RT < 2500)
categ3$cStep <- 0
#sack-shack 3.642820
categ3[categ3$Item == 'sack-shack',]$cStep <- categ3[categ3$Item == 'sack-shack',]$Step - 3.642820
#sigh-shy 3.979852
categ3[categ3$Item == 'sigh-shy',]$cStep <- categ3[categ3$Item == 'sigh-shy',]$Step - 3.979852
#sin-shin 3.233012
categ3[categ3$Item == 'sin-shin',]$cStep <- categ3[categ3$Item == 'sin-shin',]$Step - 3.233012
#sock-shock 3.481329
categ3[categ3$Item == 'sock-shock',]$cStep <- categ3[categ3$Item == 'sock-shock',]$Step - 3.481329

categ3$Step <- categ3$Step - mean(1:6)

categ3$LogRT <- log(categ3$RT)
categ3$cLogRT <- scale(categ3$LogRT)

categ3$ISI = 0

categ3.2000 <- read.delim('exp3_2000_nativeenglish.txt')

categ3.2000 <- na.omit(categ3.2000)
categ3.2000$ACC = 0
categ3.2000[categ3.2000$RESP == categ3.2000$SResp,]$ACC <- 1
categ3.2000$Experiment <- 'exp3'

categ3.2000$ExposureType <- 'predictive'

categ3.2000[str_detect(categ3.2000$Subject,'^2'),]$ExposureType <- 'unpredictive'

#categ3.2000[str_detect(categ3.2000$Subject,'^nns3-2'),]$ExposureType <- 'unpredictive'
#categ3.2000[str_detect(categ3.2000$Subject,'^nns3-4'),]$ExposureType <- 'unpredictive'

categ3.2000$ExposureType <- factor(categ3.2000$ExposureType, levels = c('unpredictive','predictive'))

categ3.2000$Attention <- 'attend'

categ3.2000[str_detect(categ3.2000$Subject,'^2'),]$Attention <- 'noattend'

categ3.2000$Attention <- factor(categ3.2000$Attention, levels = c('noattend','attend'))

t <- paste(categ3.2000$Label1,categ3.2000$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

categ3.2000$Item <- factor(t)

categ3.2000 <- subset(categ3.2000, RT > 200 & RT < 2500)
categ3.2000$cStep <- 0
#sack-shack 3.642820
categ3.2000[categ3.2000$Item == 'sack-shack',]$cStep <- categ3.2000[categ3.2000$Item == 'sack-shack',]$Step - 3.642820
#sigh-shy 3.979852
categ3.2000[categ3.2000$Item == 'sigh-shy',]$cStep <- categ3.2000[categ3.2000$Item == 'sigh-shy',]$Step - 3.979852
#sin-shin 3.233012
categ3.2000[categ3.2000$Item == 'sin-shin',]$cStep <- categ3.2000[categ3.2000$Item == 'sin-shin',]$Step - 3.233012
#sock-shock 3.481329
categ3.2000[categ3.2000$Item == 'sock-shock',]$cStep <- categ3.2000[categ3.2000$Item == 'sock-shock',]$Step - 3.481329

categ3.2000$Step <- categ3.2000$Step - mean(1:6)

categ3.2000$LogRT <- log(categ3.2000$RT)
categ3.2000$cLogRT <- scale(categ3.2000$LogRT)

categ3.2000$ISI = 2000
categ3.2000$Subject = factor(categ3.2000$Subject)

categ3.ISI <- rbind(subset(categ3, Attention == 'noattend' & ExposureType == 'unpredictive')[,c('Subject','Step', 'ACC','ExposureType','Attention','Item','cStep','cLogRT','ISI')], categ3.2000[,c('Subject','Step', 'ACC','ExposureType','Attention','Item','cStep','cLogRT','ISI')])

categ3.ISI$ISI <- factor(categ.ISI$ISI)
# Experiment 1 and 3

categ23 <- subset(categ,Experiment == 'exp2' & ExposureType == 'final')

categ23$ExposureType <- 'isolation'

categ23 <- rbind(categ23[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Attention','Item', 'cStep')],categ3[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Attention','Item', 'cStep')])

categ23$ExposureType <- factor(categ23$ExposureType, levels = c('isolation','unpredictive','predictive'))

subj.info23 <- unique(categ23[,c('Subject','ExposureType','Attention')])

all.subj.info <- unique(categ[,c('Subject','Experiment','ExposureType','Attention')])
all.subj.info <- rbind(all.subj.info, unique(categ3[,c('Subject','Experiment','ExposureType','Attention')]))
all.subj.info <- rbind(all.subj.info, unique(categ5[,c('Subject','Experiment','ExposureType','Attention')]))

categ25 <- subset(categ,Experiment == 'exp2' & ExposureType == 'final' & Attention == 'noattend')

categ25$ExposureType <- 'isolation'

categ25 <- rbind(categ25[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Attention','Item', 'cStep')],categ5[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Attention','Item', 'cStep')])

categ25$ExposureType <- factor(categ25$ExposureType, levels = c('isolation','unpredictive','predictive'))

stuttgart.data <- subset(categ,Experiment == 'exp2' & ExposureType == 'final' & Attention == 'noattend')

stuttgart.data$ExposureType <- 'isolation'

cont$ExposureType <- 'control'
cont$Experiment <- 'control'

stuttgart.data <- rbind(cont[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Item', 'cStep')], stuttgart.data[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Item', 'cStep')],categ3[categ3$Attention=='noattend',c('Subject','ACC','RT','Step','Experiment','ExposureType','Item', 'cStep')],categ5[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Item', 'cStep')])

stuttgart.data$ExposureType <- factor(stuttgart.data$ExposureType, levels = c('control','isolation','unpredictive','predictive'))


contrasts(expose.word$Attention) <- contr.sum
contrasts(expose.word$Attention) <- contrasts(expose.word$Attention) / 2

contrasts(expose.word$ExposureType) <- contr.sum
contrasts(expose.word$ExposureType) <- contrasts(expose.word$ExposureType) / 2

contrasts(expose3$Attention) <- contr.sum
contrasts(expose3$Attention) <- contrasts(expose3$Attention) / 2

contrasts(expose3$ExposureType) <- contr.sum
contrasts(expose3$ExposureType) <- contrasts(expose3$ExposureType) / 2

contrasts(expose3$Predictability) <- contr.sum
contrasts(expose3$Predictability) <- contrasts(expose3$Predictability) / 2


contrasts(categ$Attention) <- contr.sum
contrasts(categ$Attention) <- contrasts(categ$Attention) / 2

contrasts(categ$ExposureType) <- contr.sum
contrasts(categ$ExposureType) <- contrasts(categ$ExposureType) / 2

categ$Experiment <- factor(categ$Experiment)

contrasts(categ$Experiment) <- contr.sum
contrasts(categ$Experiment) <- contrasts(categ$Experiment) / 2

contrasts(categ3$Attention) <- contr.sum
contrasts(categ3$Attention) <- contrasts(categ3$Attention) / 2

contrasts(categ3$ExposureType) <- contr.sum
contrasts(categ3$ExposureType) <- contrasts(categ3$ExposureType) / 2

contrasts(categ23$Attention) <- contr.sum
contrasts(categ23$Attention) <- contrasts(categ23$Attention) / 2

#contrasts(categ23$ExposureType) <- contr.sum
#contrasts(categ23$ExposureType) <- contrasts(categ23$ExposureType) / 2

categ5 <- read.delim('exp5_native_categ.txt')

categ5 <- na.omit(categ5)

categ5$ACC <- as.integer(categ5$RESP == categ5$SResp)

categ5$Experiment <- 'exp5'

categ5$ExposureType <- 'predictive'

categ5[str_detect(categ5$Subject,'^ns5-2'),]$ExposureType <- 'unpredictive'

categ5$ExposureType <- factor(categ5$ExposureType, levels = c('unpredictive','predictive'))

categ5$Attention <- 'noattend'

t <- paste(categ5$Label1,categ5$Label2,sep='-')

t[t=='shack-sack'] = 'sack-shack'
t[t=='shy-sigh'] = 'sigh-shy'
t[t=='shin-sin'] = 'sin-shin'
t[t=='shock-sock'] = 'sock-shock'

categ5$Item <- factor(t)

categ5 <- subset(categ5, RT > 200 & RT < 2500)
categ5$cStep <- 0
#sack-shack 3.642820
categ5[categ5$Item == 'sack-shack',]$cStep <- categ5[categ5$Item == 'sack-shack',]$Step - 3.642820
#sigh-shy 3.979852
categ5[categ5$Item == 'sigh-shy',]$cStep <- categ5[categ5$Item == 'sigh-shy',]$Step - 3.979852
#sin-shin 3.233012
categ5[categ5$Item == 'sin-shin',]$cStep <- categ5[categ5$Item == 'sin-shin',]$Step - 3.233012
#sock-shock 3.481329
categ5[categ5$Item == 'sock-shock',]$cStep <- categ5[categ5$Item == 'sock-shock',]$Step - 3.481329

categ5$Step <- categ5$Step - mean(1:6)

categ5$LogRT <- log(categ5$RT)
categ5$cLogRT <- scale(categ5$LogRT)


categ35 <- rbind(categ3[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Attention','Item', 'cStep')],categ5[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Attention','Item', 'cStep')])

categ35 <- subset(categ35, Attention == 'noattend')


categ235 <- rbind(categ23[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Attention','Item', 'cStep')],categ5[,c('Subject','ACC','RT','Step','Experiment','ExposureType','Attention','Item', 'cStep')])

getCrossOver <- function(data){
  data$p <- -1*data[,'(Intercept)']/data[,'cStep']
  data$pRound <- round(data$p)
  
  data <- data.frame(Subject = row.names(data),Xover = data$p)
  return(data)
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}