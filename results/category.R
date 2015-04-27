### CONTROL ###

cont.mod <- glmer(ACC ~ Step*Background + (1+Step|Subject) + (1+Step|Item), family='binomial',data=cont)
summary(cont.mod)


### END CONTROL ###

cat.mod <- glmer(ACC ~ cStep + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=categ)
xovers <- getCrossOver(coef(cat.mod)$Subject)
ddply(subset(xovers,Xover > 0), ~Experiment*Attention*itemtype,nrow)
ddply(subset(xovers,Xover <= 0), ~Experiment*Attention*itemtype,nrow)

cat.mod3 <- glmer(ACC ~ cStep + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=categ3)
xovers3 <- getCrossOver(coef(cat.mod3)$Subject)
xovers3 <- merge(xovers3, subj.tolerances3)

allcateg <- rbind(categ[,c('Subject','ACC','cStep','Item')],categ3[,c('Subject','ACC','cStep','Item')])

all.cat.mod <- glmer(ACC ~ cStep + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=allcateg)
all.xovers <- getCrossOver(coef(all.cat.mod)$Subject)
xovers <- merge(all.xovers, subj.tolerances)
ddply(subset(xovers,Xover > 0), ~Experiment*Attention*itemtype,nrow)
ddply(subset(xovers,Xover <= 0), ~Experiment*Attention*itemtype,nrow)


### EXPERIMENT 1

categ <- merge(categ, fillerresp)
categ.learning <- subset(categ, Subject %in% unique(subset(all.xovers,Xover > 0)$Subject))

experiment.1.mod <- glmer(ACC ~ cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=subset(categ,Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(experiment.1.mod)

experiment.1.mod.trimmed <- glmer(ACC ~ cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=subset(categ,Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=100000) ), subset = abs(scale(resid(experiment.1.mod))) < 2.5)
summary(experiment.1.mod.trimmed)


experiment.1.mod.wresp <- glmer(ACC ~ WordResp*cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=subset(categ,Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(experiment.1.mod.wresp)


#experiment.1.mod.modacc <- glmer(ModACC ~ Step*ExposureType*Attention + (1+Step|Subject) + (1+Step|Item), family='binomial',data=subset(categ,Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=100000) ))
#summary(experiment.1.mod.modacc)


### END EXPERIMENT 1

### EXPERIMENT 2

ddply(unique(categ[,c('Subject','Experiment','ExposureType','Attention')]), ~ Experiment*ExposureType*Attention, nrow)
ddply(categ, ~ ExposureType*Attention*Subject, nrow)


experiment.2.mod <- glmer(ACC ~ cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=subset(categ,Experiment=='exp1'), control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(experiment.2.mod)

experiment.2.mod.trimmed <- glmer(ACC ~ cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=subset(categ,Experiment=='exp1'), control=glmerControl(optCtrl=list(maxfun=100000) ), subset = abs(scale(resid(experiment.2.mod))) < 2.5)
summary(experiment.2.mod.trimmed)


experiment.2.mod.wresp <- glmer(ACC ~ WordResp*cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=subset(categ,Experiment=='exp1'), control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(experiment.2.mod.wresp)


### END EXPERIMENT 2

### GROUPED

grouped.mod <- glmer(ACC ~ cStep*ExposureType*Attention*Experiment + (1+cStep|Subject) + (1+cStep*Experiment|Item), family='binomial',data=subset(categ,Experiment%in%c('exp1','exp2')), control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(grouped.mod)

grouped.mod.trimmed <- glmer(ACC ~ cStep*ExposureType*Attention*Experiment + (1+cStep|Subject) + (1+cStep*Experiment|Item), family='binomial',data=subset(categ,Experiment%in%c('exp1','exp2')), control=glmerControl(optCtrl=list(maxfun=100000) ), subset = abs(scale(resid(grouped.mod))) < 2.5)
summary(grouped.mod.trimmed)


### END GROUPED

### EXPERIMENT 3

ddply(unique(categ3[,c('Subject','ExposureType','Attention')]), ~ ExposureType*Attention, nrow)
ddply(categ3, ~ ExposureType*Attention*Subject, nrow)

#categ3$ExposureType <- factor(categ3$ExposureType, levels = c('predictive','unpredictive'))

#categ3$Attention <- factor(categ3$Attention, levels = c('attend','noattend'))

categ3.learning <- subset(categ3,Subject %in% unique(subset(all.xovers,Xover > 0)$Subject))

experiment.3.mod <- glmer(ACC ~ cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=categ3, control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(experiment.3.mod)

experiment.3.mod.trimmed <- glmer(ACC ~ Step*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=categ3, control=glmerControl(optCtrl=list(maxfun=100000) ), subset = abs(scale(resid(experiment.3.mod))) < 2.5)
summary(experiment.3.mod.trimmed)

categ23.learning <- subset(categ23, Subject %in% unique(subset(all.xovers,Xover > 0)$Subject))

experiment.23.mod <- glmer(ACC ~ cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=categ23, control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(experiment.23.mod)

experiment.23.mod.trimmed <- glmer(ACC ~ cStep*ExposureType*Attention + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=categ23, control=glmerControl(optCtrl=list(maxfun=100000) ), subset = abs(scale(resid(experiment.23.mod))) < 2.5)
summary(experiment.23.mod.trimmed)

### END EXPERIMENT 3

cat.mod <- glmer(ACC ~ cStep + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=categ)
xovers <- getCrossOver(coef(cat.mod)$Subject)

cat.mod3 <- glmer(ACC ~ cStep + (1+cStep|Subject) + (1+cStep|Item), family='binomial',data=categ3)
xovers3 <- getCrossOver(coef(cat.mod3)$Subject)
xovers3 <- merge(xovers3, subj.tolerances3)
ddply(xovers3, ~Attention*Predictability, summarise, mean(Xover), sd(Xover), mean(MeanLogRt), sd(MeanLogRt))
cor.test(xovers3$Xover, xovers3$MeanLogRt)
summary(aov(Xover ~ MeanLogRt*Attention*Predictability,data=xovers3))
summary(aov(MeanLogRt~Attention*Predictability,data=xovers3))


xovers <- merge(xovers,subj.tolerances)
ddply(xovers, ~Experiment*Attention*itemtype, summarise, MeanXover = mean(Xover), SDXover = sd(Xover), MeanWordResp = mean(WordResp), SDWordResp = sd(WordResp), MeanMeanLogRT = mean(MeanLogRT), SDMeanLogRT = sd(MeanLogRT))
ddply(xovers, ~Experiment*Attention*itemtype, summarise, mean(Slope), sd(Slope))
summary(aov(Xover ~ WordResp*MeanLogRT*Attention*itemtype*Experiment,data=xovers))
summary(aov(Xover ~ WordResp*Attention*itemtype,data=subset(xovers,Experiment=='exp2')))
summary(aov(Slope ~ WordResp*Attention*itemtype,data=subset(xovers,Experiment=='exp2')))
summary(aov(Xover ~ WordResp*Attention*itemtype,data=subset(xovers,Experiment=='exp1')))
cor.test(subset(xovers,Experiment=='exp1')$Xover, subset(xovers,Experiment=='exp1')$WordResp)
cor.test(subset(xovers,Experiment=='exp2')$Xover, subset(xovers,Experiment=='exp2')$WordResp)
cor.test(subset(xovers,Experiment=='exp1')$Xover, subset(xovers,Experiment=='exp1')$aWordResp)
cor.test(subset(xovers,Experiment=='exp2')$Xover, subset(xovers,Experiment=='exp2')$aWordResp)

cor.test(subset(xovers,Experiment=='exp1')$Xover, subset(xovers,Experiment=='exp1')$MeanLogRT)
cor.test(subset(xovers,Experiment=='exp2')$Xover, subset(xovers,Experiment=='exp2')$MeanLogRT)

##

ddply(xovers3,~Attention*Predictability, summarise,tau.est=cor.test(Xover,MeanLogRt)$estimate, p.val = cor.test(Xover,MeanLogRt)$p.value)

ddply(xovers,~Experiment*Attention*itemtype, summarise,tau.est=cor.test(Xover,aWordResp)$estimate, p.val = cor.test(Xover,aWordResp)$p.value)
cor.test(subset(xovers,Experiment=='exp2')$Xover, subset(xovers,Experiment=='exp2')$aWordResp)

##

ggplot(categ,aes(x=Step,y=ACC)) + geom_smooth(method='glm',family='binomial')+facet_wrap(~Subject)

ggplot(cont,aes(x=Step,y=ACC)) + geom_smooth(method='loess')+facet_wrap(~Subject)

mean_sresp <- ddply(categ,~Experiment*ExposureType*Attention*Subject,summarise,meanresp = mean(ACC))

ggplot(mean_sresp,aes(x=meanresp)) + geom_histogram(binwidth=0.1) + facet_grid(ExposureType~Attention*Experiment) + geom_density()

cat.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=categ)
t <- getCrossOver(coef(cat.mod)$Subject)
summary(cat.mod)
cont.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=cont)
summary(cont.mod)

cont.xover <- getCrossOver(coef(cont.mod)$Subject)

t2 <- merge(t,subj.tolerances)

summary(aov(Xover ~ WordResp*Experiment,data=t2))
summary(aov(Xover ~ Attention*itemtype*Experiment,data=t2))
summary(aov(WordResp ~ Attention*itemtype*Experiment,data=t2))
summary(aov(Xover ~ Attention*itemtype,data=subset(t2,Experiment=='exp2')))
cor.test(t2$Xover, t2$WordResp)

plotData <- summarySEwithin(data = categ,measurevar = 'ACC',betweenvars = c('Attention','ExposureType','Experiment'),withinvars=c('Step'),idvar='Subject')
contPlotData <- summarySEwithin(data=cont, measurevar = 'ACC',withinvars=c('Step'),idvar='Subject')
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- rbind(contPlotData,contPlotData)
contPlotData <- cbind(contPlotData,data.frame(Attention=c(rep('attend',6),rep('noattend',6),rep('attend',6),rep('noattend',6)),ExposureType=c(rep('final',12),rep('initial',12))))
contPlotData$Experiment <- 'control'
plotData <- rbind(plotData,contPlotData)
ggplot(contPlotData,aes(x=Step,y=ACC)) + geom_point() + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci))
ggplot(plotData,aes(x=Step,y=ACC, colour=Experiment)) + geom_point()+facet_grid(ExposureType~Attention) + geom_errorbar(aes(ymin=ACC-ci,ymax=ACC+ci))+ ylab('Percent /s/ response') #+ geom_point(data=contPlotData,aes(x=Step,y=ACC),colour='black') + geom_errorbar(data=contPlotData,aes(ymin=ACC-ci,ymax=ACC+ci),colour='black') 

ddply(t2,~Attention*itemtype*Experiment,summarise,mean(Xover))
ggplot(categ, aes(x=Step, y=ACC)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~Subject) + labs(title='Categorization words', y='Proportion <S> responses',x='Step number')
ggplot(categ, aes(x=RT)) +geom_histogram() +geom_density() +facet_wrap(~Subject)

ggplot(t2,aes(x=WordResp,y=Xover,colour=Attention,shape=itemtype,linetype=itemtype)) + geom_point() + geom_smooth(method='lm')

cat.mod.full.1 <- glmer(ACC ~ Step*ExposureType*Attention + (1+Step|Subject) + (1+Step|Item), family='binomial',data=subset(categ,Experiment == 'exp1'), control=glmerControl(optCtrl=list(maxfun=30000)))
summary(cat.mod.full.1)
t <- subset(categ, Experiment=='exp2')
t$Step <- factor(t$Step,ordered=T)
cat.mod.full.2 <- glmer(ACC ~ Step*ExposureType*Attention + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(cat.mod.full.2)
cat.mod.full.2 <- glmer(ACC ~ Step*ExposureType*Attention+Item*Step + (1+Step|Subject), family='binomial',data=t, control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(cat.mod.full.2)


for.icphs <- subset(categ,Attention == 'noattend')
for.icphs$Attention <- NULL
for.icphs$Background <- 'Native'
cont$Experiment <- 'Control'
cont$ExposureType <- 'Control'
for.icphs <- rbind(for.icphs,cont)
for.icphs$Background <- factor(for.icphs$Background)
for.icphs$ExposureType <- factor(for.icphs$ExposureType,levels=c('Control','initial','final'))
for.icphs$Experiment <- factor(for.icphs$Experiment,levels=c('Control','exp1','exp2'))

icphs.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))
icphs.xover <- getCrossOver(coef(icphs.mod)$Subject)
icphs.mod <- glmer(ACC ~ Step*ExposureType*Experiment + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))

t <- unique(for.icphs[,c('Subject','Experiment','ExposureType')])
t2 <- merge(icphs.xover,t)
summary(aov(Xover ~ ExposureType*Experiment,data=t2))

test <- glmer(ACC ~ Step*ExposureType*Experiment + (1+Step|Subject) + (1+Step*Experiment|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))


for.icphs$ExposureType <- factor(for.icphs$ExposureType,levels=c('initial','Control','final'))

test.2 <- glmer(ACC ~ Step*ExposureType*Experiment + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))

for.icphs$Experiment <- factor(for.icphs$Experiment,levels=c('exp1','Control','exp2'))

test.3 <- glmer(ACC ~ Step*ExposureType*Experiment + (1+Step|Subject) + (1+Step|Item), family='binomial',data=for.icphs, control=glmerControl(optCtrl=list(maxfun=10000000) ))
