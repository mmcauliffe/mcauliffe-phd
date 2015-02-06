### CONTROL ###

cont.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=cont)
summary(cont.mod)


### END CONTROL ###


### EXPERIMENT 1


experiment.1.mod <- glmer(ACC ~ Step*ExposureType*Attention + (1+Step|Subject) + (1+Step|Item), family='binomial',data=subset(categ,Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(experiment.1.mod)


### END EXPERIMENT 1

### EXPERIMENT 2


experiment.2.mod <- glmer(ACC ~ Step*ExposureType*Attention + (1+Step|Subject) + (1+Step|Item), family='binomial',data=subset(categ,Experiment=='exp1'), control=glmerControl(optCtrl=list(maxfun=100000) ))
summary(experiment.2.mod)


### END EXPERIMENT 2

###

cat.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=categ)
xovers <- getCrossOver(coef(cat.mod)$Subject)

xovers <- merge(xovers,subj.tolerances)
summary(aov(Xover ~ WordResp*Attention*itemtype*Experiment,data=xovers))
cor.test(subset(xovers,Experiment=='exp1')$Xover, subset(xovers,Experiment=='exp1')$WordResp)
cor.test(subset(xovers,Experiment=='exp2')$Xover, subset(xovers,Experiment=='exp2')$WordResp)
cor.test(subset(xovers,Experiment=='exp1')$Xover, subset(xovers,Experiment=='exp1')$tWordResp)
cor.test(subset(xovers,Experiment=='exp2')$Xover, subset(xovers,Experiment=='exp2')$tWordResp)


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
