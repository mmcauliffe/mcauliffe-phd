
ddply(subj.tolerances,~Experiment,summarise, mean(WordResp), sd(WordResp))
ddply(subj.tolerances,~Experiment*Attention*itemtype,summarise, mean(WordResp), sd(WordResp))
summary(aov(WordResp ~ Experiment*Attention*itemtype,subj.tolerances))
summary(aov(WordResp ~ Attention*itemtype,subset(subj.tolerances,Experiment == 'exp1')))
summary(aov(WordResp ~ Attention*itemtype,subset(subj.tolerances,Experiment == 'exp2')))
### EXPERIMENT 1

experiment.1.expose.mod <- glmer(ACC ~ cTrial*itemtype2*Attention*ExposureType + (1+cTrial*itemtype2|Subject) + (1+Attention|Word), family='binomial',data=subset(expose.word,Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=2000000) ))
summary(experiment.1.expose.mod)

experiment.1.expose.mod.trimmed <- glmer(ACC ~ cTrial*itemtype2*Attention*ExposureType +(1|Subject) + (0+cTrial*itemtype2|Subject) + (1+Attention|Word), family='binomial',data=subset(expose.word,Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=2000000) ), subset = abs(scale(resid(experiment.1.expose.mod))) < 2.5)
summary(experiment.1.expose.mod.trimmed)

experiment.1.expose.mod.rt <- lmer(cLogRT ~ cTrial*itemtype2*Attention*ExposureType +(1+cTrial*itemtype2|Subject) + (1+Attention|Word),data=subset(expose.word,Experiment=='exp2'), control=lmerControl(optCtrl=list(maxfun=2000000) ))
summary(experiment.1.expose.mod.rt)

experiment.1.expose.mod.rt.trimmed <- lmer(cLogRT ~ cTrial*itemtype2*Attention*ExposureType + (1+cTrial*itemtype2|Subject) + (1+Attention|Word),data=subset(expose.word,Experiment=='exp2'), control=lmerControl(optCtrl=list(maxfun=200000) ), subset = abs(scale(resid(experiment.1.expose.mod.rt))) < 2.5)
summary(experiment.1.expose.mod.rt.trimmed)


### END EXPERIMENT 1

### EXPERIMENT 2

experiment.2.expose.mod <- glmer(ACC ~ cTrial*itemtype2*Attention*ExposureType +(1+cTrial+itemtype2|Subject) + (1+Attention|Word), family='binomial',data=subset(expose.word,Experiment=='exp1'), control=glmerControl(optCtrl=list(maxfun=200000) ))
summary(experiment.2.expose.mod)

experiment.2.expose.mod.trimmed <- glmer(ACC ~ cTrial*itemtype2*Attention*ExposureType + (1+cTrial*itemtype2|Subject) + (1+Attention|Word), family='binomial',data=subset(expose.word,Experiment=='exp1'), control=glmerControl(optCtrl=list(maxfun=200000) ), subset = abs(scale(resid(experiment.2.expose.mod))) < 2.5)
summary(experiment.2.expose.mod.trimmed)

experiment.2.expose.mod.rt <- lmer(cLogRT ~ cTrial*itemtype2*Attention*ExposureType +(1|Subject) + (0+cTrial*itemtype2|Subject) + (1+Attention|Word),data=subset(expose.word,Experiment=='exp1'), control=lmerControl(optCtrl=list(maxfun=2000000) ))
summary(experiment.2.expose.mod.rt)

experiment.2.expose.mod.rt.trimmed <- lmer(cLogRT ~ cTrial*itemtype2*Attention*ExposureType + (1+cTrial*itemtype2|Subject) + (1+Attention|Word),data=subset(expose.word,Experiment=='exp1'), control=lmerControl(optCtrl=list(maxfun=2000000) ), subset = abs(scale(resid(experiment.2.expose.mod.rt))) < 2.5)
summary(experiment.2.expose.mod.rt.trimmed)

ddply(subset(expose,Experiment=='exp1'), ~ Subject*itemtype2,nrow)

### END EXPERIMENT 2

experiment.12.expose.mod.rt <- lmer(cLogRT ~ cTrial*itemtype2*Attention*ExposureType*Experiment +(1|Subject) + (0+cTrial*itemtype2|Subject) + (1+Attention|Word),data=expose.word, control=lmerControl(optCtrl=list(maxfun=2000000) ))
summary(experiment.12.expose.mod.rt)

### EXPERIMENT 3

ddply(expose3, ~Type*Attention*Predictability, nrow)

ddply(expose3, ~Type*Attention*Predictability, summarise, mean(cLogRT), sd(cLogRT))

ddply(expose3, ~Predictability*Type*Attention, summarise, mean(LogRT), sd(LogRT))

ddply(expose3, ~Subject*Predictability, summarise, mean(LogRT), sd(LogRT))

mean(ddply(expose3, ~Subject, summarise, mean(ACC))$"..1")
sd(ddply(expose3, ~Subject, summarise, mean(ACC))$"..1")
mean(ddply(subset(expose3,Type=='S-final'), ~Subject, summarise, mean(ACC))$"..1")
sd(ddply(subset(expose3,Type=='S-final'), ~Subject, summarise, mean(ACC))$"..1")


experiment.3.expose.mod.rt <- lmer(cLogRT ~ cTrial*Type*Attention*ExposureType*Predictability + (1+cTrial*(Type+Predictability)|Subject) + (1+Attention+ExposureType|Word),data = expose3, control=lmerControl(optCtrl=list(maxfun=2000000) ))
summary(experiment.3.expose.mod.rt)

experiment.3.expose.mod.rt.trimmed <- lmer(cLogRT ~ Type*Attention*Predictability + (1+Type+Predictability|Subject) + (1+Attention*Predictability|Word),data = expose3, control=lmerControl(optCtrl=list(maxfun=200000) ), subset = abs(scale(resid(experiment.3.expose.mod.rt))) < 2.5)
summary(experiment.3.expose.mod.rt.trimmed)

### END EXPERIMENT 3


ggplot(target,aes(x=Trial,y=ACC)) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

ggplot(target,aes(x=Trial,y=log(RT))) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

plotData = summarySEwithin(expose.word,'ACC',c('Attention','Experiment'), c('itemtype'),idvar='Subject')

ggplot(plotData,aes(y=ACC, x = itemtype, fill=Attention)) + geom_bar(stat='identity', position=position_dodge(.9), colour = 'black') + geom_errorbar(position=position_dodge(.9), aes(ymin=ACC-ci,ymax = ACC + ci)) + facet_grid(~Experiment) + ylim(c(0,1))

plotData = summarySEwithin(expose.word,'RT',c('Attention','Experiment'), c('itemtype'),idvar='Subject')

ggplot(plotData,aes(y=RT, x = itemtype, fill=Attention)) + geom_bar(stat='identity', position=position_dodge(.9), colour = 'black') + geom_errorbar(position=position_dodge(.9), aes(ymin=RT-ci,ymax = RT + ci)) + facet_grid(~Experiment)

filler = na.omit(subset(expose,!itemtype %in% c('S-Initial','S-Final')))

fillerresp <- ddply(filler,~Subject,summarise,FillerWordResp = mean(ACC))
ddply(filler,~Subject*Lexicality,summarise,WordResp = mean(ACC))

ddply(expose.word,~Subject*itemtype,nrow)

expose.mod <- glmer(ACC ~ Trial+itemtype+Attention + (1+itemtype|Subject) + (1+ Attention|Word), family='binomial',data=subset(expose.word, Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=20000) ))
summary(expose.mod)

expose.mod.rt <- lmer(log(RT) ~ Trial+itemtype+ Attention + (1+itemtype|Subject) + (1+ Attention|Word),data=subset(expose,Lexicality=='Word'))
summary(expose.mod)

icphs.expose <- subset(expose.word,Attention == 'noattend')

icphs.expose.mod <- glmer(ACC ~ Trial+itemtype*Experiment + (1+Trial|Subject) + (1+ Experiment|Word), family='binomial',data=icphs.expose, control=glmerControl(optCtrl=list(maxfun=200000) ))


### EXPERIMENT 3

ddply(expose3,~Predictability*Type*Subject,nrow)

ddply(expose3,~Predictability*Type,summarise, MeanAccuracy = mean(ACC), MeanRT = mean(RT), SDRT = sd(RT))

expose3.mod <- lmer(LogRT ~ Predictability*Attention + Type + (1+Predictability+Type|Subject) + (1+ Predictability*Attention|Word),data=expose3)
summary(expose3.mod)

### END EXPERIMENT 3

ddply(subset(expose,Lexicality == 'Nonword'),~ExposureType*Attention,summarise, MeanAccuracy = mean(ACC), MeanRT = mean(RT), SDRT = sd(RT))
