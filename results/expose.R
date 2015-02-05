

### EXPERIMENT 1

experiment.1.expose.mod <- glmer(ACC ~ Trial+itemtype2*Attention*ExposureType + (1+Trial+itemtype2|Subject) + (1+Attention|Word), family='binomial',data=subset(expose,Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=200000) ))
summary(experiment.1.expose.mod)


### END EXPERIMENT 1

### EXPERIMENT 2

experiment.2.expose.mod <- glmer(ACC ~ Trial+itemtype2*Attention*ExposureType + (1+Trial+itemtype2|Subject) + (1+Attention|Word), family='binomial',data=subset(expose,Experiment=='exp1'), control=glmerControl(optCtrl=list(maxfun=200000) ))

### END EXPERIMENT 2


ggplot(target,aes(x=Trial,y=ACC)) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

ggplot(target,aes(x=Trial,y=log(RT))) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

plotData = summarySEwithin(expose.word,'ACC',c('Attention','Experiment'), c('itemtype'),idvar='Subject')

ggplot(plotData,aes(y=ACC, x = itemtype, fill=Attention)) + geom_bar(stat='identity', position=position_dodge(.9), colour = 'black') + geom_errorbar(position=position_dodge(.9), aes(ymin=ACC-ci,ymax = ACC + ci)) + facet_grid(~Experiment) + ylim(c(0,1))

plotData = summarySEwithin(expose.word,'RT',c('Attention','Experiment'), c('itemtype'),idvar='Subject')

ggplot(plotData,aes(y=RT, x = itemtype, fill=Attention)) + geom_bar(stat='identity', position=position_dodge(.9), colour = 'black') + geom_errorbar(position=position_dodge(.9), aes(ymin=RT-ci,ymax = RT + ci)) + facet_grid(~Experiment)

filler = na.omit(subset(expose,!itemtype %in% c('S-Initial','S-Final')))

summary(ddply(filler,~Subject,summarise,WordResp = mean(ACC)))
ddply(filler,~Subject,summarise,WordResp = mean(ACC))

ddply(expose.word,~Subject*itemtype,nrow)

expose.mod <- glmer(ACC ~ Trial+itemtype+Attention + (1+itemtype|Subject) + (1+ Attention|Word), family='binomial',data=subset(expose.word, Experiment=='exp2'), control=glmerControl(optCtrl=list(maxfun=20000) ))
summary(expose.mod)

expose.mod.rt <- lmer(log(RT) ~ Trial+itemtype+ Attention + (1+itemtype|Subject) + (1+ Attention|Word),data=subset(expose,Lexicality=='Word'))
summary(expose.mod)

icphs.expose <- subset(expose.word,Attention == 'noattend')

icphs.expose.mod <- glmer(ACC ~ Trial+itemtype*Experiment + (1+Trial|Subject) + (1+ Experiment|Word), family='binomial',data=icphs.expose, control=glmerControl(optCtrl=list(maxfun=200000) ))

