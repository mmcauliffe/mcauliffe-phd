

ggplot(target,aes(x=Trial,y=ACC)) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

ggplot(target,aes(x=Trial,y=log(RT))) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

filler = na.omit(subset(expose,!itemtype %in% c('S-Initial','S-Final')))

ddply(filler,~Subject,summarise,WordResp = mean(ACC))

ddply(expose.word,~Subject*itemtype,nrow)

expose.mod <- glmer(ACC ~ Trial+itemtype+Attention + (1+itemtype|Subject) + (1+ Attention|Word), family='binomial',data=expose.word)
summary(expose.mod)

expose.mod.rt <- lmer(log(RT) ~ Trial+itemtype+ Attention + (1+itemtype|Subject) + (1+ Attention|Word),data=subset(expose,Lexicality=='Word'))
summary(expose.mod)
