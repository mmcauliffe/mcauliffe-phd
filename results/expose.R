
expose <- read.delim('exposure.txt')


expose$Attention <- 'attend'

expose[str_detect(expose$Subject,'^s2'),]$Attention <- 'noattend'
expose[str_detect(expose$Subject,'^s3'),]$Attention <- 'noattend'

expose$Attention <- factor(expose$Attention)

expose.word <- subset(expose,Lexicality=='Word')
expose.word$Word <- factor(expose.word$Word)

target <- na.omit(subset(expose,itemtype %in% c('S-Initial','S-Final')))

subj.tolerances <- ddply(target,~Subject*itemtype*Attention,summarise,WordResp = mean(ACC))

summary(aov(WordResp ~ itemtype*Attention,data=subj.tolerances))

ggplot(target,aes(x=Trial,y=ACC)) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

ggplot(target,aes(x=Trial,y=log(RT))) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

filler = na.omit(subset(expose,!itemtype %in% c('S-Initial','S-Final')))

ddply(filler,~Subject,summarise,WordResp = mean(ACC))

expose.mod <- glmer(ACC ~ Trial+itemtype+ Attention + (1+itemtype|Subject) + (1+ Attention|Word), family='binomial',data=expose)
summary(expose.mod)

expose.mod.rt <- lmer(log(RT) ~ Trial+itemtype+ Attention + (1+itemtype|Subject) + (1+ Attention|Word),data=subset(expose,Lexicality=='Word'))
summary(expose.mod)
