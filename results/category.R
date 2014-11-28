

mean_sresp <- ddply(categ,~ExposureType*Attention*Experiment*Subject,summarise,meanresp = mean(ACC))

ggplot(mean_sresp,aes(x=meanresp)) + geom_histogram(binwidth=0.1) + facet_grid(ExposureType~Attention*Experiment) + geom_density()

cat.mod <- glmer(ACC ~ Step + (1+Step|Subject) + (1+Step|Item), family='binomial',data=categ)
summary(cat.mod)

getCrossOver <- function(data){
  data$p <- -1*data[,'(Intercept)']/data[,'Step']
  data$pRound <- round(data$p)
  
  data <- data.frame(Subject = row.names(data),Xover = data$p)
  return(data)
}
t <- getCrossOver(coef(cat.mod)$Subject)

t2 <- merge(t,subj.tolerances)

summary(aov(Xover ~ WordResp,data=t2))
summary(aov(Xover ~ Attention*itemtype*Experiment,data=t2))
cor.test(t2$Xover, t2$WordResp)

ddply(t2,~Attention*itemtype*Experiment,summarise,mean(Xover))
ggplot(categ, aes(x=Step, y=ACC)) +geom_point() +geom_smooth(method="glm", family="binomial", size=2) +facet_wrap(~Subject) + labs(title='Categorization words', y='Proportion <S> responses',x='Step number')
ggplot(categ, aes(x=RT)) +geom_histogram() +geom_density() +facet_wrap(~Subject)

ggplot(t2,aes(x=WordResp,y=Xover,colour=Attention,shape=itemtype,linetype=itemtype)) + geom_point() + geom_smooth(method='lm')

cat.mod.full <- glmer(ACC ~ Trial + Step+ExposureType*Attention + (1+Trial+Step|Subject) + (1+Step+ExposureType+Attention|Item), family='binomial',data=categ)
summary(cat.mod.full)
