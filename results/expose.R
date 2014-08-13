data <- read.delim('exposure.txt')

target <- na.omit(subset(data,itemtype %in% c('S-Initial','S-Final')))

subj.tolerances <- ddply(target,~Subject*itemtype,summarise,WordResp = mean(ACC))

summary(aov(WordResp ~ itemtype,data=subj.tolerances))

ggplot(target,aes(x=Trial,y=ACC)) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)
