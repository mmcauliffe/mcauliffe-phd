data <- read.delim('exposure.txt')

data$Attention <- 'attend'

data[str_detect(data$Subject,'^s2'),]$Attention <- 'noattend'
data[str_detect(data$Subject,'^s3'),]$Attention <- 'noattend'

data$Attention <- factor(data$Attention)

target <- na.omit(subset(data,itemtype %in% c('S-Initial','S-Final')))

subj.tolerances <- ddply(target,~Subject*itemtype*Attention,summarise,WordResp = mean(ACC))

summary(aov(WordResp ~ itemtype*Attention,data=subj.tolerances))

ggplot(target,aes(x=Trial,y=ACC)) + geom_point() + geom_smooth(method='lm')+facet_wrap(~Subject)

filler = na.omit(subset(data,!itemtype %in% c('S-Initial','S-Final')))

ddply(filler,~Subject,summarise,WordResp = mean(ACC))
